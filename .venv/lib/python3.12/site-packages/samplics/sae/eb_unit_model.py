"""EB  for the unit level model.

This module implements the basic EB unit level model. The functionalities are organized in
classes. Each class has three main methods: *fit()*, *predict()* and *bootstrap_mse()*.
Linear Mixed Models (LMM) are the core underlying statistical framework used to model the hierarchical nature of the small area estimation (SAE) techniques implemented in this module, see McCulloch, C.E. and Searle, S.R. (2001) [#ms2001]_ for more details on LMM.

The *EbUnitModel* class implements the model developed by Molina, I. and Rao, J.N.K. (2010)
[#mr2010]_. So far, only the basic approach requiring the normal distribution of the errors is
implemented. This approach allows estimating complex indicators such as poverty indices and
other nonlinear paramaters. The class fits the model parameters using REML or ML. To predict the
area level indicators estimates, a Monte Carlo (MC) approach is used. MSE estimation is achieved
using a bootstrap procedure.

For a comprehensive review of the small area estimation models and its applications,
see Rao, J.N.K. and Molina, I. (2015) [#rm2015]_.

.. [#ms2001] McCulloch, C.E.and Searle, S.R. (2001), *Generalized, Linear, Mixed Models*,
   New York: John Wiley & Sons, Inc.
.. [#mr2010] Molina, , I. and Rao, J.N.K. (2010), Small Area Estimation of Poverty Indicators,
   *Canadian Journal of Statistics*, **38**, 369-385.
.. [#rm2015] Rao, J.N.K. and Molina, I. (2015), *Small area estimation, 2nd edn.*,
   John Wiley & Sons, Hoboken, New Jersey.
"""

from __future__ import annotations

import warnings

from typing import Any, Callable, Optional, Union

import numpy as np
import pandas as pd
import statsmodels.api as sm

from samplics.sae.eblup_unit_model import EblupUnitModel
from samplics.utils import basic_functions, formats
from samplics.utils.types import Array, DictStrNum, Number


class EbUnitModel:
    """*EbUnitModel* implements the basic Unit level model for complex indicators.

    *EbUnitModel* takes the sample data as input and fits the basic linear mixed model.
    The user can pick between restricted maximum likelihood (REML) or maximum likelihood (ML)
    to fit the model parameters. Also, EbUnitModel predicts the areas means and provides
    the point and mean squared error (MSE) estimates of the empirical Bayes linear
    unbiased (EBLUP). User can also obtain the bootstrap mse estimates of the MSE.

    *EbUnitModel* requires the user to provide the indicator function. The indicator function is
    expected to take the array of output sample observations as input and possibly some additional
    parameters needed to compute the indicator. The indicator function outputs an aggregated value.
    For example, the poverty gap indicator can have the following signature
    pov_gap(y: array, pov_line: float) -> float. If the indicator function different outputs by
    area then the self.area_list can be used to incorporate different logics across areas.

    Also, *EbUnitModel* can use Boxcox to transform the output sample values in order to reduce
    the asymmetry in the datawhen fitting the linear mixed model.

    Setting attributes
        | method (str): the fitting method of the model parameters which can take the possible
        |   values restricted maximum likelihood (REML) or maximum likelihood (ML).
        |   If not specified, "REML" is used as default.
        | indicator (function): a user defined function to compute the indicator.
        | boxcox (dict): contains the *lambda* parameter of the Boxcox and a constant for the
        | log-transformation of the Boxcox.

    Sample related attributes
        | ys (array): the output sample observations.
        | Xs (ndarray): the auxiliary information.
        | scales (array): an array of scaling parameters for the unit levels errors.
        | afactors (array): sum of the inverse squared of scale.
        | areas (array): the full vector of small areas from the sampled observations.
        | areas_list (array): the list of small areas from the sample data.
        | samp_size (dict): the sample size per small areas from the sample.
        | ys_mean (array): sample area means of the output variable.
        | Xs_mean (ndarray): sample area means of the auxiliary variables.

    Model fitting attributes
        | fitted (boolean): indicates whether the model has been fitted or not.
        | fixed_effects (array): the estimated fixed effects of the regression model.
        | fe_std (array): the estimated standard errors of the fixed effects.
        | random_effects (array): the estimated area level random effects.
        |   associated with the small areas.
        | re_std (number): the estimated standard error of the random effects.
        | error_std (number): standard error of the unit level residuals.
        | convergence (dict): a dictionnary holding the convergence status and the number of
        |   iterations from the model fitting algorithm.
        | goodness (dict): a dictionary holding the log-likelihood, AIC, and BIC.
        | gamma (dict): ratio of the between-area variability (re_std**2) to the total
        |   variability (re_std**2 + error_std**2 / a_factor).

    Prediction related attributes
        | areap (array): the list of areas for the prediction.
        | number_reps (int): number of replicates for the bootstrap MSE estimation.
        | area_est (array): area level EBLUP estimates.
        | area_mse (array): area level taylor estimation of the MSE.
        | area_mse_boot (array): area level bootstrap estimation of the MSE.

    Main methods
        | fit(): fits the linear mixed model to estimate the model parameters using REMl or ML
        |   methods.
        | predict(): predicts the area level indicator estimates which includes both the point
        |   estimates and the taylor MSE estimate.
        | bootstrap_mse(): computes the area level bootstrap MSE estimates of the indicator.
    """

    def __init__(
        self,
        method: str = "REML",
        boxcox: Optional[Number] = None,
        constant: Optional[Number] = None,
    ):
        # Setting
        self.method: str = method.upper()
        if self.method not in ("REML", "ML"):
            raise AssertionError("Value provided for method is not valid!")
        self.indicator: Callable[..., Any]
        self.number_samples: int
        self.boxcox: dict[str, Optional[Number]] = {
            "lambda": boxcox,
            "constant": constant,
        }

        # Sample data
        self.scales: np.ndarray
        self.afactors: DictStrNum
        self.ys: np.ndarray
        self.Xs: np.ndarray
        self.areas: np.ndarray
        self.areas_list: np.ndarray
        self.samp_size: DictStrNum
        self.ys_mean: np.ndarray
        self.Xs_mean: np.ndarray

        # Fitted data
        self.fitted: bool = False
        self.fixed_effects: np.ndarray
        self.fe_std: np.ndarray
        self.random_effects: np.ndarray
        self.re_std: float
        self.error_std: float
        self.convergence: dict[str, Union[float, int, bool]] = {}
        self.goodness: dict[str, Number] = {}  # loglikehood, deviance, AIC, BIC
        self.gamma: DictStrNum

        # Predict(ion/ed) data
        self.number_reps: int
        self.area_est: DictStrNum
        self.area_mse: DictStrNum
        self.area_mse_boot: Optional[DictStrNum] = None

    def _transformation(self, y: np.ndarray, inverse: bool) -> np.ndarray:
        if self.boxcox["lambda"] is None:
            return y
        elif self.boxcox["lambda"] == 0.0 and self.boxcox["constant"] is not None:
            if inverse:
                return np.asarray(np.exp(y) - self.boxcox["constant"])
            else:
                return np.asarray(np.log(y + self.boxcox["constant"]))
        elif self.boxcox["lambda"] != 0.0:
            if inverse:
                return np.asarray(
                    np.exp(np.log(1 + y * self.boxcox["lambda"]) / self.boxcox["lambda"])
                )
            else:
                return np.asarray(np.power(y, self.boxcox["lambda"]) / self.boxcox["lambda"])
        else:
            raise AssertionError

    def fit(
        self,
        ys: Array,
        Xs: Array,
        areas: Array,
        samp_weight: Optional[Array] = None,
        scales: Union[Array, Number] = 1,
        intercept: bool = True,
        tol: float = 1e-8,
        maxiter: int = 100,
    ) -> None:
        """Fits the linear mixed models to estimate the model parameters that is the fixed
        effects, the random effects standard error and the unit level residuals' standard error.
        In addition, the method provides statistics related to the model fitting e.g. convergence
        status, log-likelihood, AIC, BIC, and more.

        Args:
            ys (Array): An array of the output sample observations.
            Xs (Array): An multi-dimensional array of the sample auxiliary information.
            areas (Array): provides the area of the sampled observations.
            samp_weight (Optional[Array], optional): An array of the sample weights.
                Defaults to None.
            scales (Union[Array, Number], optional): the scale factor for the unit level errors.
                If a single number of provided, the same number will be applied to all observations. Defaults to 1.
            intercept (bool, optional): An boolean to indicate whether an intercept need to be
                added to Xs. Defaults to True
            tol (float, optional): tolerance used for convergence criteria. Defaults to 1.0e-4.
            maxiter (int, optional): maximum number of iterations for the fitting algorithm.
            Defaults to 100.
        """

        ys = formats.numpy_array(ys)

        ys_transformed = basic_functions.transform(
            ys,
            llambda=self.boxcox["lambda"],
            constant=self.boxcox["constant"],
            inverse=False,
        )

        eblup_ul = EblupUnitModel(
            method=self.method,
        )
        eblup_ul.fit(
            ys_transformed,
            Xs,
            areas,
            samp_weight,
            scales,
            intercept,
            tol=tol,
            maxiter=maxiter,
        )

        self.scales = eblup_ul.scales
        self.ys = eblup_ul.ys
        self.Xs = eblup_ul.Xs
        self.areas = eblup_ul.areas
        self.areas_list = eblup_ul.areas_list
        self.afactors = eblup_ul.afactors
        self.error_std = eblup_ul.error_std
        self.fixed_effects = eblup_ul.fixed_effects
        self.fe_std = eblup_ul.fe_std
        self.re_std = eblup_ul.re_std
        self.convergence = eblup_ul.convergence
        self.goodness = eblup_ul.goodness
        self.ys_mean = eblup_ul.ys_mean
        self.Xs_mean = eblup_ul.Xs_mean
        self.gamma = eblup_ul.gamma
        self.samp_size = eblup_ul.samp_size
        self.fitted = eblup_ul.fitted

    def _predict_indicator(
        self,
        number_samples: int,
        y_s: np.ndarray,
        X_s: np.ndarray,
        area_s: np.ndarray,
        X_r: np.ndarray,
        area_r: np.ndarray,
        arear_list: np.ndarray,
        fixed_effects: np.ndarray,
        gamma: np.ndarray,
        sigma2e: float,
        sigma2u: float,
        scale: np.ndarray,
        intercept: bool,
        max_array_length: int,
        indicator: Callable[..., Any],
        show_progress: bool,
        **kwargs: Any,
    ) -> np.ndarray:
        if intercept:
            if self.Xs_mean.ndim == 1:
                n = self.Xs_mean.shape[0]
                Xs_mean = np.insert(self.Xs_mean.reshape(n, 1), 0, 1, axis=1)
            else:
                Xs_mean = np.insert(self.Xs_mean, 0, 1, axis=1)
        else:
            Xs_mean = self.Xs_mean

        nb_arear = len(arear_list)
        mu_r = X_r @ fixed_effects

        if show_progress:
            bar_length = min(50, nb_arear)
            steps = np.linspace(1, nb_arear - 1, bar_length).astype(int)
            print(f"Generating the {number_samples} replicates samples")

        k = 0
        eta = np.zeros((number_samples, nb_arear)) * np.nan
        for i, d in enumerate(arear_list):
            # print(d)
            oos = area_r == d
            mu_dr = mu_r[oos]
            ss = self.areas_list == d
            ybar_d = self.ys_mean[ss]
            xbar_d = Xs_mean[ss]
            mu_bias_dr = self.gamma[d] * (ybar_d - xbar_d @ fixed_effects)
            scale_dr = scale[oos]
            N_dr = np.sum(oos)
            cycle_size = max(int(max_array_length // N_dr), 1)
            number_cycles = int(number_samples // cycle_size)
            last_cycle_size = number_samples % cycle_size

            y_dr = None
            for j in range(number_cycles + 1):
                if j == number_cycles:
                    cycle_size = last_cycle_size
                re_effects = np.random.normal(
                    scale=(sigma2u * (1 - self.gamma[d])) ** 0.5,
                    size=cycle_size,
                )
                errors = np.random.normal(
                    scale=scale_dr * (sigma2e**0.5), size=(cycle_size, N_dr)
                )
                y_dr_j = mu_dr[None, :] + mu_bias_dr + re_effects[:, None] + errors
                if j == 0:
                    y_dr = y_dr_j
                else:
                    y_dr = np.append(y_dr, y_dr_j, axis=0)

            if show_progress:
                if i in steps:
                    k += 1
                    print(
                        f"\r[%-{bar_length}s] %d%%"
                        % ("=" * (k + 1), (k + 1) * (100 / bar_length)),
                        end="",
                    )

            y_d = np.append(y_dr, np.tile(y_s[area_s == d], [number_samples, 1]), axis=1)
            z_d = basic_functions.transform(
                y_d,
                llambda=self.boxcox["lambda"],
                constant=self.boxcox["constant"],
                inverse=True,
            )
            eta[:, i] = np.apply_along_axis(indicator, axis=1, arr=z_d, **kwargs)  # *)

        if show_progress:
            print("\n")

        return np.asarray(np.mean(eta, axis=0))

    def predict(
        self,
        Xr: Array,
        arear: Array,
        indicator: Callable[..., Array],
        number_samples: int,
        scaler: Union[Array, Number] = 1,
        intercept: bool = True,
        max_array_length: int = int(100e6),
        show_progress: bool = True,
        **kwargs: Any,
    ) -> None:
        """Predicts the area level means and provides the taylor MSE estimation of the estimated
        area means.

        Args:
            Xr (Array): an multi-dimensional array of the out of sample auxiliary variables.
            arear (Array): provides the area of the out of sample units.
            indicator (Callable[..., Array]): a user defined function which computes the area level
                indicators. The function should take y (output variable) as the first parameters,
                additional parameters can be used. Use ***kwargs* to transfer the additional
                parameters.
            number_samples (int): number of replicates for the Monte-Carlo (MC) algorithm.
            scaler (Union[Array, Number], optional): the scale factor for the unit level errors.
                If a single number of provided, the same number will be applied to all observations. Defaults to 1.
            intercept (bool, optional): An boolean to indicate whether an intercept need to be
                added to Xr. Defaults to True.
            max_array_length (int, optional): controls the number of replicates to generate at
                the same time. This parameter helps with performance. The number can be reduce or
                increase based on the user's computer RAM capacity. Defaults to int(100e6).
            show_progress (bool, optional): shows a bar progress of the MC replicates
                calculations. Defaults to True.

        Raises:
            Exception: when predict() is called before fitting the model.
        """

        if not self.fitted:
            raise Exception(
                "The model must be fitted first with .fit() before running the prediction."
            )

        self.number_samples = int(number_samples)

        Xr = formats.numpy_array(Xr)
        arear = formats.numpy_array(arear)
        self.arear_list = np.unique(arear)
        if isinstance(scaler, (float, int)):
            scaler = np.asarray(np.ones(Xr.shape[0]) * scaler)
        else:
            scaler = formats.numpy_array(scaler)
        if intercept:
            if Xr.ndim == 1:
                n = Xr.shape[0]
                Xr = np.insert(Xr.reshape(n, 1), 0, 1, axis=1)
                Xs = np.insert(self.Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                Xr = np.insert(Xr, 0, 1, axis=1)
                Xs = np.insert(self.Xs, 0, 1, axis=1)
        else:
            Xs = self.Xs

        area_est = self._predict_indicator(
            self.number_samples,
            self.ys,
            Xs,
            self.areas,
            Xr,
            arear,
            self.arear_list,
            self.fixed_effects,
            np.asarray(list(self.gamma.values())),
            self.error_std**2,
            self.re_std**2,
            scaler,
            intercept,
            max_array_length,
            indicator,
            show_progress,
            **kwargs,
        )

        self.area_est = dict(zip(self.arear_list, area_est))

    def bootstrap_mse(
        self,
        Xr: Array,
        arear: Array,
        indicator: Callable[..., Array],
        number_reps: int,
        scaler: Union[Array, Number] = 1,
        intercept: bool = True,
        tol: float = 1e-6,
        maxiter: int = 100,
        max_array_length: int = int(100e6),
        show_progress: bool = True,
        **kwargs: Any,
    ) -> None:
        """Computes the MSE bootstrap estimates of the area level indicator estimates.

        Args:
            Xr (Array): an multi-dimensional array of the out of sample auxiliary variables.
            arear (Array): provides the area of the out of sample units.
            indicator (Callable[..., Array]): [description]
            number_reps (int): [description]
            scaler (Union[Array, Number], optional): [description]. Defaults to 1.
            intercept (bool, optional): [description]. Defaults to True.
            tol (float, optional): tolerance used for convergence criteria. Defaults to 1.0e-4.
            maxiter (int, optional): maximum number of iterations for the fitting algorithm.
            Defaults to 100.
            max_array_length (int, optional): [description]. Defaults to int(100e6).
            show_progress (bool, optional): shows a bar progress of the bootstrap replicates
                calculations. Defaults to True.

        """

        X_r = formats.numpy_array(Xr)
        area_r = formats.numpy_array(arear)
        arear_list = np.unique(area_r)

        if intercept:
            if X_r.ndim == 1:
                n = X_r.shape[0]
                X_r = np.insert(X_r.reshape(n, 1), 0, 1, axis=1)
                Xs = np.insert(self.Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                X_r = np.insert(X_r, 0, 1, axis=1)
                Xs = np.insert(self.Xs, 0, 1, axis=1)
        else:
            Xs = self.Xs

        if isinstance(scaler, (float, int)):
            scale_r = np.ones(X_r.shape[0]) * scaler
        else:
            scale_r = formats.numpy_array(scaler)

        ps = np.isin(area_r, self.areas_list)
        areas_ps = np.unique(area_r[ps])
        nb_areas_ps = areas_ps.size
        area_s = self.areas[np.isin(self.areas, arear_list)]
        area = np.append(area_r, area_s)
        scale_s = self.scales[np.isin(self.areas, arear_list)]
        scale = np.append(scale_r, scale_s)
        _, N_d = np.unique(area, return_counts=True)
        X_s = Xs[np.isin(self.areas, arear_list)]
        X = np.append(X_r, X_s, axis=0)

        aboot_factor = np.zeros(nb_areas_ps)

        indice_dict = {}
        area_dict = {}
        scale_dict = {}
        scale_s_dict = {}
        a_factor_dict = {}
        sample_size_dict = {}
        X_dict = {}
        X_s_dict = {}
        for i, d in enumerate(arear_list):
            area_ds = area_s == d
            indice_dict[d] = area == d
            area_dict[d] = area[indice_dict[d]]
            scale_dict[d] = scale[indice_dict[d]]
            a_factor_dict[d] = self.afactors[d] if d in self.areas_list else 0
            sample_size_dict[d] = self.samp_size[d] if d in self.areas_list else 0
            scale_s_dict[d] = scale_s[area_ds] if d in self.areas_list else 0
            X_dict[d] = X[indice_dict[d]]
            X_s_dict[d] = X_s[area_ds]

        cycle_size = max(int(max_array_length // sum(N_d)), 1)
        number_cycles = int(number_reps // cycle_size)
        last_cycle_size = number_reps % cycle_size
        number_cycles = number_cycles + 1 if last_cycle_size > 0 else number_cycles

        k = 0
        bar_length = min(50, number_cycles * nb_areas_ps)
        steps = np.linspace(1, number_cycles * nb_areas_ps - 1, bar_length).astype(int)

        eta_pop_boot = np.zeros((number_reps, nb_areas_ps))
        eta_samp_boot = np.zeros((number_reps, nb_areas_ps))
        y_samp_boot = np.zeros((number_reps, int(np.sum(list(sample_size_dict.values())))))
        print(f"Generating the {number_reps} bootstrap replicate populations")
        for b in range(number_cycles):
            start = b * cycle_size
            end = (b + 1) * cycle_size
            if b == number_cycles - 1:
                end = number_reps
                cycle_size = last_cycle_size

            yboot_s = None
            for i, d in enumerate(areas_ps):
                aboot_factor[i] = a_factor_dict[d]

                re_d = np.random.normal(
                    scale=self.re_std * (1 - self.gamma[d]) ** 0.5,
                    size=cycle_size,
                )
                err_d = np.random.normal(
                    scale=self.error_std * scale_dict[d],
                    size=(cycle_size, np.sum(indice_dict[d])),
                )
                yboot_d = (X_dict[d] @ self.fixed_effects)[None, :] + re_d[:, None] + err_d
                zboot_d = basic_functions.transform(
                    yboot_d,
                    llambda=self.boxcox["lambda"],
                    constant=self.boxcox["constant"],
                    inverse=True,
                )
                eta_pop_boot[start:end, i] = indicator(zboot_d, **kwargs)

                if i == 0:
                    yboot_s = yboot_d[:, -int(sample_size_dict[d]) :]
                else:
                    yboot_s = np.append(yboot_s, yboot_d[:, -int(sample_size_dict[d]) :], axis=1)

                if show_progress:
                    run_id = b * nb_areas_ps + i
                    if run_id in steps:
                        k += 1
                        print(
                            f"\r[%-{bar_length}s] %d%%"
                            % ("=" * (k + 1), (k + 1) * (100 / bar_length)),
                            end="",
                        )

            y_samp_boot[start:end, :] = yboot_s

        if show_progress:
            print("\n")

        k = 0
        bar_length = min(50, number_reps)
        steps = np.linspace(1, number_reps, bar_length).astype(int)

        reml = True if self.method == "REML" else False
        # with warnings.catch_warnings():
        #     warnings.filterwarnings("ignore")
        #     beta_ols = sm.OLS(y_samp_boot[0, :], X_s).fit().params
        # resid_ols = y_samp_boot[0, :] - np.matmul(X_s, beta_ols)
        # re_ols = basic_functions.sumby(area_s, resid_ols) / basic_functions.sumby(
        #     area_s, np.ones(area_s.size)
        # )
        fit_kwargs = {
            "tol": tol,
            "gtol": tol,
            # "pgtol": tol,
            "maxiter": maxiter,
        }  # TODO: to improve in the future. Check: statsmodels.LikelihoodModel.fit()
        print(f"Fitting and predicting using each of the {number_reps} bootstrap populations")
        for b in range(number_reps):
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore")
                boot_model = sm.MixedLM(y_samp_boot[b, :], X_s, area_s)
                boot_fit = boot_model.fit(
                    reml=reml,
                    # start_params=np.append(beta_ols, np.std(re_ols) ** 2),
                    full_output=True,
                    **fit_kwargs,
                )

            gammaboot = boot_fit.cov_re[0] / (
                boot_fit.cov_re[0] + boot_fit.scale * (1 / aboot_factor)
            )

            eta_samp_boot[b, :] = self._predict_indicator(
                self.number_samples,
                y_samp_boot[b, :],
                X_s,
                area_s,
                X_r,
                area_r,
                np.unique(area_r),
                boot_fit.fe_params,
                gammaboot,
                boot_fit.scale,
                boot_fit.cov_re[0],
                scale_r,
                intercept,
                max_array_length,
                indicator,
                False,
                **kwargs,
            )

            if show_progress:
                if b in steps:
                    k += 1
                    print(
                        f"\r[%-{bar_length}s] %d%%"
                        % ("=" * (k + 1), (k + 1) * (100 / bar_length)),
                        end="",
                    )

        print("\n")

        mse_boot = np.asarray(np.mean(np.power(eta_samp_boot - eta_pop_boot, 2), axis=0))
        self.area_mse_boot = dict(zip(self.arear_list, mse_boot))

    def to_dataframe(
        self,
        col_names: list[str] = ["_area", "_estimate", "_mse_boot"],
    ) -> pd.DataFrame:
        """Returns a pandas dataframe from dictionaries with same keys and one value per key.

        Args:
            col_names (list, optional): list of string to be used for the dataframe columns names.
                Defaults to ["_area", "_estimate", "_mse_boot"].

        Returns:
            [pd.DataFrame]: a pandas dataframe
        """

        ncols = len(col_names)

        if self.area_est is None:
            raise AssertionError("No prediction yet. Must predict the area level estimates.")
        elif self.area_mse_boot is None and ncols not in (2, 3):
            raise AssertionError("col_names must have 2 or 3 values")
        elif self.area_mse_boot is None and ncols == 3:
            col_names.pop()  # remove the last element same as .pop(-1)

        if self.area_mse_boot is None:
            area_df = formats.dict_to_dataframe(col_names, self.area_est)
        else:
            area_df = formats.dict_to_dataframe(col_names, self.area_est, self.area_mse_boot)

        return area_df
