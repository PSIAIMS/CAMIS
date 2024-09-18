"""EBLUP for the unit level model.

This module implements the basic EBLUP unit level model. The functionalities are organized in
classes. Each class has three main methods: *fit()*, *predict()* and *bootstrap_mse()*.
Linear Mixed Models (LMM) are the core underlying statistical framework used to model the
hierarchical nature of the small area estimation (SAE) techniques implemented in this module,
see McCulloch, C.E. and Searle, S.R. (2001) [#ms2001]_ for more details on LMM.

The *EblupUnitModel* class implements the model developed by Battese, G.E., Harter, R.M., and
Fuller, W.A. (1988) [#bhf1988]_. The model parameters can fitted using restricted maximum
likelihood (REML) and maximum likelihood (ML). The normality assumption of the errors is not
necessary to predict the point estimates but is required for the taylor MSE estimation. The
predictions takes into account sampling rates. A bootstrap MSE estimation method is also implemted
for this class.

For a comprehensive review of the small area estimation models and its applications,
see Rao, J.N.K. and Molina, I. (2015) [#rm2015]_.

.. [#ms2001] McCulloch, C.E.and Searle, S.R. (2001), *Generalized, Linear, Mixed Models*,
   New York: John Wiley & Sons, Inc.
.. [#bhf1988] Battese, G.E., Harter, R.M., and Fuller, W.A. (1988). An error-components model for
   prediction of county crop areas using survey and satellite data, *Journal of the American
   Statistical Association*, **83**, 28-36.
.. [#rm2015] Rao, J.N.K. and Molina, I. (2015), *Small area estimation, 2nd edn.*,
   John Wiley & Sons, Hoboken, New Jersey.
"""

from __future__ import annotations

import warnings

from typing import Optional, Union

import numpy as np
import pandas as pd
import statsmodels.api as sm

from samplics.sae.sae_core_functions import area_stats
from samplics.utils.basic_functions import sumby
from samplics.utils.formats import dict_to_dataframe, numpy_array
from samplics.utils.types import Array, DictStrNum, Number, FitMethod


class EblupUnitModel:
    """*EblupUnitModel* implements the basic unit level model for means (a linear indicator).

    *EblupUnitModel* takes the sample data as input and fits the basic linear mixed model.
    The user can pick between restricted maximum likelihood (REML) or maximum likelihood (ML)
    to fit the model parameters. Also, EblupUnitModel predicts the areas means and provides
    the point and mean squared error (MSE) estimates of the empirical Bayes linear
    unbiased (EBLUP). User can also obtain the bootstrap mse estimates of the MSE.

    Setting attributes
        | method (str): the fitting method of the model parameters which can take the possible
        |   values restricted maximum likelihood (REML) or maximum likelihood (ML).
        |   If not specified, FitMethod.reml is used as default.

    Sample related attributes
        | ys (array): the output sample observations.
        | Xs (ndarray): the auxiliary information.
        | scales (array): an array of scaling parameters for the unit levels errors.
        | afactors (array): sum of the inverse squared of scale.
        | areas (array): the full vector of small areas from the sampled observations.
        | areas_list (array): the list of small areas from the sample data.
        | samp_size (dict): the sample size per small areas from the sample.
        | ys_mean (array): sample area means of the output variable.
        | Xs_mean (ndarray): sample area means of the auxiliary variables.}}}}}

    Model fitting attributes
        | fitted (boolean): indicates whether the model has been fitted or not.
        | fixed_effects (array): the estimated fixed effects of the regression model.
        | fe_std (array): the estimated standard errors of the fixed effects.
        | random_effects (array): the estimated area level random effects.
        | re_std (number): the estimated standard error of the random effects.
        | error_std (number): the estimated standard error of the unit level residuals.
        | convergence (dict): a dictionnary holding the convergence status and the number of
        |   iterations from the model fitting algorithm.
        | goodness (dict): a dictionary holding the log-likelihood, AIC, and BIC.
        | gamma (dict): ratio of the between-area variability (re_std**2) to the total
        |   variability (re_std**2 + error_std**2 / a_factor).

    Prediction related attributes
        | areap (array): the list of areas for the prediction.
        | Xmean (array): population means of the auxiliary variables.
        | number_reps (int): number of replicates for the bootstrap MSE estimation.
        | samp_rate (dict): sampling rates at the area level.
        | area_est (array): area level EBLUP estimates.
        | area_mse (array): area level taylor estimation of the MSE.
        | area_mse_boot (array): area level bootstrap estimation of the MSE.

    Main methods
        | fit(): fits the linear mixed model to estimate the model parameters using REMl or ML
        |   methods.
        | predict(): predicts the area level mean estimates which includes both the point
        |   estimates and the taylor MSE estimate.
        | bootstrap_mse(): computes the area level bootstrap MSE estimates of the mean.
    """

    def __init__(
        self,
        method: FitMethod = FitMethod.reml,
    ):
        # Setting
        self.method = method
        if self.method not in (FitMethod.reml, FitMethod.ml):
            raise AssertionError("Method must be 'REML' or 'ML'!")

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

        # Fitting stats
        self.fitted: bool = False
        self.fixed_effects: np.ndarray
        self.fe_std: np.ndarray
        self.random_effects: np.ndarray
        self.re_std: Number = 0
        self.error_std: Number = 0
        self.convergence: dict[str, Union[float, int, bool]] = {}
        self.goodness: dict[str, Number] = {}  # loglikehood, deviance, AIC, BIC
        self.gamma: DictStrNum

        # Predict(ion/ed) data
        self.areap: np.ndarray
        self.Xp_mean: np.ndarray
        self.number_reps: int = 0
        self.samp_rate: DictStrNum
        self.area_est: DictStrNum
        self.area_mse: DictStrNum
        self.area_mse_boot: Optional[DictStrNum] = None

    def _beta(
        self,
        y: np.ndarray,
        X: np.ndarray,
        area: np.ndarray,
        weight: np.ndarray,
    ) -> np.ndarray:

        Xw = X * weight[:, None]
        p = X.shape[1]
        beta1 = np.zeros((p, p))
        beta2 = np.zeros(p)
        for d in np.unique(area):
            aread = area == d
            w_d = weight[aread]
            y_d = y[aread]
            X_d = X[aread]
            Xw_d = Xw[aread]
            Xw_d_bar = np.sum(Xw_d, axis=0) / np.sum(w_d)
            resid_d_w = X_d - Xw_d_bar * self.gamma[d]
            beta1 = beta1 + np.matmul(np.transpose(Xw_d), resid_d_w)
            beta2 = beta2 + np.sum(resid_d_w * y_d[:, None] * w_d[:, None], axis=0)

        return np.asarray(np.matmul(np.linalg.inv(beta1), beta2))

    def _mse(
        self,
        areas: np.ndarray,
        Xs_mean: np.ndarray,
        Xp_mean: np.ndarray,
        gamma: np.ndarray,
        samp_size: np.ndarray,
        afactor: np.ndarray,
        A_inv: np.ndarray,
    ) -> np.ndarray:

        sigma2e = self.error_std**2
        sigma2u = self.re_std**2

        g1 = gamma * sigma2e / afactor

        xbar_diff = Xp_mean - gamma[:, None] * Xs_mean
        g2_matrix = xbar_diff @ A_inv @ np.transpose(xbar_diff)
        g2 = np.diag(g2_matrix)

        alpha = sigma2e + afactor * sigma2u
        i_vv = 0.5 * sum((afactor / alpha) ** 2)
        i_ee = 0.5 * sum((samp_size - 1) / (sigma2e**2) + 1 / (alpha**2))
        i_ve = 0.5 * sum(afactor / (alpha**2))
        i_determinant = i_vv * i_ee - i_ve * i_ve

        g3_afactor = (1 / afactor**2) * (1 / (sigma2u + sigma2e / afactor) ** 3)
        g3 = (
            g3_afactor
            * (
                (sigma2e**2) * i_ee
                + (sigma2u**2) * i_vv
                - 2 * (sigma2e * sigma2u) * (-i_ve)
            )
            / i_determinant
        )

        return np.asarray(g1 + g2 + 2 * g3)

    def fit(
        self,
        ys: Array,
        Xs: Array,
        areas: Array,
        samp_weight: Optional[Array] = None,
        scales: Union[Array, Number] = 1,
        intercept: bool = True,
        tol: float = 1e-6,
        maxiter: int = 100,
    ) -> None:
        """Fits the linear mixed models to estimate the model parameters that is the fixed
        effects, the random effects standard error and the unit level residuals' standard error.
        In addition, the method provides statistics related to the model fitting e.g. convergence
        status, log-likelihood, AIC, BIC, and more.

        Args:
            ys (Array): An array of the output sample observations.
            Xs (Array): An multi-dimensional array of the auxiliary information.
            areas (Array): An array of the sampled area provided at the unit level.
            samp_weight (Optional[Array], optional): An array of the sample weights.
                Defaults to None.
            scales (Union[Array, Number], optional): The scale factor for the unit level errors.
                If a single number of provided, the same number will be applied to all observations. Defaults to 1.
            intercept (bool, optional): An boolean to indicate whether an intercept need to be
                added to X. Defaults to True.
            tol (float, optional): tolerance used for convergence criteria. Defaults to 1.0e-4.
            maxiter (int, optional): maximum number of iterations for the fitting algorithm.
            Defaults to 100.
        """

        areas = numpy_array(areas)
        ys = numpy_array(ys)
        Xs = numpy_array(Xs)

        self.ys = ys
        self.Xs = Xs
        self.areas = areas
        self.areas_list = np.unique(areas)

        if intercept:
            if Xs.ndim == 1:
                n = Xs.shape[0]
                Xs = np.insert(Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                Xs = np.insert(Xs, 0, 1, axis=1)
        if samp_weight is not None:
            samp_weight = numpy_array(samp_weight)

        if isinstance(scales, (float, int)):
            scales = np.asarray(np.ones(ys.shape[0]) * scales)
        else:
            scales = numpy_array(scales)
        self.scales = scales

        self.afactors = dict(zip(self.areas_list, sumby(areas, scales)))

        reml = True if self.method == FitMethod.reml else False
        basic_model = sm.MixedLM(ys, Xs, areas)
        fit_kwargs = {
            "tol": tol,
            "gtol": tol,
            # "pgtol": tol,
            "maxiter": maxiter,
        }  # TODO: to improve in the future. Check: statsmodels.LikelihoodModel.fit()
        basic_fit = basic_model.fit(reml=reml, full_output=True, **fit_kwargs)


        self.error_std = basic_fit.scale**0.5
        self.fixed_effects = basic_fit.fe_params
        self.fe_std = basic_fit.bse_fe
        self.re_std = basic_fit.cov_re[0][0] ** 0.5
        self.convergence["achieved"] = basic_fit.converged
        self.convergence["iterations"] = len(basic_fit.hist[0]["allvecs"]) - 1

        nb_obs = ys.shape[0]
        nb_variance_params = basic_fit.cov_re.shape[0] + 1
        if self.method == FitMethod.reml:  # page 111 - Rao and Molina (2015)
            aic = -2 * basic_fit.llf + 2 * nb_variance_params
            bic = (
                -2 * basic_fit.llf
                + np.log(nb_obs - self.fixed_effects.shape[0]) * nb_variance_params
            )
        elif self.method == FitMethod.ml:
            aic = -2 * basic_fit.llf + 2 * (
                self.fixed_effects.shape[0] + nb_variance_params
            )
            bic = -2 * basic_fit.llf + np.log(nb_obs) * (
                self.fixed_effects.shape[0] + nb_variance_params
            )
        else:
            aic = np.nan
            bic = np.nan
        self.goodness["loglike"] = basic_fit.llf
        self.goodness["AIC"] = aic
        self.goodness["BIC"] = bic
        self.ys_mean, Xs_mean, gamma, samp_size = area_stats(
            ys,
            Xs,
            areas,
            self.error_std,
            self.re_std,
            self.afactors,
            samp_weight,
        )
        self.random_effects = gamma * (self.ys_mean - Xs_mean @ self.fixed_effects)
        self.gamma = dict(zip(self.areas_list, gamma))
        self.samp_size = dict(zip(self.areas_list, samp_size))
        self.Xs_mean = Xs_mean[:, 1:]

        self.fitted = True

    def predict(
        self,
        Xmean: Array,
        area: Array,
        pop_size: Optional[Array] = None,
        intercept: bool = True,
    ) -> None:
        """Predicts the area level means and provides the taylor MSE estimation of the estimated
        area means.

        Args:
            Xmean (Array): a multi-dimensional array of the population means of the auxiliary
                variables.
            area (Array): An array of the areas in the same order as Xmean and Popsize.
            pop_size (Optional[Array], optional): An array of the population size for the same
                areas as in Xmean and area and in the same order. Defaults to None.
            intercept (bool, optional): [description]. Defaults to True.

        Raises:
            Exception: when predict() is called before fitting the model.
        """

        if not self.fitted:
            raise Exception(
                "The model must be fitted first with .fit() before running the prediction."
            )

        Xmean = numpy_array(Xmean)
        self.Xp_mean = Xmean
        if intercept:
            if Xmean.ndim == 1:
                n = Xmean.shape[0]
                Xp_mean = np.insert(Xmean.reshape(n, 1), 0, 1, axis=1)
                Xs_mean = np.insert(self.Xs_mean.reshape(n, 1), 0, 1, axis=1)
                Xs = np.insert(self.Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                Xp_mean = np.insert(Xmean, 0, 1, axis=1)
                Xs_mean = np.insert(self.Xs_mean, 0, 1, axis=1)
                Xs = np.insert(self.Xs, 0, 1, axis=1)
        else:
            Xp_mean = self.Xp_mean
            Xs_mean = self.Xs_mean
            Xs = self.Xs

        area = numpy_array(area)
        pop_size_dict = {}
        if pop_size is not None:
            pop_size = numpy_array(pop_size)
            pop_size_dict = dict(zip(area, pop_size))

        self.areap = area

        ps = np.isin(self.areap, self.areas_list)

        mu = dict(zip(self.areap, Xp_mean @ self.fixed_effects))
        resid = dict(zip(self.areas_list, self.ys_mean - Xs_mean @ self.fixed_effects))

        area_est = {}
        samp_rate = {}
        for d in self.areap:
            if pop_size is not None and d in self.areas_list:
                samp_rate[d] = self.samp_size[d] / pop_size_dict[d]
            else:
                samp_rate[d] = 0
            if d in self.areas_list:
                area_est[d] = (
                    mu[d]
                    + (samp_rate[d] + (1 - samp_rate[d]) * self.gamma[d]) * resid[d]
                )
            else:
                area_est[d] = mu[d]

        self.samp_rate = samp_rate
        self.area_est = area_est

        A_ps = (
            np.diag(np.zeros(Xp_mean.shape[1]))
            if Xp_mean.ndim >= 2
            else np.asarray([0])
        )

        ps_area_list = self.areap[ps]
        for d in ps_area_list:
            areadps = self.areas == d
            n_ps_d = np.sum(areadps)
            X_ps_d = Xs[areadps]
            scale_ps_d = self.scales[areadps]
            V_ps_d = (self.error_std**2) * np.diag(scale_ps_d) + (
                self.re_std**2
            ) * np.ones([n_ps_d, n_ps_d])
            A_ps = A_ps + np.transpose(X_ps_d) @ np.linalg.inv(V_ps_d) @ X_ps_d

        ps_area_indices = np.isin(self.areas_list, ps_area_list)
        a_factor_ps = np.asarray(list(self.afactors.values()))[ps_area_indices]
        gamma_ps = np.asarray(list(self.gamma.values()))[ps_area_indices]
        samp_size_ps = np.asarray(list(self.samp_size.values()))[ps_area_indices]
        mse_ps = self._mse(
            ps_area_list,
            Xs_mean[ps_area_indices],
            Xp_mean[ps],
            gamma_ps,
            samp_size_ps,
            a_factor_ps,
            np.linalg.inv(A_ps),
        )
        self.area_mse = dict(zip(ps_area_list, mse_ps))

        # TODO: add non-sampled areas prediction (section 7.2.2, Rao and molina (2015))
        # pr_area_list = self.areap[~ps]

    def bootstrap_mse(
        self,
        number_reps: int = 500,
        intercept: bool = True,
        tol: float = 1.0e-6,
        maxiter: int = 100,
        show_progress: bool = True,
    ) -> None:
        """Computes the MSE bootstrap estimates of the area level mean estimates.

        Args:
            number_reps (int): Number of replicates for the bootstrap method.. Defaults to 500.
            scale (Union[Array, Number], optional): [description]. Defaults to 1.
            intercept (bool, optional): [description]. Defaults to True.
            tol: float = 1.0e-4,
            maxiter: int = 100,
            show_progress (bool, optional): shows a bar progress of the bootstrap replicates
                calculations. Defaults to True.

        Raises:
            Exception: when bootstrap_mse() is called before fitting the model.
        """

        if not self.fitted:
            raise Exception(
                "The model must be fitted first with .fit() before running the prediction."
            )

        if intercept:
            if self.Xs.ndim == 1:
                n = self.Xs.shape[0]
                Xp_mean = np.insert(self.Xp_mean.reshape(n, 1), 0, 1, axis=1)
                Xs = np.insert(self.Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                Xp_mean = np.insert(self.Xp_mean, 0, 1, axis=1)
                Xs = np.insert(self.Xs, 0, 1, axis=1)
        else:
            Xp_mean = self.Xp_mean
            Xs = self.Xs

        nb_areas = self.areap.size
        ps = np.isin(self.areap, self.areas_list)
        ps_area_list = self.areap[ps]

        for i, d in enumerate(ps_area_list):
            aread = self.areas == d
            scale_ps_d = self.scales[aread]
            error_d = np.abs(scale_ps_d) * np.random.normal(
                scale=self.error_std, size=(number_reps, self.samp_size[d])
            )
            re_boot_d = np.random.normal(scale=self.re_std, size=(number_reps, 1))
            re_d = np.repeat(re_boot_d, int(self.samp_size[d]), axis=1)

            if i == 0:
                error = error_d
                re_boot = re_boot_d
                re = re_d
                X_ps = Xs[aread]
                area_ps = self.areas[aread]
            else:
                error = np.append(error, error_d, axis=1)
                re_boot = np.append(re_boot, re_boot_d, axis=1)
                re = np.append(re, re_d, axis=1)
                X_ps = np.append(X_ps, Xs[aread], axis=0)
                area_ps = np.append(area_ps, self.areas[aread])

        mu = X_ps @ self.fixed_effects
        y_ps_boot = mu[None, :] + re + error
        i = 0
        bar_length = min(50, number_reps)
        steps = np.linspace(0, number_reps, bar_length).astype(int)

        fit_kwargs = {
            "tol": tol,
            "gtol": tol,
            # "pgtol": tol,
            "maxiter": maxiter,
        }  # TODO: to improve in the future. Check: statsmodels.LikelihoodModel.fit()
        reml = True if self.method == FitMethod.reml else False

        boot_mse = np.zeros((number_reps, nb_areas))
        print(f"Running the {number_reps} bootstrap iterations")
        for b in range(number_reps):
            with warnings.catch_warnings(record=True):
                boot_model = sm.MixedLM(y_ps_boot[b, :], X_ps, area_ps)
                boot_fit = boot_model.fit(reml=reml, **fit_kwargs)
            boot_fe = boot_fit.fe_params
            boot_error_std = boot_fit.scale**0.5
            boot_re_std = boot_fit.cov_re[0][0] ** 0.5
            boot_ys_mean, boot_Xs_mean, boot_gamma, _ = area_stats(
                y_ps_boot[b, :],
                X_ps,
                area_ps,
                boot_error_std,
                boot_re_std,
                self.afactors,
                None,
            )
            boot_re = boot_gamma * (boot_ys_mean - boot_Xs_mean @ boot_fe)
            boot_mu = Xp_mean @ self.fixed_effects + re_boot[b, :]
            boot_mu_h = Xp_mean @ boot_fe + boot_re
            boot_mse[b, :] = (boot_mu_h - boot_mu) ** 2

            if show_progress:
                if b in steps:
                    i += 1
                    print(
                        f"\r[%-{bar_length-1}s] %d%%"
                        % ("=" * i, 2 + (100 / bar_length) * i),
                        end="",
                    )
        if show_progress:
            print("\n")

        self.area_mse_boot = dict(
            zip(ps_area_list, np.asarray(np.mean(boot_mse, axis=0)))
        )

        # TODO: nonnegligeable sampling fractions, section 7.2.4, Rao and Molina (2015)

    def to_dataframe(
        self,
        col_names: Optional(list) = None,
        # col_names: list[str] = ["_parameter", "_area", "_estimate", "_mse", "_mse_boot"],
    ) -> pd.DataFrame:
        """Returns a pandas dataframe from dictionaries with same keys and one value per key.

        Args:
            col_names (list, optional): list of string to be used for the dataframe columns names.
                Defaults to ["_parameter", "_area", "_estimate", "_mse", "_mse_boot"].

        Returns:
            [pd.DataFrame]: a pandas dataframe
        """

        if self.area_est is None:
            raise AssertionError("No estimates yet. Must first run predict().")
        elif col_names is None:
            col_names = ["_parameter", "_area", "_estimate", "_mse", "_mse_boot"]
            if self.area_mse_boot is None:
                col_names.pop()
        else:
            ncols = len(col_names)
            if ncols != 5 and self.area_mse_boot is not None:
                raise AssertionError("col_names must have 5 values")

        if self.area_mse_boot is not None:
            est_df = dict_to_dataframe(
                col_names,
                self.area_est,
                self.area_mse,
                self.area_mse_boot,
            )
        else:
            est_df = dict_to_dataframe(
                col_names,
                self.area_est,
                self.area_mse,
            )
        est_df.iloc[:, 0] = "mean"

        return est_df
