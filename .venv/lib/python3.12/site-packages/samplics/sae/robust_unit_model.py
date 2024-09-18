"""EBLUP and EB Unit Models.

This module implements robust unit level models. The functionalities are organized in classes.
Each class has three main methods: *fit()*, *predict()* and *bootstrap_mse()*. Linear Mixed
Models (LMM) are the core underlying statistical framework used to model the hierarchical
nature of the small area estimation (SAE) techniques implemented in this module,
see McCulloch, C.E. and Searle, S.R. (2001) [#ms2001]_ for more details on LMM.

The *EllUnitModel* class implements the model Elbers, C., Lanjouw, J.O., and Lanjouw, P. (2003)
[#ell2003]_. This method is nonparametric at its core, hence does not require normality
assumption nor any other parametric distribution. This implementation a semiparametric and
nonparametric are provided. In the semiparametric, the normal distribution is used to fit the
parameters and to draw the fixed-effects.

For a comprehensive review of the small area estimation models and its applications,
see Rao, J.N.K. and Molina, I. (2015) [#rm2015]_.

.. [#ms2001] McCulloch, C.E.and Searle, S.R. (2001), *Generalized, Linear, Mixed Models*,
   New York: John Wiley & Sons, Inc.
.. [#ell2003] Elbers, C., Lanjouw, J.O., and Lanjouw, P. (2003), Micro-Level Estimation of Poverty
   and Inequality. *Econometrica*, **71**, 355-364.
.. [#rm2015] Rao, J.N.K. and Molina, I. (2015), *Small area estimation, 2nd edn.*,
   John Wiley & Sons, Hoboken, New Jersey.
"""

from __future__ import annotations

from typing import Any, Callable, Optional, Union

import numpy as np
import statsmodels.api as sm

from samplics.sae.eb_unit_model import EbUnitModel
from samplics.sae.sae_core_functions import area_stats
from samplics.utils import basic_functions, formats
from samplics.utils.types import Array, DictStrNum, Number


class EllUnitModel:
    """*EllUnitModel* implements the basic Unit level model for complex indicators.

    *EllUnitModel* takes the sample data as input and fits the basic linear mixed model.
    The user can pick between restricted maximum likelihood (REML), maximum likelihood (ML),
    and method of moments (MOM) to fit the model parameters. Also, EllUnitModel predicts the
    areas means and provides the point and mean squared error (MSE) estimates of the empirical
    Bayes linear unbiased (EBLUP).

    *EllUnitModel* requires the user to provide the indicator function. The indicator function is
    expected to take the array of output sample observations as input and possibly some additional
    parameters needed to compute the indicator. The indicator function outputs an aggregated value.
    For example, the poverty gap indicator can have the following signature
    pov_gap(y: array, pov_line: float) -> float. If the indicator function different outputs by
    area then the self.area_list can be used to incorporate different logics across areas.

    Also, EllUnitModel can use Boxcox to transform the output sample values in order to reduce the
    asymmetry in the datawhen fitting the linear mixed model.

    Setting attributes
        | method (str): the fitting method of the model parameters which can take the possible
        |   values restricted maximum likelihood (REML),  maximum likelihood (ML), and method of
        |   moments (MOM). If not specified, "MOM" is used as default.
        | indicator (function): a user defined function to compute the indicator.
        | boxcox (dict): contains the *lambda* parameter of the Boxcox and a constant for the
        |   log-transformation of the Boxcox.

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
        | random_effects (array): the estimated area level random effects
        | re_std (number): the estimated standard error of the random effects.
        | error_std (number): the estimated standard error of the unit level residuals.
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

    Main methods
        | fit(): fits the linear mixed model to estimate the model parameters using REMl or ML
        |   methods.
        | predict(): predicts the area level indicator estimates which includes both the point
        |   estimates and the taylor MSE estimate.
    """

    def __init__(
        self,
        method: str = "MOM",
        boxcox: Optional[float] = None,
        constant: Optional[Number] = None,
        indicator: Optional[Any] = None,
    ):

        # Setting
        self.method: str = method.upper()
        if self.method not in ("REML", "ML", "MOM"):
            raise AssertionError("Value provided for method is not valid!")
        self.indicator = indicator
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
        self.areap: np.ndarray
        self.Xp_mean: np.ndarray
        self.number_reps: int
        self.area_est: DictStrNum
        self.area_mse: DictStrNum

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
            ys (Array):  an array of the output sample observations.
            Xs (Array): an multi-dimensional array of the sample auxiliary information.
            areas (Array): provides the area of the sampled observations.
            samp_weight (Optional[Array], optional): An array of the sample weights.
                Defaults to None.
            scales (Union[Array, Number], optional): the scale factor for the unit level errors.
                If a single number of provided, the same number will be applied to all observations. Defaults to 1.
            intercept (bool, optional): An boolean to indicate whether an intercept need to be
                added to Xs. Defaults to True.
        """

        areas = formats.numpy_array(areas)
        ys = formats.numpy_array(ys)
        Xs = formats.numpy_array(Xs)
        if intercept:
            if Xs.ndim == 1:
                n = Xs.shape[0]
                Xs = np.insert(Xs.reshape(n, 1), 0, 1, axis=1)
            else:
                Xs = np.insert(Xs, 0, 1, axis=1)
        if samp_weight is not None:
            samp_weight = formats.numpy_array(samp_weight)

        if isinstance(scales, (float, int)):
            scales = np.asarray(np.ones(ys.shape[0]) * scales)
        else:
            scales = formats.numpy_array(scales)

        if self.method in ("REML", "ML"):
            eb_ul = EbUnitModel(
                method=self.method,
                boxcox=self.boxcox["lambda"],
                constant=self.boxcox["constant"],
            )
            eb_ul.fit(
                ys, Xs, areas, samp_weight, scales, False, tol=tol, maxiter=maxiter
            )
            self.scales = eb_ul.scales
            self.afactors = eb_ul.afactors
            self.ys = eb_ul.ys
            self.Xs = eb_ul.Xs
            self.areas = eb_ul.areas
            self.areas_list = eb_ul.areas_list
            self.error_std = eb_ul.error_std
            self.fixed_effects = eb_ul.fixed_effects
            self.fe_std = eb_ul.fe_std
            self.re_std = eb_ul.re_std
            self.convergence = eb_ul.convergence
            self.goodness = eb_ul.goodness
            self.ys_mean = eb_ul.ys_mean
            self.Xs_mean = eb_ul.Xs_mean
            self.gamma = eb_ul.gamma
            self.samp_size = eb_ul.samp_size
            self.fitted = eb_ul.fitted

        if self.method == "MOM":
            ys_transformed = basic_functions.transform(
                ys,
                llambda=self.boxcox["lambda"],
                constant=self.boxcox["constant"],
                inverse=False,
            )
            ols_fit = sm.OLS(ys_transformed, Xs).fit()
            # re_ols = basic_functions.averageby(areas, ols_fit.resid)
            self.fixed_effects = ols_fit.params
            self.scales = scales
            self.ys = ys
            self.Xs = Xs
            self.areas = areas
            self.areas_list = np.unique(areas)
            self.afactors = dict(
                zip(self.areas_list, basic_functions.sumby(areas, scales))
            )
            self.ys_mean, self.Xs_mean, _, samp_size = area_stats(
                ys, Xs, areas, 0, 1, self.afactors, samp_weight
            )
            self.samp_size = dict(zip(self.areas_list, samp_size))
            self.fitted = True

    def _predict_indicator_parametric(
        self,
        number_samples: int,
        indicator: Callable[..., Array],
        mu: Array,
        area: Array,
        sigma2u: Number,
        sigma2e: Number,
        scale: Array,
        max_array_length: int,
        show_progress: bool,
        **kwargs: Any,
    ) -> tuple[np.ndarray, np.ndarray]:

        areas = np.unique(area)
        nb_areas = len(areas)
        if show_progress:
            bar_length = min(50, nb_areas)
            steps = np.linspace(1, nb_areas - 1, bar_length).astype(int)
            print(f"Generating the {number_samples} replicates samples")

        k = 0
        eta = np.zeros((number_samples, nb_areas)) * np.nan
        for i, d in enumerate(areas):
            aread = area == d
            mu_d = mu[aread]
            scale_d = scale[aread]
            N_d = np.sum(aread)
            cycle_size = max(int(max_array_length // N_d), 1)
            number_cycles = int(number_samples // cycle_size)
            last_cycle_size = number_samples % cycle_size

            for j in range(number_cycles + 1):
                if j == number_cycles:
                    cycle_size = last_cycle_size
                re_effects = np.random.normal(scale=sigma2u**0.5, size=cycle_size)
                errors = np.random.normal(
                    scale=scale_d * (sigma2e**0.5), size=(cycle_size, N_d)
                )
                y_d_j = mu_d[None, :] + re_effects[:, None] + errors
                if j == 0:
                    y_d = y_d_j
                else:
                    y_d = np.append(y_d, y_d_j, axis=0)

            if show_progress:
                if i in steps:
                    k += 1
                    print(
                        f"\r[%-{bar_length}s] %d%%"
                        % ("=" * (k + 1), (k + 1) * (100 / bar_length)),
                        end="",
                    )

            z_d = basic_functions.transform(
                y_d,
                llambda=self.boxcox["lambda"],
                constant=self.boxcox["constant"],
                inverse=True,
            )
            eta[:, i] = np.apply_along_axis(indicator, axis=1, arr=z_d, **kwargs)

        if show_progress:
            print("\n")

        ell_estimate = np.mean(eta, axis=0)
        ell_mse = np.mean(np.power(eta - ell_estimate[None, :], 2), axis=0)

        return np.asarray(ell_estimate), np.asarray(ell_mse)

    def _predict_indicator_nonparametric(
        self,
        number_samples: int,
        indicator: Callable[..., Array],
        mu: np.ndarray,
        area: np.ndarray,
        total_residuals: np.ndarray,
        max_array_length: int,
        show_progress: bool,
        **kwargs: Any,
    ) -> tuple[np.ndarray, np.ndarray]:

        areas = np.unique(area)
        nb_areas = len(areas)
        if show_progress:
            bar_length = min(50, nb_areas)
            steps = np.linspace(1, nb_areas - 1, bar_length).astype(int)
            print(f"Generating the {number_samples} replicates samples")

        k = 0
        area_effects = basic_functions.averageby(self.areas, total_residuals)
        unit_errors = None
        for i, d in enumerate(self.areas_list):
            total_residuals_d = total_residuals[self.areas == d]
            if i == 0:
                unit_errors = total_residuals_d - area_effects[i]
            else:
                unit_errors = np.append(
                    unit_errors, total_residuals_d - area_effects[i]
                )

        eta = np.zeros((number_samples, nb_areas)) * np.nan
        for i, d in enumerate(areas):
            aread = area == d
            mu_d = mu[aread]
            N_d = np.sum(aread)
            cycle_size = max(int(max_array_length // N_d), 1)
            number_cycles = int(number_samples // cycle_size)
            last_cycle_size = number_samples % cycle_size

            y_d = None
            for j in range(number_cycles + 1):
                if j == number_cycles:
                    cycle_size = last_cycle_size
                re_effects = np.random.choice(area_effects, size=cycle_size)
                errors = np.random.choice(unit_errors, size=(cycle_size, N_d))
                y_d_j = mu_d[None, :] + re_effects[:, None] + errors
                if j == 0:
                    y_d = y_d_j
                else:
                    y_d = np.append(y_d, y_d_j, axis=0)

            if show_progress:
                if i in steps:
                    k += 1
                    print(
                        f"\r[%-{bar_length}s] %d%%"
                        % ("=" * (k + 1), (k + 1) * (100 / bar_length)),
                        end="",
                    )

        if show_progress:
            print("\n")

        ell_estimate = np.mean(eta, axis=0)
        ell_mse = np.mean(np.power(eta - ell_estimate[None, :], 2), axis=0)

        return np.asarray(ell_estimate), np.asarray(ell_mse)

    def predict(
        self,
        X: Array,
        area: Array,
        indicator: Callable[..., Array],
        number_samples: int,
        scale: Array = 1,
        intercept: bool = True,
        max_array_length: int = int(100e6),
        show_progress: bool = True,
        **kwargs: Any,
    ) -> None:
        """Predicts the area level indicator and its the MSE estimates.

        Args:
            X (Array): an multi-dimensional array of the auxiliary variables for the population.
            area (Array): provides the area of the population units.
            indicator (Callable[..., Array]): a user defined function which computes the area level
                indicators. The function should take y (output variable) as the first parameters,
                additional parameters can be used. Use ***kwargs* to transfer the additional
                parameters.
            number_samples (int): [description]
            scale (Array, optional): [description]. Defaults to 1.
            intercept (bool, optional): An boolean to indicate whether an intercept need to be
                added to X. Defaults to True.
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

        X = formats.numpy_array(X)
        self.number_samples = int(number_samples)
        if isinstance(scale, (float, int)):
            scale = np.ones(X.shape[0]) * scale
        else:
            scale = formats.numpy_array(scale)
        area = formats.numpy_array(area)
        self.areas_p = np.unique(area)
        X = formats.numpy_array(X)
        if intercept:
            if X.ndim == 1:
                n = X.shape[0]
                X = np.insert(X.reshape(n, 1), 0, 1, axis=1)
            else:
                X = np.insert(X, 0, 1, axis=1)

        mu = X @ self.fixed_effects

        if self.method in ("REML", "ML"):
            area_est, area_mse = self._predict_indicator_parametric(
                self.number_samples,
                indicator,
                mu,
                area,
                self.re_std**2,
                self.error_std**2,
                scale,
                max_array_length,
                show_progress,
                **kwargs,
            )
        if self.method in ("MOM"):
            # y_transformed_s = basic_functions.transform(
            #    self.y_s, llambda=self.boxcox["lambda"], inverse=False
            # )
            # total_residuals = y_transformed_s - self.X_s @ self.fixed_effects
            total_residuals = self.ys - self.Xs @ self.fixed_effects
            area_est, area_mse = self._predict_indicator_nonparametric(
                self.number_samples,
                indicator,
                mu,
                area,
                total_residuals,
                max_array_length,
                show_progress,
                **kwargs,
            )

        self.area_est = dict(zip(self.areas_p, area_est))
        self.area_mse = dict(zip(self.areas_p, area_mse))
