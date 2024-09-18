"""EBLUP Area Model

The module implements the basic EBLUP area level model initially developed by Fay, R.E. and
Herriot, R.A. (1979) [#fh1979]_ and explicitly (mathematically) formulated in Rao, J.N.K. and
Molina, I. (2015) [#rm2015]_. The functionalities are organized in one class *EblupAreaModel* with
two main methods: *fit()* and *predict()*. The unit level standard error is assume known and the
model only estimate the fixed effects and the standard error of the random effects. The model
parameters can be fitted using restricted maximum likelihood (REML), maximum likelihood (ML) or
Fay-Herriot (FH). The method *predict()* provides both the mean and the MSE estimates at the area
level.

.. [#fh1979] Fay, R.E. and Herriot, R.A. (1979), Estimation of Income from Small Places: An
   Application of James-Stein Procedures to Census Data. *Journal of the American Statistical
   Association*, **74**, 269-277.
"""

from __future__ import annotations

import math

from typing import Optional, Union

import numpy as np
import pandas as pd

from samplics.utils.formats import dict_to_dataframe, numpy_array
from samplics.utils.types import Array, DictStrNum, Number, FitMethod


class EblupAreaModel:
    """*EblupAreaModel* implements the basic area level model.

    *EblupAreaModel* takes the unstable survey estimates as input and model them to produce the
    small area estimates. The standard error of the unit level residuals are assumed known and
    usually correspond to the survey estimates standard error. In some cases, the survey standard
    errors (or variances) are modelled to reduce their variability and the modelled standard errors
    are them used for the computes the small area estimates. The user can pick between REML, ML and
    FH to estimate the model parameters. EblupAreaModel provides both the area level point estimates and the taylor approximation of the MSEs.

    Setting attributes
        | method (str): the fitting method of the model parameters which can take the possible
        |   values restricted maximum likelihood (REML), maximum likelihood (ML), and
        |   Fay-Herriot (FH). If not specified, FitMethod.reml is used as default.

    Sample related attributes
        | yhat (array): the survey output area level estimates. This is also referred to as the
        | direct estimates.
        | error_std (number): the estimated standard error of the direct estimates.
        | X (ndarray): the auxiliary information.
        | b_const (array): an array of scaling parameters for the area random effects.
        | area (array): the area of the survey output estimates.
        | samp_size (dict): the sample size per small areas from the sample.
        | ys_mean (array): sample area means of the output variable.
        | Xs_mean (ndarray): sample area means of the auxiliary variables.

    Model fitting attributes
        | fitted (boolean): indicates whether the model has been fitted or not.
        | fixed_effects (array): the estimated fixed effects of the regression model.
        | fe_std (array): the estimated standard errors of the fixed effects.
        | random_effects (array): the estimated standard errors of the fixed effects.
        | re_std (number): the estimated area level standard error of the random effects.
        | convergence (dict): a dictionnary holding the convergence status and the number of
        |   iterations from the model fitting algorithm.
        | goodness (dict): a dictionary holding the log-likelihood, AIC, and BIC.

    Prediction related attributes
        | area_est (array): area level modelled estimates.
        | area_mse (array): area level taylor estimation of the MSE.

    Main methods
        | fit(): fits the linear mixed model to estimate the model parameters using REMl or ML
        |   methods.
        | predict(): predicts the area level mean estimates which includes both the point   | |
        |   estimates and the taylor MSE estimate.
    """

    def __init__(self, method: FitMethod = FitMethod.reml) -> None:
        if method not in (FitMethod.fh, FitMethod.ml, FitMethod.reml):
            raise AssertionError("Parameter method must be 'FH', 'ML, or 'REML'!")
        else:
            self.method = method

        # Sample data
        self.yhat: np.ndarray
        self.error_std: np.ndarray
        self.X: np.ndarray
        self.area: np.ndarray

        # Fitting stats
        self.fitted: bool = False
        self.fixed_effects: np.ndarray
        self.fe_std: np.ndarray
        self.re_std: Number
        self.convergence: dict[str, Union[float, int, bool]] = {}
        self.goodness: dict[str, Number] = {}  # loglikehood, deviance, AIC, BIC

        # Predict(ino/ed) data
        self.area_est: DictStrNum
        self.area_mse: DictStrNum
        self.area_mse_as1: DictStrNum
        self.area_mse_as2: DictStrNum
        self.area_mse_terms: dict[str, DictStrNum]

    def __str__(self) -> str:
        estimation = pd.DataFrame()
        estimation["area"] = self.area
        estimation["estimate"] = self.area_est
        estimation["mse"] = self.area_est

        fit = pd.DataFrame()
        fit["beta_coef"] = self.fixed_effects
        fit["beta_stderr"] = np.diag(self.fe_std)

        return f"""\n\nFH Area Model - Best predictor,\n\nConvergence status: {self.convergence['achieved']}\nNumber of iterations: {self.convergence['iterations']}\nPrecision: {self.convergence['precision']}\n\nGoodness of fit: {self.goodness}\n\nEstimation:\n{estimation}\n\nFixed effect:\n{fit}\n\nRandom effect variance:\n{self.re_std**2}\n\n"""

    def __repr__(self) -> str:
        return self.__str__()

    @staticmethod
    def _fixed_coefficients(
        area: np.ndarray,
        yhat: np.ndarray,
        X: np.ndarray,
        sigma2_e: np.ndarray,
        sigma2_v: float,
        b_const: np.ndarray,
    ) -> tuple[np.ndarray, np.ndarray]:
        # V = np.diag(sigma2_v * (b_const**2) + sigma2_e)
        # V_inv = np.linalg.inv(V)
        V_inv = np.diag(1 / (sigma2_v * (b_const**2) + sigma2_e))
        x_v_X_inv = np.linalg.inv(np.matmul(np.matmul(np.transpose(X), V_inv), X))
        x_v_x_inv_x = np.matmul(np.matmul(x_v_X_inv, np.transpose(X)), V_inv)
        beta_hat = np.matmul(x_v_x_inv_x, yhat)
        beta_cov = np.matmul(np.matmul(np.transpose(X), V_inv), X)

        return beta_hat.ravel(), np.linalg.inv(beta_cov)

    def _log_likelihood(
        self, y: np.ndarray, X: np.ndarray, beta: np.ndarray, V: np.ndarray
    ) -> Number:
        m = y.size
        const = m * np.log(2 * np.pi)
        ll_term1 = np.log(np.linalg.det(V))
        V_inv = np.linalg.inv(V)
        resid_term = y - np.dot(X, beta)
        if self.method in (FitMethod.ml, FitMethod.fh):  # Whta is likelihood for FH
            resid_var = np.dot(np.transpose(resid_term), V_inv)
            ll_term2 = np.dot(resid_var, resid_term)
            loglike = -0.5 * (const + ll_term1 + ll_term2)
        elif self.method == FitMethod.reml:
            xT_vinv_x = np.dot(np.dot(np.transpose(X), V_inv), X)
            ll_term2 = np.log(np.linalg.det(xT_vinv_x))
            ll_term3 = np.dot(np.dot(y, V_inv), resid_term)
            loglike = -0.5 * (const + ll_term1 + ll_term2 + ll_term3)
        else:
            raise AssertionError("A fitting method must be specified.")

        return float(loglike)

    def _partial_derivatives(
        self,
        area: np.ndarray,
        yhat: np.ndarray,
        X: np.ndarray,
        sigma2_e: np.ndarray,
        sigma2_v: Number,
        b_const: np.ndarray,
    ) -> tuple[Number, Number]:
        deriv_sigma = 0.0
        info_sigma = 0.0
        if self.method == FitMethod.ml:
            beta, beta_cov = self._fixed_coefficients(
                area=area,
                yhat=yhat,
                X=X,
                sigma2_e=sigma2_e,
                sigma2_v=sigma2_v,
                b_const=b_const,
            )
            for d in area:
                b_d = b_const[area == d]
                phi_d = sigma2_e[area == d]
                X_d = X[area == d, :]
                yhat_d = yhat[area == d]
                mu_d = np.matmul(X_d, beta)
                resid_d = yhat_d - mu_d
                sigma2_d = sigma2_v * (b_d**2) + phi_d
                term1 = (b_d**2 / sigma2_d)[0]
                term2 = (((b_d**2) * (resid_d**2)) / (sigma2_d**2))
                deriv_sigma += -0.5 * (term1 - term2)
                info_sigma += 0.5 * (term1**2)
        elif self.method == FitMethod.reml:
            B = np.diag(b_const**2)
            v_i = sigma2_e + sigma2_v * (b_const**2)
            V = np.diag(v_i)
            v_inv = np.linalg.inv(V)
            x_vinv_x = np.matmul(np.matmul(np.transpose(X), v_inv), X)
            x_xvinvx_x = np.matmul(np.matmul(X, np.linalg.inv(x_vinv_x)), np.transpose(X))
            P = v_inv - np.matmul(np.matmul(v_inv, x_xvinvx_x), v_inv)
            P_B = np.matmul(P, B)
            P_B_P = np.matmul(P_B, P)
            term1 = np.trace(P_B)
            term2 = np.matmul(np.matmul(np.transpose(yhat), P_B_P), yhat)
            deriv_sigma = -0.5 * (term1 - term2)
            info_sigma = 0.5 * np.trace(np.matmul(P_B_P, B))
        elif self.method == FitMethod.fh:  # Fay-Herriot approximation
            beta, beta_cov = self._fixed_coefficients(
                area=area,
                yhat=yhat,
                X=X,
                sigma2_e=sigma2_e,
                sigma2_v=sigma2_v,
                b_const=b_const,
            )
            for d in area:
                b_d = b_const[area == d]
                phi_d = sigma2_e[area == d]
                X_d = X[area == d, :]
                yhat_d = yhat[area == d]
                mu_d = np.dot(X_d, beta)
                resid_d = yhat_d - mu_d
                sigma2_d = sigma2_v * (b_d**2) + phi_d
                deriv_sigma += ((resid_d**2) / sigma2_d)[0]
                info_sigma += -(((b_d**2) * (resid_d**2)) / (sigma2_d**2))[0]
            m = yhat.size
            p = X.shape[1]
            deriv_sigma = m - p - deriv_sigma

        return deriv_sigma, info_sigma

    def _iterative_fisher_scoring(
        self,
        area: np.ndarray,
        yhat: np.ndarray,
        X: np.ndarray,
        sigma2_e: np.ndarray,
        b_const: np.ndarray,
        sigma2_v_start: float,
        tol: float,
        maxiter: int,
    ) -> tuple[float, float, int, float, bool]:  # May not need variance
        """Fisher-scroring algorithm for estimation of variance component
        return (sigma, covariance, number_iterations, tolerance, covergence status)"""

        iterations = 0
        tolerance = tol + 1.0
        sigma2_v = sigma2_v_start
        info_sigma = 0.0
        while iterations < maxiter and tolerance > tol:
            sigma2_v_previous = sigma2_v
            deriv_sigma, info_sigma = self._partial_derivatives(
                area=area,
                yhat=yhat,
                X=X,
                sigma2_e=sigma2_e,
                sigma2_v=sigma2_v,
                b_const=b_const,
            )
            sigma2_v += deriv_sigma / info_sigma
            tolerance = abs((sigma2_v - sigma2_v_previous) / sigma2_v_previous)
            iterations += 1

        return (
            max(sigma2_v, 0),
            1 / info_sigma,
            iterations,
            tolerance,
            tolerance <= tol,
        )

    def _eb_estimates(
        self,
        X: np.ndarray,
        beta: np.ndarray,
        area: np.ndarray,
        sigma2_e: np.ndarray,
        sigma2_v: Number,
        sigma2_v_cov: Number,
        b_const: np.ndarray,
    ) -> tuple[
        np.ndarray,
        np.ndarray,
        np.ndarray,
        np.ndarray,
        np.ndarray,
        np.ndarray,
        np.ndarray,
        np.ndarray,
    ]:
        m = self.yhat.size
        v_i = sigma2_e + sigma2_v * (b_const**2)
        V_inv = np.diag(1 / v_i)
        G = np.diag(np.ones(m) * sigma2_v)

        Z = np.diag(b_const)

        b = np.matmul(np.matmul(G, np.transpose(Z)), V_inv)
        d = np.transpose(X - np.matmul(np.transpose(b), X))
        x_vinv_x = np.matmul(np.matmul(np.transpose(X), V_inv), X)
        g2_term = np.linalg.inv(x_vinv_x)

        b_term_ml1 = np.linalg.inv(x_vinv_x)
        b_term_ml2_diag = (b_const**2) / (v_i**2)
        b_term_ml2 = np.matmul(np.matmul(np.transpose(X), np.diag(b_term_ml2_diag)), X)
        b_term_ml = float(np.trace(np.matmul(b_term_ml1, b_term_ml2)))

        estimates = np.array(self.yhat) * np.nan
        g1 = np.array(self.yhat) * np.nan
        g2 = np.array(self.yhat) * np.nan
        g3 = np.array(self.yhat) * np.nan
        g3_star = np.array(self.yhat) * np.nan

        g1_partial = np.array(self.yhat) * np.nan

        sum_inv_vi2 = np.sum(1 / (v_i**2))
        b_sigma2_v = 0.0
        if self.method == FitMethod.reml:
            g3_scale = 2.0 / sum_inv_vi2
        elif self.method == FitMethod.ml:
            b_sigma2_v = -(1.0 / 2.0 * sigma2_v_cov) * b_term_ml
            g3_scale = 2.0 / sum_inv_vi2
        elif self.method == FitMethod.fh:
            sum_vi = np.sum((1 / v_i))
            b_sigma2_v = 2.0 * (m * sum_inv_vi2 - sum_vi**2) / (sum_vi**3)
            g3_scale = 2.0 * m / sum_vi**2
        else:
            g3_scale = 0.0

        for d in area:
            b_d = b_const[area == d]
            phi_d = sigma2_e[area == d]
            X_d = X[area == d]
            yhat_d = self.yhat[area == d]
            mu_d = np.matmul(X_d, beta)
            resid_d = yhat_d - mu_d
            variance_d = sigma2_v * (b_d**2) + phi_d
            gamma_d = sigma2_v * (b_d**2) / variance_d
            estimates[area == d] = gamma_d * yhat_d + (1 - gamma_d) * mu_d
            g1[area == d] = gamma_d * phi_d
            g2_term_d = np.matmul(np.matmul(X_d, g2_term), np.transpose(X_d))
            g2[area == d] = ((1 - gamma_d) ** 2) * g2_term_d[0]
            g3[area == d] = ((1 - gamma_d) ** 2) * g3_scale / variance_d
            g3_star[area == d] = (g3[area == d] / variance_d) * (resid_d**2)
            g1_partial[area == d] = (b_d**2) * ((1 - gamma_d) ** 2) * b_sigma2_v

        mse = 0
        mse1_area_specific = 0
        mse2_area_specific = 0
        if self.method == FitMethod.reml:
            mse = g1 + g2 + 2 * g3
            mse1_area_specific = g1 + g2 + 2 * g3_star
            mse2_area_specific = g1 + g2 + g3 + g3_star
        elif self.method in (FitMethod.ml, FitMethod.fh):
            mse = g1 - g1_partial + g2 + 2 * g3
            mse1_area_specific = g1 - g1_partial + g2 + 2 * g3_star
            mse2_area_specific = g1 - g1_partial + g2 + g3 + g3_star

        return (
            np.asarray(estimates),
            np.asarray(mse),
            np.asarray(mse1_area_specific),
            np.asarray(mse2_area_specific),
            np.asarray(g1),
            np.asarray(g2),
            np.asarray(g3),
            np.asarray(g3_star),
        )

    def fit(
        self,
        yhat: Array,
        X: Array,
        area: Array,
        error_std: Array,
        re_std_start: float = 0.001,
        b_const: Union[np.ndarray, Number] = 1.0,
        intercept: bool = True,
        tol: float = 1e-4,
        maxiter: int = 100,
    ) -> None:
        """Fits the linear mixed models to estimate the fixed effects and the standard error of
        the random effects. In addition, the method provides statistics related to the model
        fitting e.g. convergence status, log-likelihood, and more.

        Args:
            yhat (Array): an array of the estimated area level survey estimates also called
            the direct estimates.
            X (Array): an multi-dimensional array of the auxiliary information associated to
            the sampled areas.
            area (Array): provides the areas associated to the direct estimates.
            error_std (Array): [description]
            re_std_start (float, optional): [description]. Defaults to 0.001.
            b_const (Union[np.array, Number], optional): [description]. Defaults to 1.0.
            tol (float, optional): tolerance used for convergence criteria. Defaults to 1.0e-4.
            maxiter (int, optional): maximum number of iterations for the fitting algorithm.
            Defaults to 100.
        """

        area = numpy_array(area)
        yhat = numpy_array(yhat)
        X = numpy_array(X)

        error_std = numpy_array(error_std)

        if (error_std <= 0).any():
            raise ValueError(
                "Some of the standard errors are not strictly positive. All standard errors must be greater than 0."
            )

        if isinstance(b_const, (int, float)):
            b_const = np.asarray(np.ones(area.size) * b_const)
        else:
            b_const = numpy_array(b_const)

        if intercept and isinstance(X, np.ndarray):
            X = np.insert(X, 0, 1, axis=1)

        (
            sigma2_v,
            sigma2_v_cov,
            iterations,
            tolerance,
            convergence,
        ) = self._iterative_fisher_scoring(
            area=area,
            yhat=yhat,
            X=X,
            sigma2_e=error_std**2,
            b_const=b_const,
            sigma2_v_start=re_std_start**2,
            tol=tol,
            maxiter=maxiter,
        )

        beta, beta_cov = self._fixed_coefficients(
            area=area,
            yhat=yhat,
            X=X,
            sigma2_e=error_std**2,
            sigma2_v=sigma2_v,
            b_const=b_const,
        )

        self.yhat = yhat
        self.error_std = error_std
        self.X = X
        self.area = area
        self.fixed_effects = beta
        self.fe_std = np.diag(beta_cov) ** (1 / 2)
        self.re_std = sigma2_v ** (1 / 2)
        self.re_std_cov = sigma2_v_cov

        self.convergence["achieved"] = convergence
        self.convergence["iterations"] = iterations
        self.convergence["precision"] = tolerance

        m = yhat.size
        p = X.shape[1] + 1
        Z_b2_Z = np.ones(shape=(m, m))
        V = np.diag(error_std**2) + sigma2_v * Z_b2_Z
        logllike = self._log_likelihood(yhat, X=X, beta=self.fixed_effects, V=V)
        self.goodness["loglike"] = logllike
        self.goodness["AIC"] = -2 * logllike + 2 * (p + 1)
        self.goodness["BIC"] = -2 * logllike + math.log(m) * (p + 1)

        self.fitted = True

    def predict(
        self,
        X: Array,
        area: Array,
        b_const: Union[np.ndarray, Number] = 1.0,
        intercept: bool = True,
    ) -> None:
        """Provides the modelled area levels estimates and their MSE estimates.

        Args:
            X (Array): an multi-dimensional array of the auxiliary variables associated to
            areas to predict.
            area (Array): provides the areas for the prediction.
            error_std (Array):
            b_const (Union[np.ndarray, Number], optional): [description]. Defaults to 1.0.

        Raises:
            Exception: [description]
        """

        area = numpy_array(area)
        if not self.fitted:
            raise Exception(
                "The model must be fitted first with .fit() before running the prediction."
            )

        X = numpy_array(X)
        if intercept and isinstance(X, np.ndarray):
            X = np.insert(X, 0, 1, axis=1)

        if isinstance(b_const, (int, float)):
            b_const = np.asarray(np.ones(area.size) * b_const)
        else:
            b_const = numpy_array(b_const)

        point_est, mse, mse1, mse2, g1, g2, g3, g3_star = self._eb_estimates(
            X=X,
            area=area,
            beta=self.fixed_effects,
            sigma2_e=self.error_std**2,
            sigma2_v=self.re_std**2,
            sigma2_v_cov=self.re_std_cov,
            b_const=b_const,
        )

        self.area_est = dict(zip(area, point_est))
        self.area_mse = dict(zip(area, mse))

    def to_dataframe(self, col_names: Optional(list) = None) -> pd.DataFrame:
        """Returns a pandas dataframe from dictionaries with same keys and one value per key.

        Args:
            col_names (list, optional): list of string to be used for the dataframe columns names.
                Defaults to ["_parameter", "_area", "_estimate", "_mse"].

        Returns:
            [type]: a pandas dataframe
        """

        est_df = dict_to_dataframe(col_names, self.area_est, self.area_mse)
        est_df.iloc[:, 0] = "mean"

        return est_df
