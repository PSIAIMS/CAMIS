"""EBLUP Area Model

"""


import numpy as np
import polars as pl

from samplics.types import (
    AuxVars,
    DictStrNum,
    DirectEst,
    EblupEst,
    FitMethod,
    FitStats,
    Number,
)


def _predict_eblup(
    x: AuxVars,
    fit_eblup: FitStats,
    y: DirectEst,
    intercept: bool = True,  # if True, it adds an intercept of 1
    b_const: DictStrNum | Number = 1.0,
) -> EblupEst:
    area = y.to_numpy(keep_vars="__domain").flatten()

    if isinstance(b_const, (int, float)):
        b_const = dict(zip(area, np.ones(area.size) * b_const))

    sigme2_e = {}
    for d in fit_eblup.err_stderr:
        sigme2_e[d] = fit_eblup.err_stderr[d] ** 2

    est, mse, mse1, mse2, g1, g2, g3, g3_star = _eblup_estimates(
        method=fit_eblup.method,
        yhat=y.est,
        auxvars=x,
        area=area,
        beta=np.array(fit_eblup.fe_est),
        sigma2_e=sigme2_e,
        sigma2_v=fit_eblup.re_stderr**2,
        sigma2_v_cov=fit_eblup.re_stderr_var,
        intercept=intercept,
        b_const=b_const,
    )

    return EblupEst(pred=est, fit_stats=fit_eblup, domain=None, mse=mse, mse_boot=None, mse_jkn=None)


def _eblup_estimates(
    method: FitMethod,
    yhat: dict,
    auxvars: np.ndarray,
    beta: np.ndarray,
    area: np.ndarray,
    sigma2_e: dict,
    sigma2_v: Number,
    sigma2_v_cov: Number,
    intercept: bool,
    b_const: dict,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray,]:
    m = len(yhat)
    b_const_vec = np.array(list(b_const.values()))
    v_i = np.array(list(sigma2_e.values())) + sigma2_v * (b_const_vec**2)
    v_inv = np.diag(1 / v_i)
    G = np.diag(np.ones(m) * sigma2_v)
    Z = np.diag(b_const_vec)
    b = (G @ np.transpose(Z)) @ v_inv

    if intercept:
        x = np.insert(auxvars.to_numpy(drop_vars=["__record_id", "__domain"]), 0, 1, axis=1)  # add the intercept
    else:
        x = auxvars.to_numpy(drop_vars=["__record_id", "__domain"])

    d = np.transpose(x - np.transpose(b) @ x)
    x_vinv_x = np.transpose(x) @ v_inv @ x

    g2_term = np.linalg.pinv(x_vinv_x)

    b_term_ml1 = g2_term  # np.linalg.inv(x_vinv_x)
    b_term_ml2_diag = (b_const_vec**2) / (v_i**2)
    b_term_ml2 = np.matmul(np.matmul(np.transpose(x), np.diag(b_term_ml2_diag)), x)
    b_term_ml = float(np.trace(np.matmul(b_term_ml1, b_term_ml2)))

    estimates = {}  # np.zeros(m) * np.nan
    g1 = {}  # np.zeros(m) * np.nan
    g2 = {}  # np.zeros(m) * np.nan
    g3 = {}  # np.zeros(m) * np.nan
    g3_star = {}  # np.zeros(m) * np.nan

    g1_partial = {}  # np.zeros(m) * np.nan

    sum_inv_vi2 = np.sum(1 / (v_i**2))
    b_sigma2_v = 0.0
    if method == FitMethod.reml:
        g3_scale = 2.0 / sum_inv_vi2
    elif method == FitMethod.ml:
        b_sigma2_v = -(1.0 / 2.0 * sigma2_v_cov) * b_term_ml
        g3_scale = 2.0 / sum_inv_vi2
    elif method == FitMethod.fh:
        sum_vi = np.sum((1 / v_i))
        b_sigma2_v = 2.0 * (m * sum_inv_vi2 - sum_vi**2) / (sum_vi**3)
        g3_scale = 2.0 * m / sum_vi**2
    else:
        g3_scale = 0.0

    mse = {}
    mse1_area_specific = {}
    mse2_area_specific = {}

    for d in area:
        b_d = b_const[d]
        phi_d = sigma2_e[d]

        if intercept:
            X_d = np.insert(pl.from_dict(auxvars.x[d]).to_numpy(), 0, 1, axis=1)
        else:
            X_d = pl.from_dict(auxvars.x[d]).to_numpy()

        yhat_d = yhat[d]
        mu_d = X_d @ beta
        resid_d = yhat_d - mu_d
        variance_d = sigma2_v * (b_d**2) + phi_d
        gamma_d = sigma2_v * (b_d**2) / variance_d
        estimates[d] = (gamma_d * yhat_d + (1 - gamma_d) * mu_d)[0]
        g1[d] = gamma_d * phi_d
        g2_term_d = ((X_d @ g2_term) @ np.transpose(X_d))[0][0]
        g2[d] = ((1 - gamma_d) ** 2) * (g2_term_d)
        g3[d] = ((1 - gamma_d) ** 2) * g3_scale / variance_d
        g3_star[d] = ((g3[d] / variance_d) * (resid_d**2))[0]
        g1_partial[d] = (b_d**2) * ((1 - gamma_d) ** 2) * b_sigma2_v

        if method == FitMethod.reml:
            mse[d] = g1[d] + g2[d] + 2 * g3[d]
            mse1_area_specific[d] = g1[d] + g2[d] + 2 * g3_star[d]
            mse2_area_specific[d] = g1[d] + g2[d] + g3[d] + g3_star[d]
        elif method in (FitMethod.fh, FitMethod.ml):
            mse[d] = g1 - g1_partial[d] + g2[d] + 2 * g3[d]
            mse1_area_specific[d] = g1[d] - g1_partial[d] + g2[d] + 2 * g3_star[d]
            mse2_area_specific[d] = g1[d] - g1_partial[d] + g2[d] + g3[d] + g3_star[d]

    return (
        estimates,
        mse,
        mse1_area_specific,
        mse2_area_specific,
        g1,
        g2,
        g3,
        g3_star,
    )
