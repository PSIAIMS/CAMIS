# """EBLUP Area Model

# """

# import math

# import numpy as np
# import polars as pl

# from samplics.types import AuxVars, DepVarsPrcl, FitMethod, FitStats, Number


# # Fitting a EBLUP model
# def _fit(
#     y: DepVarsPrcl,
#     x: AuxVars,
#     method: FitMethod,
#     intercept: bool = True,  # if True, it adds an intercept of 1
#     err_init: float | None = None,
#     # b_const: Array | Number = 1.0, #TODO: move it to the y (add it in DepVars, etc.)
#     tol: float = 1e-4,
#     maxiter: int = 100,
# ) -> FitStats:
#     # TODO: add checks that area is the same in y and x

#     if err_init is None:
#         err_init = np.median(y.to_numpy(keep_vars="stderr").flatten())

#     (
#         sig2_v,
#         sig2_v_cov,
#         iterations,
#         tolerance,
#         convergence,
#     ) = _iterative_fisher_scoring(
#         method=method,
#         # area=area,
#         y=y,
#         x=x,
#         sig_e=y.stderr,
#         # b_const=b_const,
#         intercept=intercept,
#         sig_v_start=err_init,
#         tol=tol,
#         maxiter=maxiter,
#     )

#     beta, beta_cov = _fixed_coefficients(
#         # area=area,
#         y=y,
#         x=x,
#         sig2_e=y.stderr,
#         sig2_v=sig2_v,
#         intercept=intercept
#         # b_const=b_const,
#     )

#     m, p = x.to_numpy(drop_vars=["__drop_id", "__domain"]).shape

#     log_llike = _log_likelihood(
#         method=method,
#         y=y,
#         x=x,
#         beta=beta,
#         sig2_e=y.stderr,
#         sig2_v=sig2_v,
#         intercept=intercept,
#     )

#     return FitStats(
#         method=method,
#         err_stderr=y.stderr,
#         fe_est=beta.tolist(),
#         fe_stderr=(np.diag(beta_cov) ** (1 / 2)).tolist(),
#         re_stderr=sig2_v ** (1 / 2),
#         re_stderr_var=sig2_v_cov,
#         log_llike=log_llike,
#         convergence={
#             "achieved": convergence,
#             "iterations": iterations,
#             "precision": tolerance,
#         },
#         goodness={
#             "AIC": -2 * log_llike + 2 * (p + 1),
#             "BIC": -2 * log_llike + math.log(m) * (p + 1),
#             "KIC": -2 * log_llike + 3 * (p + 1),
#         },
#     )


# def _iterative_fisher_scoring(
#     method: FitMethod,
#     # area: np.ndarray,
#     y: DepVarsPrcl,
#     x: AuxVars,
#     sig_e: dict,
#     # b_const: np.ndarray,
#     intercept: bool,
#     sig_v_start: float,
#     tol: float,
#     maxiter: int,
# ) -> tuple[float, float, int, float, bool]:  # May not need variance
#     """Fisher-scroring algorithm for estimation of variance component
#     return (sigma, covariance, number_iterations, tolerance, covergence status)"""

#     iterations = 0
#     tolerance = tol + 1.0
#     sig2_v_prev = sig_v_start**2
#     sig2_e = {d: sig_e[d] ** 2 for d in sig_e}

#     info_sigma = 0.0
#     beta, beta_cov = None, None
#     while iterations < maxiter and tolerance > tol:
#         if method == FitMethod.ml or method == FitMethod.fh:
#             beta, beta_cov = _fixed_coefficients(
#                 y=y,
#                 x=x,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v_prev,
#                 intercept=intercept
#                 # b_const=b_const,
#             )

#         deriv_sigma, info_sigma = _partial_derivatives(
#             method=method,
#             y=y,
#             x=x,
#             sig2_e=sig2_e,
#             sig2_v=sig2_v_prev,
#             intercept=intercept,
#             beta=beta,
#         )
#         sig2_v = sig2_v_prev + deriv_sigma / info_sigma
#         tolerance = abs((sig2_v - sig2_v_prev) / sig2_v_prev)
#         iterations += 1
#         sig2_v_prev = sig2_v

#     return (
#         float(max(sig2_v, 0)),
#         1 / info_sigma,
#         iterations,
#         tolerance,
#         tolerance <= tol,
#     )


# def _partial_derivatives_fh(
#     method: FitMethod,
#     y: DepVarsPrcl,
#     x: AuxVars,
#     sig2_e: dict,
#     sig2_v: Number,
#     intercept: bool,
#     beta: np.ndarray,
# ) -> tuple[Number, Number]:
#     x = x.to_polars().drop(["__record_id"])

#     if intercept:
#         x0 = pl.Series("__intercept", np.ones(x.shape[0]))
#         x = x.insert_at_idx(0, x0)

#     m, p = x.shape
#     p = p - 1  # x has an extra column

#     deriv_sig = 0.0
#     info_sig = 0.0

#     for d in y.domains:
#         b_d = 1  # b_const[d][0]
#         x_d = x.filter(pl.col("__domain") == d).drop("__domain").to_numpy()
#         y_d = y.est[d]
#         resid_d = y_d - x_d @ beta
#         sig2_d = sig2_v * (b_d**2) + sig2_e[d]
#         deriv_sig += float((resid_d**2) / sig2_d)
#         info_sig += -float(((b_d**2) * (resid_d**2)) / (sig2_d**2))
#     deriv_sig = m - p - deriv_sig

#     return deriv_sig, info_sig


# def _partial_derivatives_ml(
#     method: FitMethod,
#     y: DepVarsPrcl,
#     x: AuxVars,
#     sig2_e: dict,
#     sig2_v: Number,
#     intercept: bool,
#     beta: np.ndarray,
# ) -> tuple[Number, Number]:
#     x = x.to_polars().drop(["__record_id"])

#     if intercept:
#         x0 = pl.Series("__intercept", np.ones(x.shape[0]))
#         x = x.insert_at_idx(0, x0)

#     m, p = x.shape
#     p = p - 1  # x has an extra column

#     deriv_sig = 0.0
#     info_sig = 0.0

#     for d in y.domains:
#         b_d = 1  # b_const[d][0]
#         x_d = x.filter(pl.col("__domain") == d).drop("__domain").to_numpy()
#         y_d = y.est[d]
#         resid_d = y_d - (x_d @ beta)[0]
#         sig2_d = sig2_v * (b_d**2) + sig2_e[d]
#         term1 = b_d**2 / sig2_d
#         info_sig += term1**2
#         term2 = ((b_d**2) * (resid_d**2)) / (sig2_d**2)
#         deriv_sig += term1 - term2
#     deriv_sig = -0.5 * deriv_sig
#     info_sig = 0.5 * info_sig

#     return deriv_sig, info_sig


# def _partial_derivatives_reml(
#     method: FitMethod,
#     y: DepVarsPrcl,
#     x: AuxVars,
#     sig2_e: dict,
#     sig2_v: Number,
#     intercept: bool,
#     beta: np.ndarray,
# ) -> tuple[Number, Number]:
#     x = x.to_polars().drop(["__record_id"])

#     if intercept:
#         x0 = pl.Series("__intercept", np.ones(x.shape[0]))
#         x = x.insert_at_idx(0, x0)

#     m, p = x.shape
#     p = p - 1  # x has an extra column

#     deriv_sig = 0.0
#     info_sig = 0.0

#     b_const = 1
#     B = np.diag(np.ones(x.shape[0]))
#     x_mat = x.drop("__domain").to_numpy()
#     sig2_e_vec = np.array(list(sig2_e.values()))
#     v_inv = np.diag(1 / (sig2_e_vec + sig2_v * (b_const**2)))
#     x_vinv_x = np.transpose(x_mat) @ v_inv @ x_mat
#     x_xvinvx_x = x_mat @ np.linalg.inv(x_vinv_x) @ np.transpose(x_mat)
#     P = v_inv - v_inv @ x_xvinvx_x @ v_inv
#     Py = P @ y.to_numpy(keep_vars="est").flatten()
#     PB = P @ B
#     term1 = -0.5 * np.trace(PB)
#     term2 = 0.5 * (np.transpose(Py) @ B @ Py)
#     deriv_sig = term1 + term2
#     info_sig = 0.5 * np.trace(PB @ PB)

#     return deriv_sig, info_sig


# def _partial_derivatives(
#     method: FitMethod,
#     y: DepVarsPrcl,
#     x: AuxVars,
#     sig2_e: dict,
#     sig2_v: Number,
#     intercept: bool,
#     beta: np.ndarray,
# ) -> tuple[Number, Number]:
#     match method:
#         case FitMethod.fh:
#             return _partial_derivatives_fh(
#                 method=method,
#                 y=y,
#                 x=x,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#                 beta=beta,
#             )
#         case FitMethod.ml:
#             return _partial_derivatives_ml(
#                 method=method,
#                 y=y,
#                 x=x,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#                 beta=beta,
#             )
#         case FitMethod.reml:
#             return _partial_derivatives_reml(
#                 method=method,
#                 y=y,
#                 x=x,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#                 beta=beta,
#             )
#         case _:
#             raise ValueError("Fitting method not available")


# def _fixed_coefficients(
#     y: dict,
#     x: dict,
#     sig2_e: dict,
#     sig2_v: float,
#     intercept: bool,
# ) -> tuple[np.ndarray, np.ndarray]:
#     y_vec = y.to_numpy(keep_vars="est").flatten()
#     if intercept:
#         x_mat = np.insert(x.to_numpy(drop_vars=["__record_id", "__domain"]), 0, 1, axis=1)  # add the intercept
#     else:
#         x_mat = x.to_numpy(drop_vars=["__record_id", "__domain"])

#     b_const = 1
#     sig2_e_vec = np.array(list(sig2_e.values()))
#     v_inv = np.diag(1 / (sig2_v * (b_const**2) + sig2_e_vec))
#     x_v_X_inv = np.linalg.pinv(np.transpose(x_mat) @ v_inv @ x_mat)
#     x_v_x_inv_x = x_v_X_inv @ (np.transpose(x_mat) @ v_inv)
#     beta_hat = x_v_x_inv_x @ y_vec
#     beta_cov = np.transpose(x_mat) @ v_inv @ x_mat

#     return beta_hat.ravel(), np.linalg.inv(beta_cov)


# def _log_likelihood_fh(
#     method: FitMethod,
#     y: np.ndarray,
#     x: np.ndarray,
#     beta: np.ndarray,
#     sig2_e: dict,
#     sig2_v: float,
#     intercept: bool,
# ) -> Number:
#     NotImplementedError


# def _log_likelihood_ml(
#     method: FitMethod,
#     y: np.ndarray,
#     x: np.ndarray,
#     beta: np.ndarray,
#     sig2_e: dict,
#     sig2_v: float,
#     intercept: bool,
# ) -> Number:
#     m = len(y.est.keys())
#     const = m * np.log(2 * np.pi)
#     ll_term1 = np.log(np.array(list(sig2_e.values())) + sig2_v).sum()
#     ll_term2 = 0

#     x = x.to_polars().drop(["__record_id"])
#     if intercept:
#         x0 = pl.Series("__intercept", np.ones(m))
#         x = x.insert_at_idx(0, x0)

#     for d in y.domains:
#         x_d = x.filter(pl.col("__domain") == d).drop("__domain").to_numpy()
#         ll_term2 += ((y.est[d] - x_d @ beta)[0] ** 2) / (sig2_e[d] + sig2_v)

#     return -0.5 * (const + ll_term1 + ll_term2)


# def _log_likelihood_reml(
#     method: FitMethod,
#     y: np.ndarray,
#     x: np.ndarray,
#     beta: np.ndarray,
#     sig2_e: dict,
#     sig2_v: float,
#     intercept: bool,
# ) -> Number:
#     b_const = 1
#     sig2_e_vec = np.array(list(sig2_e.values()))
#     np.diag(sig2_v * (b_const**2) + sig2_e_vec)
#     v_inv = np.diag(1 / (sig2_v * (b_const**2) + sig2_e_vec))
#     x_mat = x.to_polars(drop_vars=["__record_id", "__domain"])
#     if intercept:
#         x0 = pl.Series("__intercept", np.ones(x_mat.shape[0]))
#         x_mat = x_mat.insert_at_idx(0, x0).to_numpy()

#     return (
#         _log_likelihood_ml(
#             method=method,
#             y=y,
#             x=x,
#             beta=beta,
#             sig2_e=sig2_e,
#             sig2_v=sig2_v,
#             intercept=intercept,
#         )
#     ) + -0.5 * np.log(np.linalg.det(np.transpose(x_mat) @ v_inv @ x_mat))


# def _log_likelihood(
#     method: FitMethod,
#     y: np.ndarray,
#     x: np.ndarray,
#     beta: np.ndarray,
#     sig2_e: dict,
#     sig2_v: float,
#     intercept: bool,
# ) -> Number:
#     match method:
#         case FitMethod.fh:
#             loglike = _log_likelihood_fh(
#                 method=method,
#                 y=y,
#                 x=x,
#                 beta=beta,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#             )
#         case FitMethod.ml:
#             loglike = _log_likelihood_ml(
#                 method=method,
#                 y=y,
#                 x=x,
#                 beta=beta,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#             )
#         case FitMethod.reml:
#             loglike = _log_likelihood_reml(
#                 method=method,
#                 y=y,
#                 x=x,
#                 beta=beta,
#                 sig2_e=sig2_e,
#                 sig2_v=sig2_v,
#                 intercept=intercept,
#             )
#         case _:
#             ValueError("Must choose a valid fitting method")

#     return loglike
