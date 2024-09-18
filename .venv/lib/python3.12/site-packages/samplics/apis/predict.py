from samplics.apis.sae.area_eblup import _predict_eblup
from samplics.types import AuxVars, DictStrNum, DirectEst, FitStats, Number


def predict(
    x: AuxVars,
    fit_stats: FitStats,
    y: DirectEst,
    intercept: bool = True,  # if True, it adds an intercept of 1
    b_const: DictStrNum | Number = 1.0,
):
    return _predict_eblup(x=x, fit_eblup=fit_stats, y=y, intercept=intercept, b_const=b_const)
