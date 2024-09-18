from __future__ import annotations

from typing import Optional, Union

import numpy as np

# import pandas as pd
import statsmodels.api as sm

from samplics.utils.formats import (
    fpc_as_dict,
    numpy_array,
)
from samplics.utils.types import Array, Number, Series, StringNumber


class SurveyGLM:
    """General linear models under complex survey sampling"""

    def __init__(self):
        self.beta: np.ndarray

    @staticmethod
    def _residuals(
        e: np.ndarray, psu: np.ndarray, nb_vars: Number
    ) -> tuple(np.ndarray, Number):

        psus = np.unique(psu)
        if psus.shape[0] == 1 and e.shape[0] == 1:
            raise AssertionError("Only one observation in the stratum")
        if psus.shape[0] == 1:
            psu = np.arange(e.shape[0])
            psus = np.unique(psu)
        e_values = np.zeros((psus.shape[0], nb_vars))

        for i, p in enumerate(np.unique(psus)):
            e_values[i, :] += np.sum(e[psu == p, :], axis=0)
        e_means = np.sum(e_values, axis=0) / psus.shape[0]

        return np.transpose(e_values - e_means) @ (e_values - e_means), psus.shape[0]

    def _calculate_g(
        self,
        samp_weight: np.ndarray,
        resid: np.ndarray,
        x: np.ndarray,
        stratum: np.ndarray,
        psu: np.ndarray,
        fpc: Union[dict[StringNumber, Number], Number],
        glm_scale=Number,
    ) -> np.ndarray:

        e = (samp_weight * resid)[:, None] * x / glm_scale
        if psu.shape in ((), (0,)):
            psu = np.arange(e.shape[0])
        if stratum.shape in ((), (0,)):
            e_h, n_h = self._residuals(e=e, psu=psu, nb_vars=x.shape[1])
            return fpc * (n_h / (n_h - 1)) * e_h
        else:
            g_h = np.zeros((x.shape[1], x.shape[1]))
            for s in np.unique(stratum):
                e_s = e[stratum == s, :]
                psu_s = psu[stratum == s]
                e_h, n_h = self._residuals(e=e_s, psu=psu_s, nb_vars=x.shape[1])
                g_h += fpc[s] * (n_h / (n_h - 1)) * e_h
            return g_h

    def estimate(
        self,
        y: Array,
        x: Optional[Array] = None,
        samp_weight: Optional[Array] = None,
        stratum: Optional[Series] = None,
        psu: Optional[Series] = None,
        fpc: Union[dict[StringNumber, Number], Series, Number] = 1.0,
        remove_nan: bool = False,
    ) -> None:

        _y = numpy_array(y)
        _x = numpy_array(x)
        _psu = numpy_array(psu)
        _stratum = numpy_array(stratum)
        _samp_weight = numpy_array(samp_weight)

        if _samp_weight.shape in ((), (0,)):
            _samp_weight = np.ones(y.shape[0])
        if _samp_weight.shape[0] == 1:
            _samp_weight = _samp_weight * np.ones(_y.shape[0])

        if not isinstance(fpc, dict):
            self.fpc = fpc_as_dict(_stratum, fpc)
        else:
            if np.unique(_stratum).tolist() != list(fpc.keys()):
                raise AssertionError(
                    "fpc dictionary keys must be the same as the strata!"
                )
            else:
                self.fpc = fpc

        glm_model = sm.GLM(endog=_y, exog=_x, var_weights=_samp_weight)
        glm_results = glm_model.fit()

        g = self._calculate_g(
            samp_weight=_samp_weight,
            resid=glm_results.resid_response,
            x=_x,
            stratum=_stratum,
            psu=_psu,
            fpc=self.fpc,
            glm_scale=glm_results.scale,
        )

        d = glm_results.cov_params()

        self.beta = glm_results.params
        self.cov_beta = (d @ g) @ d
