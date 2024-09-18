from __future__ import annotations

from typing import Optional, Union

from samplics.utils.types import Array, Number, Series, StringNumber


# import numpy as np
# import pandas as pd


class CalibrateEstimator:
    """Provides calibration based estimation"""

    def __init__(
        self, param: str, alpha: float = 0.05, rand_seed: Optional[int] = None
    ):
        pass

    def estimate(
        self,
        y: Array,
        calib_weight: Array,
        # calib_rep_weight: Optional[Array] = None,
        x: Optional[Array] = None,
        stratum: Optional[Series] = None,
        psu: Optional[Series] = None,
        ssu: Optional[Series] = None,
        domain: Optional[Series] = None,
        by: Optional[Series] = None,
        fpc: Union[dict[StringNumber, Number], Series, Number] = 1.0,
        deff: bool = False,
        coef_variation: bool = False,
        as_factor: bool = False,
        remove_nan: bool = False,
    ) -> None:
        pass
