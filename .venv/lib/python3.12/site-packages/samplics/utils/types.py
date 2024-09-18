"""Provides the custom types used throughout the modules.
"""

from enum import Enum, unique
from typing import Dict

import numpy as np
import pandas as pd
import polars as pl


DF = pl.DataFrame | pd.DataFrame
Array = np.ndarray | pd.Series | pl.Series | list | tuple
Series = pd.Series | pl.Series | list | tuple

Number = float | int
StringNumber = str | float | int

DictStrNum = Dict[StringNumber, Number]
DictStrInt = Dict[StringNumber, int]
DictStrFloat = Dict[StringNumber, float]
DictStrBool = Dict[StringNumber, bool]



@unique
class FitMethod(Enum):
    ols= "OLS"
    wls= "WLS"
    fh = "FH"
    ml= "ML"
    reml= "REML"

# Population parameters
@unique
class PopParam(Enum):
    count = "Count"
    mean = "Mean"
    total = "Total"
    prop = "Proportion"
    ratio = "Ratio"


@unique
class RepMethod(Enum):
    jackknife  = "Jackknife"
    bootstrap = "Bootstrap"
    brr = "BRR"
    

# Methods for sample size
@unique
class SizeMethod(Enum):
    wald = "Wald"
    fleiss = "Fleiss"


@unique
class SelectMethod(Enum):
    srs_wr = "SRS with replacement"
    srs_wor = "SRS without replacement"
    sys = "Systematic"
    pps_brewer = "PPS Brewer"
    pps_hv = "PPS Hanurav-Vijayan"
    pps_murphy = "PPS Murphy"
    pps_rs = "PPS Rao-Sampford"
    pps_sys = "PPS Systematic"
    pps_wr = "PPS with replacement"
    grs = "General"


@unique
class SinglePSUEst(Enum):
    """Estimation options for strata with singleton PSU"""

    error = "Raise Error when one PSU in a stratum"
    skip = "Set variance to zero and skip stratum with one PSU"
    certainty = "Use SSUs or lowest units to estimate the variance"
    combine = "Combine the strata with the singleton psu to another stratum"
