from typing import Dict, TypeVar

import numpy as np
import pandas as pd
import polars as pl


# NumpyArray = TypeVar("np.ndarray", bound=np.ndarray)
# PandasDF = TypeVar("pd.DataFrame", bound=pd.DataFrame)
# PolarsDF = TypeVar("pl.DataFrame", bound=pl.DataFrame)


DF = pl.DataFrame | pd.DataFrame
Array = np.ndarray | pd.Series | pl.Series | list | tuple
Series = pd.Series | pl.Series | list | tuple

Number = float | int
StringNumber = str | float | int

DictStrNum = Dict[StringNumber, Number]
DictStrInt = Dict[StringNumber, int]
DictStrFloat = Dict[StringNumber, float]
DictStrBool = Dict[StringNumber, bool]


# TypeVar definitions

DataFrame = TypeVar("DataFrame", bound=pd.DataFrame | pl.DataFrame)
