"""Formats to tranform the data into another format.

Functions:
    | *numpy_array()* converts an array-like data to np.ndarray
    | *array_to_dict()* converts an array to a dictionary where the keys are the unique values of
    |   the array and the values of the dictionary are the counts of the array values.
    | *dataframe_to_array()* returns a pandas dataframe from an np.ndarray.
"""

from __future__ import annotations

from typing import Any, Optional, Union

import numpy as np
import pandas as pd
import polars as pl

from samplics.utils.checks import assert_not_unique
from samplics.utils.types import (
    Array,
    DictStrInt,
    DictStrNum,
    Number,
    Series,
    StringNumber,
)


def numpy_array(arr: Array) -> np.ndarray:
    if isinstance(arr, (pd.DataFrame, pl.DataFrame)):
        return arr.to_numpy()
    elif isinstance(arr, (pd.Series, pl.Series)):
        return arr.to_numpy()
    elif not isinstance(arr, np.ndarray):
        arr_np = np.asarray(arr)
        if isinstance(arr, (list, tuple)) and len(arr_np.shape) == 2:
            arr_np = np.transpose(arr_np)
        return arr_np
    else:
        return arr


def array_to_dict(arr: np.ndarray, domain: Optional[np.ndarray] = None) -> DictStrNum:
    if domain is None:
        keys, counts = np.unique(numpy_array(arr), return_counts=True)
        out_dict = dict(zip(keys, counts))
    else:
        out_dict = {}
        for d in np.unique(domain):
            arr_d = arr[domain == d]
            keys_d, counts_d = np.unique(numpy_array(arr_d), return_counts=True)
            out_dict[d] = dict(zip(keys_d, counts_d))

    return out_dict


def dataframe_to_array(df: pd.DataFrame) -> np.ndarray:
    if isinstance(df, (pd.Series, pl.Series)):
        return df.to_numpy()
    elif isinstance(df, (pl.DataFrame, pd.DataFrame)):
        if isinstance(df, pd.DataFrame):
            df = pl.from_pandas(df)
        nb_vars = df.shape[1]
        col_names = df.columns

        x_array = df.select(col_names[0]).to_series().cast(pl.Utf8).to_numpy()
        if nb_vars > 1:
            for k in range(1, nb_vars):
                x_array = (
                    x_array + "_&_" + df.select(col_names[k]).to_series().cast(pl.Utf8).to_numpy()
                )
        return x_array
    else:
        raise AssertionError("The input data is not a pandas dataframe")


def data_to_dict(
    data: Union[DictStrInt, int],
    strat: bool,
    stratum: Array,
) -> Union[DictStrInt, int]:
    if not isinstance(data, dict) and strat:
        return dict(zip(stratum, np.repeat(data, len(stratum))))
    if isinstance(data, (int, float)) and not strat:
        return data
    elif isinstance(data, dict):
        return data
    else:
        raise AssertionError


def sample_units(all_units: Array, unique: bool = True) -> np.ndarray:
    all_units = numpy_array(all_units)
    if unique:
        assert_not_unique(all_units)

    return all_units


def dict_to_dataframe(col_names: list[str], *args: Any) -> pd.DataFrame:
    if isinstance(args[0], dict):
        values_df = pd.DataFrame(columns=col_names)
        keys = list(args[0].keys())
        number_keys = len(keys)
        for k, arg in enumerate(args):
            args_keys = list(args[k].keys())
            if not isinstance(arg, dict) or (keys != args_keys) or number_keys != len(args_keys):
                raise AssertionError(
                    "All input parameters must be dictionaries with the same keys."
                )

            if isinstance(
                args[0].get(keys[0]), dict
            ):  # For the case of nested dictionaries e.g. proportion or as_factor=True
                keys_list = list()
                levels = list()
                values = list()
                for key in keys:
                    keys_list += np.repeat(key, len(list(arg[key].keys()))).tolist()
                    levels += list(arg[key].keys())
                    values += list(arg[key].values())
                if k == 0:
                    values_df.iloc[:, 1] = keys_list
                    values_df.iloc[:, 2] = levels
                values_df[col_names[k + 3]] = values
            else:
                if k == 0:
                    values_df.iloc[:, 1] = keys
                values_df[col_names[k + 2]] = arg.values()
    else:
        values_df = pd.DataFrame({args})
        values_df.insert(0, "_parameter", " ")
        values_df.columns = col_names

    return values_df


# def remove_nans(excluded_units: Array, *args: Any) -> list:

#     excluded_units = numpy_array(excluded_units)
#     vars_list = list()
#     for var in args:
#         if var is not None and len(var.shape) != 0:
#             vars_list.append(var[~excluded_units])
#         else:
#             vars_list.append(None)

#     return vars_list


def remove_nans(n: Number, *args: np.ndarray) -> list:
    excluded_units = np.zeros(n).astype(bool)

    for var in args:
        if var.shape not in ((), (0,)):
            assert n == var.shape[0]
            if var.dtype.kind in ("i", "u", "f", "c", "b", "m", "M"):
                excluded_units = excluded_units | np.isnan(var)
            else:
                var = var.astype(str)
                excluded_units = excluded_units | np.isin(var, ("nan", ""))

    return ~excluded_units


def fpc_as_dict(stratum: np.ndarray, fpc: Union[Array, Number]) -> Union[DictStrNum, Number]:
    if stratum.shape in ((), (0,)) and isinstance(fpc, (int, float)):
        return fpc
    elif stratum.shape not in ((), (0,)) and isinstance(fpc, (int, float)):
        return dict(zip(stratum, np.repeat(fpc, stratum.shape[0])))
    elif stratum.shape not in ((), (0,)) and isinstance(fpc, np.ndarray):
        return dict(zip(stratum, fpc))
    else:
        raise TypeError("stratum and fpc are not compatible!")


def convert_numbers_to_dicts(
    number_strata: Optional[int], *args: Union[DictStrNum, Number]
) -> list[DictStrNum]:
    dict_number = 0
    stratum: Optional[list[StringNumber]] = None
    for arg in args:
        if arg is not None and not isinstance(arg, (int, float, dict)):
            raise TypeError("Arguments must be of type int, float or dict!")

        if isinstance(arg, dict):
            dict_number += 1
            if dict_number == 1:
                stratum = list(arg.keys())
            elif dict_number >= 1:
                if stratum != list(arg.keys()):
                    raise AssertionError("Python dictionaries have different keys")

    if stratum is None:
        if isinstance(number_strata, (int, float)) and number_strata >= 1:
            stratum = ["_stratum_" + str(i) for i in range(1, number_strata + 1)]
        else:
            raise ValueError("Number of strata must be superior or equal to 1!")
    else:
        number_strata = len(stratum)

    list_of_dicts = list()
    for arg in args:
        if isinstance(arg, (int, float)):
            list_of_dicts.append(dict(zip(stratum, np.repeat(arg, number_strata))))
        else:
            list_of_dicts.append(arg)

    return list_of_dicts


def concatenate_series_to_str(row: Series) -> str:
    return "__by__".join([str(c) for c in row])


def numpy_to_dummies(arr: np.ndarray, vars_names: list[str]) -> np.ndarray:
    df = pd.DataFrame(arr.astype(str))
    df.columns = vars_names

    return np.asarray(pd.get_dummies(df, drop_first=True).to_numpy())
