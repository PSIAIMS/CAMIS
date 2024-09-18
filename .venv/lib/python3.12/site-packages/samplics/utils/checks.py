"""Runs checks on the data to help capture some errors faster and provides better error messages.

Functions:
    | *assert_probabilities()* ensures that probability values are between 0 and 1.
    | *assert_weights()* ensures that sample weights are not negatives.
    | *assert_not_unique()* return an assertion error if the array has non unique values.
    | *assert_response_status()* checks that the response values are in ("in", "rr", "nr", "uk").
    | *assert_brr_number_psus()* checks that the number of psus is a multiple of 2.
"""
from __future__ import annotations

from typing import Iterable, Optional, Union

import numpy as np
import pandas as pd

from samplics.utils import formats
from samplics.utils.types import Array, Number, StringNumber


def assert_probabilities(**kargs: Union[Number, Iterable]) -> None:

    err_msg = "Probabilities must be between 0 and 1, inclusively!"
    for k in kargs:
        if not assert_in_range(0, 1, x=kargs[k]):
            raise ValueError(err_msg)


def assert_proportions(**kargs: Union[Number, Iterable]) -> None:

    err_msg = "Proportions must be between 0 and 1, inclusively!"
    for k in kargs:
        if not assert_in_range(0, 1, x=kargs[k]):
            raise ValueError(err_msg)


def assert_in_range(low: Number, high: Number, x: Union[Number, Iterable]) -> bool:
    if isinstance(x, (int, float)):
        if x > high or x < low:
            return False
    elif isinstance(x, (np.ndarray, pd.Series)):
        if (x > high).any() or (x < low).any():
            return False
    elif isinstance(x, Iterable):
        for i in x:
            if isinstance(x, dict):
                if x[i] > high or x[i] < low:
                    return False
            elif i > high or i < low:
                return False

    return True


def assert_weights(weights: Array) -> None:
    weights = formats.numpy_array(weights)
    if (weights < 0).any():
        raise ValueError("Sample weights must be positive values")


def assert_not_unique(array_unique_values: Array) -> None:
    if np.unique(array_unique_values).size != len(array_unique_values):
        raise AssertionError(
            "The array must contains unique values identifying all the units in the sampling frame."
        )


def assert_response_status(
    response_status: Union[str, np.ndarray],
    response_dict: Optional[dict[str, StringNumber]],
) -> None:
    if response_status is None:
        raise AssertionError("response_status is not provided")
    elif (
        not np.isin(response_status, ("in", "rr", "nr", "uk")).all()
        and response_dict is None
    ):
        raise AssertionError(
            "The response status must only contains values in ('in', 'rr', 'nr', 'uk') or the mapping should be provided using response_dict parameter"
        )
    elif "rr" not in response_status and response_dict is None:
        raise AssertionError("The response status must at least contains rr!")
    elif isinstance(response_dict, dict):
        # resp_keys = list(response_dict.keys())
        resp_keys = [x.lower() for x in response_dict]
        if not np.isin(resp_keys, ("in", "rr", "nr", "uk")).all():
            raise AssertionError("Response mapping dictionnary has unexpected value(s)")


def assert_brr_number_psus(psu: np.ndarray) -> None:
    if psu.size % 2 != 0:
        raise AssertionError(
            "For the BRR method, the number of PSUs must be a multiple of two."
        )
