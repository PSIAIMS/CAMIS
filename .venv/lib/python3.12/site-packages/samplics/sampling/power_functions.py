"""Sample size calculation module
This module compiles the core functions for calculating power and sample size
"""

from __future__ import annotations

import math

from typing import Optional, Union

import numpy as np
import pandas as pd

from scipy.stats import norm as normal

from samplics.utils.checks import assert_proportions
from samplics.utils.formats import numpy_array
from samplics.utils.types import Array, DictStrNum, Number


def calculate_power_prop(
    two_sides: bool,
    prop_0: Union[DictStrNum, Number, Array],
    prop_1: Union[DictStrNum, Number, Array],
    samp_size: Union[DictStrNum, Number, Array],
    arcsin: bool = True,
    alpha: float = 0.05,
):

    z_value = normal().ppf(1 - alpha / 2)

    if (
        isinstance(prop_0, dict)
        and isinstance(prop_1, dict)
        and isinstance(samp_size, dict)
    ):
        if two_sides:
            powerr: dict = {}
            for s in prop_0:
                z = (prop_1[s] - prop_0[s]) / math.sqrt(
                    prop_1[s] * (1 - prop_1[s]) / samp_size[s]
                )
                powerr[s] = normal().cdf(z - z_value) + normal().cdf(-z - z_value)
        else:
            powerr: dict = {}
            for s in prop_0:
                z = (prop_1[s] - prop_0[s]) / math.sqrt(
                    prop_1[s] * (1 - prop_1[s]) / samp_size[s]
                )
                powerr[s] = normal().cdf(z - z_value) + normal().cdf(-z - z_value)
    elif (
        isinstance(prop_0, (int, float))
        and isinstance(prop_1, (int, float))
        and isinstance(samp_size, (int, float))
    ):
        if arcsin:
            h = 2 * math.asin(math.sqrt(prop_1)) - 2 * math.asin(math.sqrt(prop_0))
            if two_sides:
                return (
                    1
                    - normal().cdf(z_value - h * math.sqrt(samp_size))
                    + normal().cdf(-z_value - h * math.sqrt(samp_size))
                )
            else:
                return (
                    1
                    - normal().cdf(z_value - h * math.sqrt(samp_size))
                    # + normal().cdf(-z_value - h * math.sqrt(samp_size))
                )
        else:
            if two_sides:
                z = (prop_1 - prop_0) / math.sqrt(prop_1 * (1 - prop_1) / samp_size)
                return normal().cdf(z - z_value) + normal().cdf(-z - z_value)
            else:
                z = (prop_1 - prop_0) / math.sqrt(prop_1 * (1 - prop_1) / samp_size)
                return normal().cdf(z - z_value) + normal().cdf(-z - z_value)
    elif (
        isinstance(prop_0, (np.ndarray, pd.Series, list, tuple))
        and isinstance(prop_1, (np.ndarray, pd.Series, list, tuple))
        and isinstance(samp_size, (np.ndarray, pd.Series, list, tuple))
    ):
        prop_0 = numpy_array(prop_0)
        prop_1 = numpy_array(prop_1)
        samp_size = numpy_array(samp_size)

        if two_sides:
            z = (prop_1 - prop_0) / np.sqrt(prop_1 * (1 - prop_1) / samp_size)
            return normal().cdf(z - z_value) + normal().cdf(-z - z_value)
        else:
            z = (prop_1 - prop_0) / np.sqrt(prop_1 * (1 - prop_1) / samp_size)
            return normal().cdf(z - z_value) + normal().cdf(-z - z_value)


def calculate_power(
    two_sides: bool,
    delta: Union[DictStrNum, Number, Array],
    sigma: Union[DictStrNum, Number, Array],
    samp_size: Union[DictStrNum, Number, Array],
    alpha: float,
):

    if (
        isinstance(delta, dict)
        and isinstance(sigma, dict)
        and isinstance(samp_size, dict)
    ):
        if two_sides:
            return {
                s: 1
                - normal().cdf(
                    normal().ppf(1 - alpha / 2)
                    - delta[s] / (sigma[s] / math.sqrt(samp_size[s]))
                )
                + normal().cdf(
                    -normal().ppf(1 - alpha / 2)
                    - delta[s] / (sigma[s] / math.sqrt(samp_size[s]))
                )
                for s in delta
            }
        else:
            return 1 - normal().cdf(
                normal().ppf(1 - alpha) - delta / (sigma / math.sqrt(samp_size))
            )
    elif (
        isinstance(delta, (int, float))
        and isinstance(sigma, (int, float))
        and isinstance(samp_size, (int, float))
    ):
        if two_sides:
            return (
                1
                - normal().cdf(
                    normal().ppf(1 - alpha / 2) - delta / (sigma / math.sqrt(samp_size))
                )
                + normal().cdf(
                    -normal().ppf(1 - alpha / 2)
                    - delta / (sigma / math.sqrt(samp_size))
                )
            )
        else:
            return 1 - normal().cdf(
                normal().ppf(1 - alpha) - delta / (sigma / math.sqrt(samp_size))
            )
    elif (
        isinstance(delta, (np.ndarray, pd.Series, list, tuple))
        and isinstance(sigma, (np.ndarray, pd.Series, list, tuple))
        and isinstance(samp_size, (np.ndarray, pd.Series, list, tuple))
    ):
        delta = numpy_array(delta)
        sigma = numpy_array(sigma)
        power = np.zeros(delta.shape[0])
        for k in range(delta.shape[0]):
            if two_sides:
                power[k] = (
                    1
                    - normal().cdf(
                        normal().ppf(1 - alpha / 2)
                        - delta[k] / (sigma[k] / math.sqrt(samp_size[k]))
                    )
                    + normal().cdf(
                        -normal().ppf(1 - alpha / 2)
                        - delta[k] / (sigma[k] / math.sqrt(samp_size[k]))
                    )
                )
            else:
                power[k] = 1 - normal().cdf(
                    normal().ppf(1 - alpha)
                    - delta[k] / (sigma[k] / math.sqrt(samp_size[k]))
                )
            return power


def power_for_one_proportion(
    prop_0: Union[DictStrNum, Number, Array],
    prop_1: Union[DictStrNum, Number, Array],
    samp_size: Union[DictStrNum, Number, Array],
    arcsin: bool = True,
    testing_type: str = "two-sided",
    alpha: Union[Number, Array] = 0.05,
) -> Union[DictStrNum, Number, Array]:

    type = testing_type.lower()
    if type not in ("two-sided", "less", "greater"):
        raise AssertionError("type must be 'two-sided', 'less', 'greater'.")

    assert_proportions(prop_0=prop_0, prop_1=prop_1, alpha=alpha)

    if isinstance(alpha, (int, float)):
        z_value = (
            normal().ppf(1 - alpha / 2)
            if type == "two-sided"
            else normal().ppf(1 - alpha)
        )
    if isinstance(alpha, (np.ndarray, pd.Series, list, tuple)):
        alpha = numpy_array(alpha)
        z_value = (
            normal().ppf(1 - alpha / 2)
            if type == "two-sided"
            else normal().ppf(1 - alpha)
        )

    if (
        isinstance(prop_0, dict)
        and isinstance(prop_1, dict)
        and isinstance(samp_size, dict)
    ):
        power: dict = {}
        for s in prop_0:
            if arcsin:
                z = (
                    2 * math.asin(math.sqrt(prop_1[s]))
                    - 2 * math.asin(math.sqrt(prop_0[s]))
                ) * math.sqrt(samp_size[s])
            else:
                z = (prop_1[s] - prop_0[s]) / math.sqrt(
                    prop_1[s] * (1 - prop_1[s]) / samp_size[s]
                )

            if isinstance(alpha, dict):
                z_value = (
                    normal().ppf(1 - alpha[s] / 2)
                    if type == "two-sided"
                    else normal().ppf(1 - alpha[s])
                )

            if type == "two-sided":
                power[s] = normal().cdf(abs(z) - z_value)
            elif type == "greater":
                power[s] = normal().cdf(z - z_value)
            else:  # type == "less":
                power[s] = normal().cdf(-z - z_value)
    elif (
        isinstance(prop_0, (int, float))
        and isinstance(prop_1, (int, float))
        and isinstance(samp_size, (int, float))
    ):
        if arcsin:
            z = (
                2 * math.asin(math.sqrt(prop_1)) - 2 * math.asin(math.sqrt(prop_0))
            ) * math.sqrt(samp_size)
        else:
            z = (prop_1 - prop_0) / math.sqrt(prop_1 * (1 - prop_1) / samp_size)

        if type == "two-sided":
            power = normal().cdf(abs(z) - z_value)
        elif type == "greater":
            power = normal().cdf(z - z_value)
        else:  # type == "less":
            power = normal().cdf(-z - z_value)
    elif (
        isinstance(prop_0, (np.ndarray, pd.Series, list, tuple))
        and isinstance(prop_1, (np.ndarray, pd.Series, list, tuple))
        and isinstance(samp_size, (np.ndarray, pd.Series, list, tuple))
    ):
        prop_0 = numpy_array(prop_0)
        prop_1 = numpy_array(prop_1)
        samp_size = numpy_array(samp_size)

        for s in prop_0:
            if arcsin:
                z = (
                    2 * np.arcsin(np.sqrt(prop_1)) - 2 * np.arcsin(np.sqrt(prop_0))
                ) * np.sqrt(samp_size)
            else:
                z = (prop_1 - prop_0) / np.sqrt(prop_1 * (1 - prop_1) / samp_size)

            if type == "two-sided":
                power = normal().cdf(np.abs(z) - z_value)
            elif type == "greater":
                power = normal().cdf(z - z_value)
            else:  # type == "less":π
                power = normal().cdf(-z - z_value)

    return power


def power_for_two_proportions(
    prop_a: Union[DictStrNum, Number, Array],
    prop_b: Union[DictStrNum, Number, Array],
    samp_size: Optional[Union[DictStrNum, Number, Array]] = None,
    ratio: Optional[Union[DictStrNum, Number, Array]] = None,
    samp_size_a: Optional[Union[DictStrNum, Number, Array]] = None,
    samp_size_b: Optional[Union[DictStrNum, Number, Array]] = None,
    testing_type: str = "two-sided",
    alpha: Union[Number, Array] = 0.05,
) -> Union[DictStrNum, Number, Array]:

    type = testing_type.lower()
    if type not in ("two-sided", "less", "greater"):
        raise AssertionError("type must be 'two-sided', 'less', 'greater'.")

    assert_proportions(prop_a=prop_a, prop_b=prop_b, alpha=alpha)


def power_for_one_mean(
    mean_0: Union[DictStrNum, Number, Array],
    mean_1: Union[DictStrNum, Number, Array],
    sigma: Union[DictStrNum, Number, Array],
    samp_size: Union[DictStrNum, Number, Array],
    testing_type: str = "two-sided",
    alpha: Union[Number, Array] = 0.05,
) -> Union[DictStrNum, Number, Array]:

    type = testing_type.lower()
    if type not in ("two-sided", "less", "greater"):
        raise AssertionError("type must be 'two-sided', 'less', 'greater'.")

    assert_proportions(alpha=alpha)

    if (
        isinstance(mean_0, dict)
        and isinstance(mean_1, dict)
        and isinstance(sigma, dict)
        and isinstance(samp_size, dict)
    ):
        if type == "two-sided":
            return {
                s: normal().cdf(
                    abs(mean_0[s] - mean_1[s]) / (sigma[s] / math.sqrt(samp_size[s]))
                    - normal().ppf(1 - alpha / 2)
                )
                for s in mean_0
            }
        elif type == "greater":
            return normal().cdf(
                (mean_0 - mean_1) / (sigma / math.sqrt(samp_size))
                - normal().ppf(1 - alpha)
            )
        else:
            return normal().cdf(
                -(mean_0 - mean_1) / (sigma / math.sqrt(samp_size))
                - normal().ppf(1 - alpha)
            )
    elif (
        isinstance(mean_0, (int, float))
        and isinstance(mean_1, (int, float))
        and isinstance(sigma, (int, float))
        and isinstance(samp_size, (int, float))
    ):
        if type == "two-sided":
            return normal().cdf(
                abs(mean_0 - mean_1) / (sigma / math.sqrt(samp_size))
                - normal().ppf(1 - alpha / 2)
            )
        elif type == "greater":
            return normal().cdf(
                (mean_0 - mean_1) / (sigma / math.sqrt(samp_size))
                - normal().ppf(1 - alpha)
            )
        else:
            return normal().cdf(
                -(mean_0 - mean_1) / (sigma / math.sqrt(samp_size))
                - normal().ppf(1 - alpha)
            )

    elif (
        isinstance(mean_0, (np.np.ndarray, pd.Series, list, tuple))
        and isinstance(mean_1, (np.np.ndarray, pd.Series, list, tuple))
        and isinstance(sigma, (np.np.ndarray, pd.Series, list, tuple))
        and isinstance(samp_size, (np.np.ndarray, pd.Series, list, tuple))
    ):
        mean_0 = numpy_array(mean_0)
        mean_1 = numpy_array(mean_1)
        sigma = numpy_array(sigma)
        power = np.zeros(mean_0.shape[0])
        for k in range(mean_0.shape[0]):
            if type == "two-sided":
                power[k] = normal().cdf(
                    abs(mean_0[k] - mean_1[k]) / (sigma[k] / math.sqrt(samp_size[k]))
                    - normal().ppf(1 - alpha / 2)
                )
            elif type == "greater":
                power[k] = normal().cdf(
                    (mean_0[k] - mean_1[k]) / (sigma[k] / math.sqrt(samp_size[k]))
                    - normal().ppf(1 - alpha)
                )
            else:
                power[k] = normal().cdf(
                    -(mean_0[k] - mean_1[k]) / (sigma[k] / math.sqrt(samp_size[k]))
                    - normal().ppf(1 - alpha)
                )
        return power
