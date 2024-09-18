"""This module provides basic functions used across multiple classes and modules.

Functions:
    | *sumby()* sums numpy arrays by some grouping.
    | *averageby()* average numpy arrays by some grouping.
    | *transform()* implements the Boxcox tranformation.
    | *skewness()* computes the Pearson's moment coefficient of skewness (assymetry measure).
    | *kurtosis()* computes the fourth standardized moment, a measure of assymetry.
    | *plot_skewness()* and *plot_kurtosis()* visualise the skewness and kurtosis coefficients
respectively.

"""

from __future__ import annotations

from typing import Optional, Union

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from samplics.utils.formats import numpy_array
from samplics.utils.types import Array, Number


def set_variables_names(
    vars: Array, varnames: Optional[Union[str, list[str]]], prefix: str
) -> Union[str, list[str]]:

    if varnames is None:
        if isinstance(vars, pd.DataFrame):
            return list(vars.columns)
        elif isinstance(vars, pd.Series) and vars.name is not None:
            return [vars.name]
        else:
            if isinstance(vars, (pd.Series, np.ndarray)):
                if len(vars.shape) == 2:
                    return [prefix + "_" + str(k) for k in range(1, vars.shape[1] + 1)]
                else:
                    return [
                        prefix + "_" + str(k) for k in range(1, len(vars.shape) + 1)
                    ]
            elif isinstance(vars, (tuple, list)):
                return [prefix + "_" + str(k) for k in range(1, len(vars) + 1)]
            else:
                raise TypeError("vars should be an array-like")
    else:
        return varnames


def sumby(
    group: np.ndarray, y: np.ndarray
) -> np.ndarray:  # Could use pd.grouby().sum(), may scale better
    """Sums the numpy array by group.

    Args:
        group (np.ndarray): provides the group associated to each observation.
        y (np.ndarray): variable of interest for the summation.

    Returns:
        np.ndarray: the sums by group.
    """

    groups = np.unique(group)
    sums = np.zeros(groups.size)
    for k, gr in enumerate(groups):
        sums[k] = np.sum(y[group == gr])

    return sums


def averageby(
    group: np.ndarray, y: np.ndarray
) -> np.ndarray:  # Could use pd.grouby().sum(), may scale better
    """[summary]

    Args:
        group (np.ndarray): provides the group associated to each observation.
        y (np.ndarray): variable of interest for the averaging.

    Returns:
        np.ndarray: the averages by group.
    """

    groups = np.unique(group)
    means = np.zeros(groups.size)
    for k, gr in enumerate(groups):
        means[k] = np.mean(y[group == gr])

    return means


def transform(
    y: np.ndarray,
    llambda: Optional[Number] = None,
    constant: Optional[Number] = None,
    inverse: bool = True,
) -> np.ndarray:
    """Transforms the variable of interest using the Boxcox method or its inverse.

    Args:
        y (np.ndarray): variable of interest.
        llambda (Optional[Number], optional): Boxcox parameter lambda. A value of zero indicates
        a logarithm transformation. Defaults to None.
        constant (float, optional): An additive term to ensure that the logarithm function is
        applied to positive values. Defaults to 0.0.
        inverse (bool, optional): indicates if the Boxcox transformation is applied or its
        inverse. Defaults to True.

    Returns:
        np.ndarray: [description]
    """
    if llambda is None:
        return y
    elif llambda == 0.0:
        if constant is None:
            constant = 0.0
        if inverse:
            return np.asarray(np.exp(y) - constant)
        else:
            if ((y + constant) <= 0).any():
                raise ValueError("log function not defined for negative numbers")
            else:
                return np.asarray(np.log(y + constant))
    elif llambda != 0.0:
        if inverse:
            if ((1 + y * llambda) <= 0).any():
                raise ValueError("log function not defined for negative numbers")
            return np.asarray(np.exp(np.log(1 + y * llambda) / llambda))
        else:
            return np.asarray(np.power(y, llambda) / llambda)
    else:
        raise AssertionError


def skewness(y: Array, type: int = 1) -> float:

    if type not in (1, 2, 3):
        raise AssertionError("Parameter type must be 1, 2 or 3.")

    y = numpy_array(y)
    skewness = float(np.mean((y - np.mean(y)) ** 3) / np.std(y) ** 3)
    if type == 2:
        n = y.shape[0]
        if n <= 3:
            raise ValueError("For type 2, y must be of size 3 or more.")
        else:
            skewness = skewness * np.sqrt(n * (n - 1)) / (n - 2)
    elif type == 3:
        n = y.shape[0]
        skewness = skewness * np.power(1 - 1 / n, 3 / 2)

    return skewness


def kurtosis(y: Array, type: int = 1) -> float:

    if type not in (1, 2, 3):
        raise AssertionError("Parameter type must be 1, 2 or 3.")

    y = numpy_array(y)
    kurtosis = float(np.mean((y - np.mean(y)) ** 4) / np.std(y) ** 4)
    if type == 1:
        kurtosis = kurtosis - 3
    elif type == 2:
        n = y.shape[0]
        if n <= 4:
            raise ValueError("For type 2, y must be of size 4 or more.")
        kurtosis = ((n + 1) * (kurtosis - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
    elif type == 3:
        n = y.shape[0]
        kurtosis = kurtosis * np.power(1 - 1 / n, 2) - 3

    return kurtosis


def _plot_measure(
    y: np.ndarray,
    coef_min: Number = -5,
    coef_max: Number = 5,
    nb_points: int = 100,
    measure: str = "skewness",
    block: bool = True,
) -> None:
    y = numpy_array(y)
    lambda_range = np.linspace(coef_min, coef_max, num=nb_points)
    coefs = np.zeros(lambda_range.size)
    measure_loc = None
    for k, ll in enumerate(lambda_range):
        if (1 + y * ll < 0).any():
            break
        y_ll = transform(y, ll)
        if measure.lower() == "skewness":
            coefs[k] = skewness(y_ll)
            measure_loc = "lower right"
        elif measure.lower() == "kurtosis":
            coefs[k] = kurtosis(y_ll)
            measure_loc = "upper right"
        else:
            raise ValueError("measure type not valid!")

    normality = np.abs(coefs) < 2.0

    p1 = plt.scatter(
        lambda_range[normality],
        coefs[normality],
        marker="D",
        c="green",
        s=25,
        alpha=0.3,
    )
    p2 = plt.scatter(
        lambda_range[~normality],
        coefs[~normality],
        c="red",
        s=10,
        alpha=0.6,
        edgecolors="none",
    )
    plt.axhline(0, color="blue", linestyle="--")
    plt.title(f"{measure.title()} by BoxCox lambda")
    plt.ylabel(f"{measure.title()}")
    plt.xlabel("Lambda (coefs)")
    plt.legend(
        (p1, p2),
        ("Normality zone", "Non-normality zone"),
        loc=measure_loc,
    )
    plt.show(block=block)


def plot_skewness(
    y: np.ndarray,
    coef_min: Number = -5,
    coef_max: Number = 5,
    nb_points: int = 100,
    block: bool = True,
) -> None:
    """Plots a scatter plot of skewness coefficients and the lambda coefficients of the Boxcox
    transformation. The plot helps identify the range of lambda values to minimize the
    skewness coefficient.

    Args:
        y (np.ndarray): variable of interest.
        coef_min (Number, optional): lowest value for the horizontal axis. Defaults to -5.
        coef_max (Number, optional): highest value for the horizontal axis. Defaults to 5.
        nb_points (int, optional): number of points in the plot. Defaults to 100.
    """
    _plot_measure(
        y=y,
        coef_min=coef_min,
        coef_max=coef_max,
        nb_points=nb_points,
        measure="skewness",
        block=block,
    )


def plot_kurtosis(
    y: np.ndarray,
    coef_min: Number = -5,
    coef_max: Number = 5,
    nb_points: int = 100,
    block: bool = True,
) -> None:
    """Plots a scatter plot of skewness coefficients and the lambda coefficients of the Boxcox
    transformation. The plot helps identify the range of lambda values to minimize the
    kurtosis coefficient.

    Args:
        y (np.ndarray): variable of interest.
        coef_min (Number, optional): lowest value for the horizontal axis. Defaults to -5.
        coef_max (Number, optional): highest value for the horizontal axis. Defaults to 5.
        nb_points (int, optional): number of points in the plot. Defaults to 100.
    """
    _plot_measure(
        y=y,
        coef_min=coef_min,
        coef_max=coef_max,
        nb_points=nb_points,
        measure="kurtosis",
        block=block,
    )


def get_single_psu_strata(stratum: Array, psu: Array) -> Optional(np.ndarray):
    stratum = numpy_array(stratum)
    psu = numpy_array(psu)

    if psu.shape in (
        (),
        (0,),
    ):  # psu is None will not work because psu is an np.ndarray
        strata_ids, psu_counts = np.unique(stratum, return_counts=True)
    else:
        strata_ids, psu_counts = np.unique(
            np.unique((stratum, psu), axis=1)[0, :], return_counts=True
        )
    # breakpoint()

    single_psus = psu_counts == 1
    if single_psus.sum() == 0:
        return None
    else:
        return strata_ids[single_psus]
