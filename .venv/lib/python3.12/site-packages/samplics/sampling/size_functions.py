"""Sample size calculation module
This module compiles the core functions for calculating power and sample size
"""

from __future__ import annotations

import math

from typing import Optional, Union

import numpy as np
import pandas as pd

from scipy.stats import norm as normal

from samplics.utils.formats import numpy_array
from samplics.utils.types import Array, DictStrNum, Number


def _ss_for_proportion_wald(
    target: Union[Number, Array],
    half_ci: Union[Number, Array],
    pop_size: Optional[Union[Number, Array]],
    deff_c: Union[Number, Array],
    resp_rate: Union[Number, Array],
    alpha: float,
) -> Union[Number, Array]:

    if isinstance(target, (np.ndarray, pd.Series, list, tuple)):
        target = numpy_array(target)
    if isinstance(half_ci, (np.ndarray, pd.Series, list, tuple)):
        half_ci = numpy_array(half_ci)
    if isinstance(deff_c, (np.ndarray, pd.Series, list, tuple)):
        deff_c = numpy_array(deff_c)
    if isinstance(resp_rate, (np.ndarray, pd.Series, list, tuple)):
        resp_rate = numpy_array(resp_rate)
    if isinstance(pop_size, (np.ndarray, pd.Series, list, tuple)):
        pop_size = numpy_array(pop_size)
    if isinstance(alpha, (np.ndarray, pd.Series, list, tuple)):
        alpha = numpy_array(alpha)

    z_value = normal().ppf(1 - alpha / 2)

    if isinstance(pop_size, (np.ndarray, int, float)):
        return math.ceil(
            (1 / resp_rate)
            * deff_c
            * pop_size
            * z_value**2
            * target
            * (1 - target)
            / ((pop_size - 1) * half_ci**2 + z_value**2 * target * (1 - target))
        )
    else:
        return math.ceil(
            (1 / resp_rate)
            * deff_c
            * z_value**2
            * target
            * (1 - target)
            / half_ci**2
        )


def _ss_for_proportion_wald_stratified(
    target: DictStrNum,
    half_ci: DictStrNum,
    pop_size: Optional[DictStrNum],
    deff_c: DictStrNum,
    resp_rate: DictStrNum,
    alpha: DictStrNum,
) -> DictStrNum:

    samp_size: DictStrNum = {}
    for s in half_ci:
        pop_size_c = None if pop_size is None else pop_size[s]
        samp_size[s] = _ss_for_proportion_wald(
            target=target[s],
            half_ci=half_ci[s],
            pop_size=pop_size_c,
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
        )

    return samp_size


def calculate_ss_wald_prop(
    target: Union[DictStrNum, Number, Array],
    half_ci: Union[DictStrNum, Number, Array],
    pop_size: Optional[Union[DictStrNum, Number, Array]] = None,
    deff_c: Union[DictStrNum, Number, Array] = 1.0,
    resp_rate: Union[DictStrNum, Number, Array] = 1.0,
    alpha: Union[DictStrNum, Number, Array] = 0.05,
    strat: bool = False,
) -> Union[DictStrNum, Number, Array]:

    if strat:
        return _ss_for_proportion_wald_stratified(
            target=target,
            half_ci=half_ci,
            pop_size=pop_size,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )
    else:
        return _ss_for_proportion_wald(
            target=target,
            half_ci=half_ci,
            pop_size=pop_size,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )


def _ss_for_proportion_fleiss(
    target: Union[Number, Array],
    half_ci: Union[Number, Array],
    deff_c: Union[Number, Array],
    resp_rate: Union[Number, Array],
    alpha: Union[Number, Array],
) -> Union[Number, Array]:

    if isinstance(target, (np.ndarray, pd.Series, list, tuple)):
        target = numpy_array(target)
    if isinstance(half_ci, (np.ndarray, pd.Series, list, tuple)):
        half_ci = numpy_array(half_ci)
    if isinstance(deff_c, (np.ndarray, pd.Series, list, tuple)):
        deff_c = numpy_array(deff_c)
    if isinstance(resp_rate, (np.ndarray, pd.Series, list, tuple)):
        resp_rate = numpy_array(resp_rate)
    if isinstance(alpha, (np.ndarray, pd.Series, list, tuple)):
        alpha = numpy_array(alpha)

    z_value = normal().ppf(1 - alpha / 2)

    def fleiss_factor(p: float, d: float) -> float:

        if 0 <= p < d or 1 - d < p <= 1:
            return 8 * d * (1 - 2 * d)
        elif d <= p < 0.3:
            return 4 * (p + d) * (1 - p - d)
        elif 0.7 < p <= 1 - d:
            return 4 * (p - d) * (1 - p + d)
        elif 0.3 <= p <= 0.7:
            return 1
        else:
            raise ValueError("Parameters p or d not valid.")

    if isinstance(target, np.ndarray):
        fct = np.zeros(target.shape[0])
        for k in range(target.shape[0]):
            fct[k] = fleiss_factor(target[k], half_ci[k])
    else:
        fct = fleiss_factor(target, half_ci)

    return math.ceil(
        (1 / resp_rate)
        * deff_c
        * (
            fct * (z_value**2) / (4 * half_ci**2)
            + 1 / half_ci
            - 2 * z_value**2
            + (z_value + 2) / fct
        )
    )


def _ss_for_proportion_fleiss_stratified(
    target: DictStrNum,
    half_ci: DictStrNum,
    deff_c: DictStrNum,
    resp_rate: DictStrNum,
    alpha: DictStrNum,
) -> DictStrNum:

    samp_size: DictStrNum = {}
    for s in half_ci:
        samp_size[s] = _ss_for_proportion_fleiss(
            target=target[s],
            half_ci=half_ci[s],
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
        )

    return samp_size


def calculate_ss_fleiss_prop(
    target: Union[DictStrNum, Number, Array],
    half_ci: Union[DictStrNum, Number, Array],
    deff_c: Union[DictStrNum, Number, Array] = 1.0,
    resp_rate: Union[DictStrNum, Number, Array] = 1.0,
    alpha: Union[DictStrNum, Number, Array] = 0.05,
    strat: bool = False,
) -> Union[DictStrNum, Number, Array]:

    if strat:
        return _ss_for_proportion_fleiss_stratified(
            target=target,
            half_ci=half_ci,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )
    else:
        return _ss_for_proportion_fleiss(
            target=target,
            half_ci=half_ci,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )


def _ss_for_mean_wald(
    half_ci: Union[Number, Array],
    sigma: Union[Number, Array],
    pop_size: Optional[Union[Number, Array]],
    deff_c: Union[Number, Array],
    resp_rate: Union[Number, Array],
    alpha: Union[Number, Array],
) -> Union[Number, Array]:

    if isinstance(half_ci, (np.ndarray, pd.Series, list, tuple)):
        half_ci = numpy_array(half_ci)
    if isinstance(sigma, (np.ndarray, pd.Series, list, tuple)):
        sigma = numpy_array(sigma)
    if isinstance(deff_c, (np.ndarray, pd.Series, list, tuple)):
        deff_c = numpy_array(deff_c)
    if isinstance(resp_rate, (np.ndarray, pd.Series, list, tuple)):
        resp_rate = numpy_array(resp_rate)
    if isinstance(pop_size, (np.ndarray, pd.Series, list, tuple)):
        pop_size = numpy_array(pop_size)
    if isinstance(alpha, (np.ndarray, pd.Series, list, tuple)):
        alpha = numpy_array(alpha)

    z_value = normal().ppf(1 - alpha / 2)
    if pop_size is not None:
        return math.ceil(
            ((1 / resp_rate) * deff_c * pop_size * z_value**2 * sigma**2)
            / ((pop_size - 1) * half_ci**2 + z_value**2 * sigma**2)
        )
    else:
        return math.ceil(
            (1 / resp_rate) * deff_c * z_value**2 * sigma**2 / half_ci**2
        )


def _ss_for_mean_wald_stratified(
    half_ci: DictStrNum,
    sigma: Union[Number, Array],
    pop_size: Optional[DictStrNum],
    deff_c: DictStrNum,
    resp_rate: DictStrNum,
    alpha: DictStrNum,
) -> DictStrNum:

    samp_size: DictStrNum = {}
    for s in half_ci:
        pop_size_c = None if pop_size is None else pop_size[s]
        samp_size[s] = _ss_for_mean_wald(
            half_ci=half_ci[s],
            sigma=sigma[s],
            pop_size=pop_size_c,
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
        )

    return samp_size


def calculate_ss_wald_mean(
    half_ci: Union[DictStrNum, Number, Array],
    sigma: Union[DictStrNum, Number, Array],
    pop_size: Optional[Union[DictStrNum, Number, Array]] = None,
    deff_c: Union[DictStrNum, Number, Array] = 1.0,
    resp_rate: Union[DictStrNum, Number] = 1.0,
    alpha: Union[DictStrNum, Number, Array] = 0.05,
    strat: bool = False,
) -> Union[DictStrNum, Number, Array]:

    if strat:
        return _ss_for_mean_wald_stratified(
            half_ci=half_ci,
            sigma=sigma,
            pop_size=pop_size,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )
    else:
        return _ss_for_mean_wald(
            half_ci=half_ci,
            sigma=sigma,
            pop_size=pop_size,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
        )


def _calculate_ss_wald_mean_one_sample(
    two_sides: bool,
    epsilon: Union[Array, Number],
    delta: Union[Array, Number],
    sigma: Union[Array, Number],
    deff_c: Union[Array, Number],
    resp_rate: Union[Array, Number],
    alpha: Union[Array, Number],
    power: Union[Array, Number],
) -> Union[Array, Number]:

    if two_sides and delta == 0:
        z_alpha = normal().ppf(1 - alpha / 2)
        z_beta = normal().ppf(power)
    elif two_sides and delta != 0:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf((1 + power) / 2)  # 1 - beta/2 where beta = 1 - power
    else:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf(power)

    return math.ceil(
        (1 / resp_rate)
        * deff_c
        * ((z_alpha + z_beta) * sigma / (delta - abs(epsilon))) ** 2
    )


def _calculate_ss_wald_mean_one_sample_stratified(
    two_sides: bool,
    epsilon: DictStrNum,
    delta: DictStrNum,
    sigma: DictStrNum,
    deff_c: DictStrNum,
    resp_rate: DictStrNum,
    alpha: DictStrNum,
    power: DictStrNum,
) -> DictStrNum:

    samp_size: DictStrNum = {}
    for s in epsilon:
        samp_size[s] = _calculate_ss_wald_mean_one_sample(
            two_sides=two_sides,
            epsilon=epsilon[s],
            delta=delta[s],
            sigma=sigma[s],
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
            power=power[s],
        )
    return samp_size


def calculate_ss_wald_mean_one_sample(
    two_sides: bool,
    epsilon: Union[DictStrNum, Number, Array],
    delta: Union[DictStrNum, Number, Array],
    sigma: Union[DictStrNum, Number, Array],
    deff_c: Union[DictStrNum, Number, Array],
    resp_rate: Union[DictStrNum, Number, Array],
    alpha: Union[DictStrNum, Number, Array],
    power: Union[DictStrNum, Number, Array],
    strat: bool = True,
) -> DictStrNum:

    if strat:
        return _calculate_ss_wald_mean_one_sample_stratified(
            two_sides=two_sides,
            epsilon=epsilon,
            delta=delta,
            sigma=sigma,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )
    else:
        return _calculate_ss_wald_mean_one_sample(
            two_sides=two_sides,
            epsilon=epsilon,
            delta=delta,
            sigma=sigma,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )


def _calculate_ss_wald_mean_two_samples(
    two_sides: bool,
    epsilon: Union[Array, Number],
    delta: Union[Array, Number],
    sigma_1: Union[Array, Number],
    sigma_2: Optional[Union[Array, Number]],
    equal_var: bool,
    kappa: Union[Array, Number],
    deff_c: Union[Array, Number],
    resp_rate: Union[Array, Number],
    alpha: Union[Array, Number],
    power: Union[Array, Number],
) -> tuple[Union[Array, Number], Union[Array, Number]]:

    if two_sides and delta == 0:
        z_alpha = normal().ppf(1 - alpha / 2)
        z_beta = normal().ppf(power)
    elif two_sides and delta != 0:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf((1 + power) / 2)  # 1 - beta/2 where beta = 1 - power
    else:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf(power)

    if equal_var:
        samp_size_2 = math.ceil(
            (1 / resp_rate)
            * deff_c
            * (1 + 1 / kappa)
            * ((z_alpha + z_beta) * sigma_1 / (delta - abs(epsilon))) ** 2
        )
        samp_size_1 = math.ceil(kappa * samp_size_2)
    else:
        pass

    return (samp_size_1, samp_size_2)


def _calculate_ss_wald_mean_two_samples_stratified(
    two_sides: bool,
    epsilon: Union[Array, Number],
    delta: Union[Array, Number],
    sigma_1: Union[Array, Number],
    sigma_2: Optional[Union[Array, Number]],
    equal_var: bool,
    kappa: Union[Array, Number],
    deff_c: Union[Array, Number],
    resp_rate: Union[Array, Number],
    alpha: Union[Array, Number],
    power: Union[Array, Number],
) -> tuple[DictStrNum, DictStrNum]:

    samp_size_1: DictStrNum = {}
    samp_size_2: DictStrNum = {}
    for s in epsilon:
        sigma_2_s = sigma_2[s] if sigma_2 is not None else None
        samp_size_1[s], samp_size_2[s] = _calculate_ss_wald_mean_two_samples(
            two_sides=two_sides,
            epsilon=epsilon[s],
            delta=delta[s],
            sigma_1=sigma_1[s],
            sigma_2=sigma_2_s,
            equal_var=equal_var,
            kappa=kappa,
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
            power=power[s],
        )
    return (samp_size_1, samp_size_2)


def calculate_ss_wald_mean_two_samples(
    two_sides: bool,
    epsilon: Union[DictStrNum, Number, Array],
    delta: Union[DictStrNum, Number, Array],
    sigma_1: Union[DictStrNum, Number, Array],
    sigma_2: Union[DictStrNum, Number, Array],
    equal_var: Union[DictStrNum, Number, Array],
    kappa: Union[DictStrNum, Number, Array],
    deff_c: Union[DictStrNum, Number, Array],
    resp_rate: Union[DictStrNum, Number, Array],
    alpha: Union[DictStrNum, Number, Array],
    power: Union[DictStrNum, Number, Array],
    strat: bool = True,
) -> DictStrNum:

    if strat:
        return _calculate_ss_wald_mean_two_samples_stratified(
            two_sides=two_sides,
            epsilon=epsilon,
            delta=delta,
            sigma_1=sigma_1,
            sigma_2=sigma_2,
            equal_var=equal_var,
            kappa=kappa,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )
    else:
        return _calculate_ss_wald_mean_two_samples(
            two_sides=two_sides,
            epsilon=epsilon,
            delta=delta,
            sigma_1=sigma_1,
            sigma_2=sigma_2,
            equal_var=equal_var,
            kappa=kappa,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )


def _calculate_ss_wald_prop_two_samples(
    two_sides: bool,
    epsilon: Union[Array, Number],
    prop_1: Union[Array, Number],
    prop_2: Union[Array, Number],
    delta: Union[Array, Number],
    kappa: Union[Array, Number],
    deff_c: Union[Array, Number],
    resp_rate: Union[Array, Number],
    alpha: Union[Array, Number],
    power: Union[Array, Number],
) -> tuple[Union[Array, Number], Union[Array, Number]]:

    if two_sides and delta == 0:
        z_alpha = normal().ppf(1 - alpha / 2)
        z_beta = normal().ppf(power)
    elif two_sides and delta != 0:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf((1 + power) / 2)  # 1 - beta/2 where beta = 1 - power
    else:
        z_alpha = normal().ppf(1 - alpha)
        z_beta = normal().ppf(power)

    samp_size_2 = math.ceil(
        (1 / resp_rate)
        * deff_c
        * (prop_1 * (1 - prop_1) / kappa + prop_2 * (1 - prop_2))
        * ((z_alpha + z_beta) / (delta - abs(epsilon))) ** 2
    )
    samp_size_1 = math.ceil(kappa * samp_size_2)

    return (samp_size_1, samp_size_2)


def _calculate_ss_wald_prop_two_samples_stratified(
    two_sides: bool,
    epsilon: DictStrNum,
    prop_1: DictStrNum,
    prop_2: DictStrNum,
    delta: DictStrNum,
    kappa: DictStrNum,
    deff_c: DictStrNum,
    resp_rate: DictStrNum,
    alpha: DictStrNum,
    power: DictStrNum,
) -> DictStrNum:

    samp_size: DictStrNum = {}
    for s in epsilon:
        samp_size[s] = _calculate_ss_wald_prop_two_samples(
            two_sides=two_sides[s],
            epsilon=epsilon[s],
            prop_1=prop_1[s],
            prop_2=prop_2[s],
            delta=delta[s],
            kappa=kappa[s],
            deff_c=deff_c[s],
            resp_rate=resp_rate[s],
            alpha=alpha[s],
            power=power[s],
        )
    return samp_size


def calculate_ss_wald_prop_two_samples(
    two_sides: bool,
    epsilon: Union[DictStrNum, Number, Array],
    prop_1: Union[DictStrNum, Number, Array],
    prop_2: Union[DictStrNum, Number, Array],
    delta: Union[DictStrNum, Number, Array],
    kappa: Union[DictStrNum, Number, Array],
    deff_c: Union[DictStrNum, Number, Array],
    resp_rate: Union[DictStrNum, Number, Array],
    alpha: Union[DictStrNum, Number, Array],
    power: Union[DictStrNum, Number, Array],
    strat: bool = True,
) -> DictStrNum:

    if strat:
        return _calculate_ss_wald_prop_two_samples_stratified(
            two_sides=two_sides,
            epsilon=epsilon,
            prop_1=prop_1,
            prop_2=prop_2,
            delta=delta,
            kappa=kappa,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )
    else:
        return _calculate_ss_wald_prop_two_samples(
            two_sides=two_sides,
            epsilon=epsilon,
            prop_1=prop_1,
            prop_2=prop_2,
            delta=delta,
            kappa=kappa,
            deff_c=deff_c,
            resp_rate=resp_rate,
            alpha=alpha,
            power=power,
        )
