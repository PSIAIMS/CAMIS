"""Comparison module

The module implements comparisons of groups.

"""

from __future__ import annotations

import math

from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd

from scipy.stats import t

from samplics.estimation import TaylorEstimator
from samplics.utils.basic_functions import set_variables_names
from samplics.utils.checks import assert_probabilities
from samplics.utils.formats import numpy_array
from samplics.utils.types import Array, Number, Series, SinglePSUEst, StringNumber, PopParam


class Ttest:
    def __init__(
        self, samp_type: str, paired: bool = False, alpha: float = 0.05
    ) -> None:

        if samp_type.lower() not in ("one-sample", "two-sample"):
            raise ValueError(
                "Parameter 'type' must be equal to 'one-sample', 'two-sample'!"
            )
        assert_probabilities(x=alpha)

        self.samp_type = samp_type.lower()
        self.paired = paired

        self.point_est: Any = {}
        self.stats: Any = {}
        self.stderror: Any = {}
        self.stddev: Any = {}
        self.lower_ci: Any = {}
        self.upper_ci: Any = {}
        self.deff: Any = {}
        self.alpha: float = alpha
        # self.design_info: Dict[str, Number] = {}
        self.group_names: List[str] = []
        self.group_levels: Dict[str, StringNumber] = {}

    def __repr__(self) -> str:
        return f"T-test(samp_type={self.samp_type}, paired={self.paired}, alpha={self.alpha})"

    def __str__(self) -> str:

        if self.point_est == {}:
            return "No table to display"
        else:
            tbl_head = f"Design-based {self.samp_type.title()} T-test"
            if (
                self.samp_type == "one-sample" and self.group_names == []
            ) or self.paired:
                if self.samp_type == "one-sample":
                    tbl_subhead1 = (
                        f" Null hypothesis (Ho): mean = {self.stats['known_mean']}"
                    )
                else:
                    tbl_subhead1 = f" Null hypothesis (Ho): mean(Diff = {self.vars_names[0]} - {self.vars_names[1]}) = 0"
                tbl_subhead2 = f" t statistics: {self.stats['t']:.4f}"
                tbl_subhead3 = f" Degrees of freedom: {self.stats['df']:.2f}"
                tbl_subhead4 = " Alternative hypothesis (Ha):"
                tbl_subhead4a = (
                    f"  Prob(T < t) = {self.stats['p_value']['less_than']:.4f}"
                )
                tbl_subhead4b = (
                    f"  Prob(|T| > |t|) = {self.stats['p_value']['not_equal']:.4f}"
                )
                tbl_subhead4c = (
                    f"  Prob(T > t) = {self.stats['p_value']['greater_than']:.4f}"
                )

                return f"\n{tbl_head}\n{tbl_subhead1}\n{tbl_subhead2}\n{tbl_subhead3}\n{tbl_subhead4}\n{tbl_subhead4a}\n{tbl_subhead4b}\n{tbl_subhead4c} \n\n{self.to_dataframe().to_string(index=False)}\n"

            elif (self.samp_type == "one-sample" and self.group_names != []) or (
                self.samp_type == "two-sample" and not self.paired
            ):
                tbl_subhead1 = f" Null hypothesis (Ho): mean({self.group_names[0]}) = mean({self.group_names[1]}) "
                tbl_subhead2 = " Equal variance assumption:"
                tbl_subhead2a = f"  t statistics: {self.stats['t_eq_variance']:.4f}"
                tbl_subhead2b = (
                    f"  Degrees of freedom: {self.stats['df_eq_variance']:.2f}"
                )
                tbl_subhead3 = "  Alternative hypothesis (Ha):"
                tbl_subhead3a = f"   Prob(T < t) = {self.stats['p_value_eq_variance']['less_than']:.4f}"
                tbl_subhead3b = f"   Prob(|T| > |t|) = {self.stats['p_value_eq_variance']['not_equal']:.4f}"
                tbl_subhead3c = f"   Prob(T > t) = {self.stats['p_value_eq_variance']['greater_than']:.4f}"
                tbl_subhead4 = " Unequal variance assumption:"
                tbl_subhead4a = f"  t statistics: {self.stats['t_uneq_variance']:.4f}"
                tbl_subhead4b = (
                    f"  Degrees of freedom: {self.stats['df_uneq_variance']:.2f}"
                )
                tbl_subhead5 = "  Alternative hypothesis (Ha):"
                tbl_subhead5a = f"   Prob(T < t) = {self.stats['p_value_uneq_variance']['less_than']:.4f}"
                tbl_subhead5b = f"   Prob(|T| > |t|) = {self.stats['p_value_uneq_variance']['not_equal']:.4f}"
                tbl_subhead5c = f"   Prob(T > t) = {self.stats['p_value_uneq_variance']['greater_than']:.4f}"

                return f"\n{tbl_head}\n{tbl_subhead1}\n{tbl_subhead2}\n{tbl_subhead2a}\n{tbl_subhead2b}\n{tbl_subhead3}\n{tbl_subhead3a}\n{tbl_subhead3b}\n{tbl_subhead3c}\n{tbl_subhead4}\n{tbl_subhead4a}\n{tbl_subhead4b}\n{tbl_subhead5}\n{tbl_subhead5a}\n{tbl_subhead5b}\n{tbl_subhead5c} \n\n{self.to_dataframe().to_string(index=False)}\n"
            else:
                raise ValueError("Wrong specifications!")

    def _one_sample_one_group(
        self,
        y: np.ndarray,
        known_mean: Number,
        samp_weight: Array,
        stratum: Array,
        psu: Array,
        ssu: Array,
        fpc: Union[Dict, float] = 1,
        coef_var: bool = False,
        single_psu: Union[
            SinglePSUEst, dict[StringNumber, SinglePSUEst]
        ] = SinglePSUEst.error,
        strata_comb: Optional[dict[Array, Array]] = None,
    ) -> None:

        one_sample = TaylorEstimator(param=PopParam.mean, alpha=self.alpha)
        one_sample.estimate(
            y=y,
            samp_weight=samp_weight,
            stratum=stratum,
            psu=psu,
            ssu=ssu,
            fpc=fpc,
            coef_var=coef_var,
            single_psu=single_psu,
            strata_comb=strata_comb,
        )

        samp_mean = one_sample.point_est
        samp_std_dev = math.sqrt(y.shape[0]) * one_sample.stderror
        samp_t_value = math.sqrt(y.shape[0]) * (samp_mean - known_mean) / samp_std_dev
        left_p_value = t.cdf(samp_t_value, y.shape[0] - 1)

        # self.design_info = {
        #     "number_strata": one_sample.number_strata,
        #     "number_psus": one_sample.number_psus,
        #     "design_effect": 0,
        #     "degrees_of_freedom": one_sample.number_psus - one_sample.number_strata,
        # }

        self.stats = {
            "number_obs": y.shape[0],
            "known_mean": known_mean,
            "df": y.shape[0] - 1,
            "t": samp_t_value,
            "p_value": {
                "less_than": left_p_value,
                "greater_than": 1 - left_p_value,
                "not_equal": 2 * t.cdf(-abs(samp_t_value), y.shape[0] - 1),
            },
        }

        self.point_est = one_sample.point_est
        self.stderror = one_sample.stderror
        self.lower_ci = one_sample.lower_ci
        self.upper_ci = one_sample.upper_ci
        self.stddev = samp_std_dev

    def _two_groups_unpaired(
        self, mean_est: TaylorEstimator, group: np.ndarray
    ) -> Tuple[Any, Any, Any, Any, Any, Any]:

        if self.samp_type == "one-sample":
            group1 = mean_est.domains[0]
            group2 = mean_est.domains[1]
        elif self.samp_type == "two-sample":
            group1 = mean_est.by[0]
            group2 = mean_est.by[1]
        else:
            raise ValueError("Parameter type is not valid!")

        mean_group1 = mean_est.point_est[group1]
        mean_group2 = mean_est.point_est[group2]

        nb_obs_group1 = np.sum(group == group1)
        nb_obs_group2 = np.sum(group == group2)

        stddev_group1 = math.sqrt(nb_obs_group1) * mean_est.stderror[group1]
        stddev_group2 = math.sqrt(nb_obs_group2) * mean_est.stderror[group2]

        t_equal_variance = (mean_group1 - mean_group2) / (
            math.sqrt(
                (
                    (nb_obs_group1 - 1) * stddev_group1**2
                    + (nb_obs_group2 - 1) * stddev_group2**2
                )
                / (nb_obs_group1 + nb_obs_group2 - 2)
            )
            * math.sqrt(1 / nb_obs_group1 + 1 / nb_obs_group2)
        )

        t_df_equal_variance = nb_obs_group1 + nb_obs_group2 - 2

        t_unequal_variance = (mean_group1 - mean_group2) / math.sqrt(
            stddev_group1**2 / nb_obs_group1 + stddev_group2**2 / nb_obs_group2
        )

        t_df_unequal_variance = math.pow(
            stddev_group1**2 / nb_obs_group1 + stddev_group2**2 / nb_obs_group2, 2
        ) / (
            math.pow(stddev_group1**2 / nb_obs_group1, 2) / (nb_obs_group1 - 1)
            + math.pow(stddev_group2**2 / nb_obs_group2, 2) / (nb_obs_group2 - 1)
        )

        left_p_value_equal_variance = t.cdf(t_equal_variance, t_df_equal_variance)
        both_p_value_equal_variance = 2 * t.cdf(
            -abs(t_equal_variance), t_df_equal_variance
        )

        left_p_value_unequal_variance = t.cdf(t_unequal_variance, t_df_unequal_variance)
        both_p_value_unequal_variance = 2 * t.cdf(
            -abs(t_unequal_variance), t_df_unequal_variance
        )

        stats = {
            "number_obs": {group1: nb_obs_group1, group2: nb_obs_group2},
            "t_eq_variance": t_equal_variance,
            "df_eq_variance": t_df_equal_variance,
            "t_uneq_variance": t_unequal_variance,
            "df_uneq_variance": t_df_unequal_variance,
            "p_value_eq_variance": {
                "less_than": left_p_value_equal_variance,
                "greater_than": 1 - left_p_value_equal_variance,
                "not_equal": both_p_value_equal_variance,
            },
            "p_value_uneq_variance": {
                "less_than": left_p_value_unequal_variance,
                "greater_than": 1 - left_p_value_unequal_variance,
                "not_equal": both_p_value_unequal_variance,
            },
        }

        if (
            isinstance(mean_est.point_est, dict)
            and isinstance(mean_est.stderror, dict)
            and isinstance(mean_est.lower_ci, dict)
            and isinstance(mean_est.upper_ci, dict)
        ):
            point_est = mean_est.point_est.copy()
            stderror = mean_est.stderror.copy()
            lower_ci = mean_est.lower_ci.copy()
            upper_ci = mean_est.upper_ci.copy()
        else:
            point_est = mean_est.point_est
            stderror = mean_est.stderror
            lower_ci = mean_est.lower_ci
            upper_ci = mean_est.upper_ci

        stddev = {
            group1: math.sqrt(nb_obs_group1) * stderror[group1],
            group2: math.sqrt(nb_obs_group2) * stderror[group2],
        }

        return point_est, stderror, stddev, lower_ci, upper_ci, stats

    def _two_samples_unpaired(
        self,
        y: np.ndarray,
        group: np.ndarray,
        samp_weight: Optional[Array] = None,
        stratum: Optional[Array] = None,
        psu: Optional[Array] = None,
        ssu: Optional[Array] = None,
        fpc: Union[Dict, float] = 1,
        coef_var: bool = False,
        single_psu: Union[
            SinglePSUEst, dict[StringNumber, SinglePSUEst]
        ] = SinglePSUEst.error,
        strata_comb: Optional[dict[Array, Array]] = None,
    ) -> None:

        two_samples_unpaired = TaylorEstimator(param=PopParam.mean, alpha=self.alpha)
        two_samples_unpaired.estimate(
            y=y,
            by=group,
            samp_weight=samp_weight,
            stratum=stratum,
            psu=psu,
            ssu=ssu,
            fpc=fpc,
            coef_var=coef_var,
            single_psu=single_psu,
            strata_comb=strata_comb,
        )
        self._two_groups_unpaired(mean_est=two_samples_unpaired, group=group)

    def compare(
        self,
        y: Union[Series, Array],
        known_mean: Optional[Number] = None,
        group: Optional[Array] = None,
        varnames: Optional[Union[str, List[str]]] = None,
        samp_weight: Array = None,
        stratum: Optional[Array] = None,
        psu: Optional[Array] = None,
        ssu: Optional[Array] = None,
        fpc: Union[Dict, float] = 1,
        coef_var: bool = False,
        single_psu: Union[
            SinglePSUEst, dict[StringNumber, SinglePSUEst]
        ] = SinglePSUEst.error,
        strata_comb: Optional[dict[Array, Array]] = None,
        remove_nan: bool = False,
    ) -> None:

        if y is None:
            raise AssertionError("vars need to be an array-like object")
        if known_mean is None and group is None:
            raise AssertionError("Parameters 'known_mean' or 'group' must be provided!")
        if known_mean is not None and group is not None:
            raise AssertionError(
                "Only one parameter 'known_mean' or 'group' should be provided!"
            )

        if varnames is None:
            self.vars_names = set_variables_names(y, None, "var")
        elif isinstance(varnames, str):
            self.vars_names = set_variables_names(y, None, varnames)
        elif isinstance(varnames, list):
            self.vars_names = set_variables_names(y, varnames, varnames[0])
        else:
            raise AssertionError("varnames should be a string or a list of string")

        _y = numpy_array(y)
        _group = numpy_array(group)
        _samp_weight = numpy_array(samp_weight)
        _stratum = numpy_array(stratum)
        _psu = numpy_array(psu)
        _ssu = numpy_array(ssu)

        if self.samp_type == "one-sample" and known_mean is not None:
            self._one_sample_one_group(
                y=_y,
                known_mean=known_mean,
                samp_weight=_samp_weight,
                stratum=_stratum,
                psu=_psu,
                ssu=_ssu,
                fpc=fpc,
                coef_var=coef_var,
                single_psu=single_psu,
                strata_comb=strata_comb,
            )
        elif self.samp_type == "one-sample" and group is not None:
            one_sample = TaylorEstimator(param=PopParam.mean, alpha=self.alpha)
            one_sample.estimate(
                y=_y,
                domain=_group,
                samp_weight=_samp_weight,
                stratum=_stratum,
                psu=_psu,
                ssu=_ssu,
                fpc=fpc,
                coef_var=coef_var,
                single_psu=single_psu,
                strata_comb=strata_comb,
            )
            (
                self.point_est,
                self.stderror,
                self.stddev,
                self.lower_ci,
                self.upper_ci,
                self.stats,
            ) = self._two_groups_unpaired(mean_est=one_sample, group=_group)
            self.group_names = list(self.point_est.keys())
        elif self.samp_type == "two-sample" and not self.paired:
            self._two_samples_unpaired(
                y=_y,
                group=_group,
                samp_weight=_samp_weight,
                stratum=_stratum,
                psu=_psu,
                ssu=_ssu,
                fpc=fpc,
                coef_var=coef_var,
                single_psu=single_psu,
                strata_comb=strata_comb,
            )

            two_samples_unpaired = TaylorEstimator(param=PopParam.mean, alpha=self.alpha)
            two_samples_unpaired.estimate(
                y=_y,
                by=_group,
                samp_weight=_samp_weight,
                stratum=_stratum,
                psu=_psu,
                ssu=_ssu,
                fpc=fpc,
                coef_var=coef_var,
                single_psu=single_psu,
                strata_comb=strata_comb,
            )
            (
                self.point_est,
                self.stderror,
                self.stddev,
                self.lower_ci,
                self.upper_ci,
                self.stats,
            ) = self._two_groups_unpaired(mean_est=two_samples_unpaired, group=group)
            self.group_names = list(self.point_est.keys())
        elif self.samp_type == "two-sample" and self.paired:
            if len(_y.shape) == 1 or _y.shape[1] != 2:
                raise AssertionError(
                    "Parameter y must be an array-like object of dimension n by 2 for two-sample paired T-test"
                )
            d = _y[:, 0] - _y[:, 1]
            self._one_sample_one_group(
                y=d,
                known_mean=0,
                samp_weight=_samp_weight,
                stratum=_stratum,
                psu=_psu,
                ssu=_ssu,
                fpc=fpc,
            )
        else:
            raise ValueError("type must be equal to 'one-sample', 'two-sample'!")

    def to_dataframe(
        self,
    ) -> pd.DataFrame:

        if (self.samp_type == "one-sample" and self.group_names == []) or self.paired:
            return pd.DataFrame(
                data={
                    "Nb. Obs": [self.stats["number_obs"]],
                    PopParam.mean: [self.point_est],
                    "Std. Error": [self.stderror],
                    "Std. Dev.": [self.stddev],
                    "Lower CI": [self.lower_ci],
                    "Upper CI": [self.upper_ci],
                }
            )
        else:
            groups = list(self.point_est.keys())
            return pd.DataFrame(
                data={
                    "Group": groups,
                    "Nb. Obs": list(self.stats["number_obs"].values()),
                    PopParam.mean: list(self.point_est.values()),
                    "Std. Error": list(self.stderror.values()),
                    "Std. Dev.": list(self.stddev.values()),
                    "Lower CI": list(self.lower_ci.values()),
                    "Upper CI": list(self.upper_ci.values()),
                }
            )
