import numpy as np

from samplics.utils.types import DictStrBool, DictStrInt


def _grs_select(
    probs: np.ndarray,
    samp_unit: np.ndarray,
    samp_size: DictStrInt | int,
    stratum: np.ndarray,
    wr: DictStrBool | bool,
) -> tuple[np.ndarray, np.ndarray]:

    sample = np.zeros(samp_unit.size).astype(bool)
    hits = np.zeros(samp_unit.size).astype(int)

    if stratum.shape in ((), (0,)):
        sampled_indices = np.random.choice(
            a=samp_unit.size,
            size=samp_size,
            replace=wr,
            p=probs / np.sum(probs),
        )
    else:
        sampled_indices_list = []
        for s in samp_size:
            units_s = stratum == s
            sampled_indices_s = np.random.choice(
                a=samp_unit[units_s],
                size=samp_size[s],
                replace=wr,
                p=probs[units_s] / np.sum(probs[units_s]),
            )
            sampled_indices_list.append(sampled_indices_s)
        sampled_indices = np.array([val for sublist in sampled_indices_list for val in sublist]).flatten()

    indices_s, hits_s = np.unique(sampled_indices, return_counts=True)
    sample[indices_s] = True
    hits[indices_s] = hits_s

    return sample, hits
