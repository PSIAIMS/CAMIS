"""computes Hadamard matrices.
A Hadamard matrix is a square matrix whose entries are either +1 or −1 and whose rows are mutually orthogonal.The Hadamard matrix is used to derive the BRR replicate weights. It is conjectured that
a Hadamard matrix exist for all n divisible by 4. However, the *hadarmard(n)* functions from
*scipy.linalg* only provides the matrix for n that are power of 2. Hence, in this module,
additional Hadamard matrices are implemented. For example, *scipy.linalg.hadamard()* can provide a matrix for n = 4, 8, 16, 32, 64, 128, etc. The module add Hadamard matrices for n = 12, 20, 24, 28, and some additional to come.

In appendix A, Wolter, K. M. (1985) [#w1985]_ provides a list of Hadamard matrices for all
n multiple of 4 up to 100 which should be sufficient for most applications. Note that this is
this reference is for the first edition of the book which explicitly provides the hadamard
matrices. Above that, the scipy function can be used. Also, more Hadamard matrices can be
found at: http://neilsloane.com/hadamard/

.. [#w1985] Wolter., K. M. (1985), *Introduction to variance Estimation*, Springer-Verlag New York, Inc

TODO: implements Hadamard matrices of order higher than 28.

"""

import math

import numpy as np

from scipy.linalg import hadamard as hdd


def hadamard(n: int) -> np.ndarray:

    n_log2 = int(math.log(n, 2))
    if math.pow(2, n_log2) == n:
        return np.asarray(hdd(n))
    elif n % 4 == 0:
        hadamard_run = "_hadamard" + str(n) + "()"
        return np.asarray(eval(hadamard_run))
    else:
        raise ValueError("n is not valid!")


def _hadamard2() -> np.ndarray:

    hadamard2 = np.ones((2, 2))
    hadamard2[1, 1] = -1

    return hadamard2


def _hadamard12() -> np.ndarray:

    hadamard12 = np.ones((12, 12))

    for c in [1, 3, 7, 8, 9, 11]:
        hadamard12[1, c] = -1
    for c in [1, 2, 4, 8, 9, 10]:
        hadamard12[2, c] = -1
    for c in [2, 3, 5, 9, 10, 11]:
        hadamard12[3, c] = -1
    for c in [1, 3, 4, 6, 10, 11]:
        hadamard12[4, c] = -1
    for c in [1, 2, 4, 5, 7, 11]:
        hadamard12[5, c] = -1
    for c in [1, 2, 3, 5, 6, 8]:
        hadamard12[6, c] = -1
    for c in [2, 3, 4, 6, 7, 9]:
        hadamard12[7, c] = -1
    for c in [3, 4, 5, 7, 8, 10]:
        hadamard12[8, c] = -1
    for c in [4, 5, 6, 8, 9, 11]:
        hadamard12[9, c] = -1
    for c in [1, 5, 6, 7, 9, 10]:
        hadamard12[10, c] = -1
    for c in [2, 6, 7, 8, 10, 11]:
        hadamard12[11, c] = -1

    return hadamard12


def _hadamard20() -> np.ndarray:

    hadamard20 = np.ones((20, 20))

    row = np.array([1, 3, 4, 9, 11, 13, 14, 15, 16, 19])
    for r in range(1, 20):
        for c in row:
            hadamard20[r, c] = -1
        row = (row + 1) % 20
        if row[9] == 0:
            row[9] = 1
            row = np.sort(row)

    return hadamard20


def _hadamard24() -> np.ndarray:

    hadamard24 = np.ones((24, 24))

    row1_c1 = np.array([1, 3, 7, 8, 9, 11])
    row1_c2 = np.array([13, 15, 19, 20, 21, 23])
    for r1 in range(0, 11):
        for c1 in row1_c1:
            hadamard24[r1 + 1, c1] = -1
        row1_c1 = (row1_c1 + 1) % 12
        if row1_c1[5] == 0:
            row1_c1[5] = 1
            row1_c1 = np.sort(row1_c1)

        for c2 in row1_c2:
            hadamard24[r1 + 1, c2] = -1
        row1_c2 = (row1_c2 + 1) % 24
        if row1_c2[5] == 0:
            row1_c2[5] = 13
            row1_c2 = np.sort(row1_c2)

    row2_c1 = np.array([1, 3, 7, 8, 9, 11])
    row2_c2 = np.array([14, 16, 17, 18, 22])
    for r2 in range(12, 23):
        for c1 in row2_c1:
            hadamard24[r2 + 1, c1] = -1
        row2_c1 = (row2_c1 + 1) % 12
        if row2_c1[5] == 0:
            row2_c1[5] = 1
            row2_c1 = np.sort(row2_c1)

        for c2 in row2_c2:
            hadamard24[r2 + 1, c2] = -1
        row2_c2 = (row2_c2 + 1) % 24
        if row2_c2[4] == 0:
            row2_c2[4] = 13
            row2_c2 = np.sort(row2_c2)

    hadamard24[12:24, 12] = -1
    hadamard24[12, 12:24] = -1

    return hadamard24


def _hadamard28() -> np.ndarray:

    hadamard28 = np.ones((28, 28))

    hadamard28[0, 1] = -1

    for c in [0, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27]:
        hadamard28[1, c] = -1
    for c in [3, 6, 7, 12, 13, 14, 15, 16, 17, 18, 19, 24, 25]:
        hadamard28[2, c] = -1
    for c in [1, 2, 3, 5, 6, 9, 11, 12, 14, 16, 18, 21, 23, 24, 27]:
        hadamard28[3, c] = -1
    for c in [5, 8, 9, 14, 15, 16, 17, 18, 19, 20, 21, 26, 27]:
        hadamard28[4, c] = -1
    for c in [1, 3, 4, 5, 7, 8, 11, 13, 14, 16, 18, 20, 23, 25, 26]:
        hadamard28[5, c] = -1
    for c in [2, 3, 7, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23]:
        hadamard28[6, c] = -1
    for c in [1, 2, 5, 6, 7, 9, 10, 13, 15, 16, 18, 20, 22, 25, 27]:
        hadamard28[7, c] = -1
    for c in [4, 5, 9, 12, 13, 18, 19, 20, 21, 22, 23, 24, 25]:
        hadamard28[8, c] = -1
    for c in [1, 3, 4, 7, 8, 9, 11, 12, 15, 17, 18, 20, 22, 24, 27]:
        hadamard28[9, c] = -1
    for c in [6, 7, 11, 14, 15, 20, 21, 22, 23, 24, 25, 26, 27]:
        hadamard28[10, c] = -1
    for c in [1, 3, 5, 6, 9, 10, 11, 13, 14, 17, 19, 20, 22, 24, 26]:
        hadamard28[11, c] = -1
    for c in [2, 3, 8, 9, 13, 16, 17, 22, 23, 24, 25, 26, 27]:
        hadamard28[12, c] = -1
    for c in [1, 2, 5, 7, 8, 11, 12, 13, 15, 16, 19, 21, 22, 24, 26]:
        hadamard28[13, c] = -1
    for c in [2, 3, 4, 5, 10, 11, 15, 18, 19, 24, 25, 26, 27]:
        hadamard28[14, c] = -1
    for c in [1, 2, 4, 7, 9, 10, 13, 14, 15, 17, 18, 21, 23, 24, 26]:
        hadamard28[15, c] = -1
    for c in [2, 3, 4, 5, 6, 7, 12, 13, 17, 20, 21, 26, 27]:
        hadamard28[16, c] = -1
    for c in [1, 2, 4, 6, 9, 11, 12, 15, 16, 17, 19, 20, 23, 25, 26]:
        hadamard28[17, c] = -1
    for c in [2, 3, 4, 5, 6, 7, 8, 9, 14, 15, 19, 22, 23]:
        hadamard28[18, c] = -1
    for c in [1, 2, 4, 6, 8, 11, 13, 14, 17, 18, 19, 21, 22, 25, 27]:
        hadamard28[19, c] = -1
    for c in [4, 5, 6, 7, 8, 9, 10, 11, 16, 17, 21, 24, 25]:
        hadamard28[20, c] = -1
    for c in [1, 3, 4, 6, 8, 10, 13, 15, 16, 19, 20, 21, 23, 24, 27]:
        hadamard28[21, c] = -1
    for c in [6, 7, 8, 9, 10, 11, 12, 13, 18, 19, 23, 26, 27]:
        hadamard28[22, c] = -1
    for c in [1, 3, 5, 6, 8, 10, 12, 15, 17, 18, 21, 22, 23, 25, 26]:
        hadamard28[23, c] = -1
    for c in [2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 20, 21, 25]:
        hadamard28[24, c] = -1
    for c in [1, 2, 5, 7, 8, 10, 12, 14, 17, 19, 20, 23, 24, 25, 27]:
        hadamard28[25, c] = -1
    for c in [4, 5, 10, 11, 12, 13, 14, 15, 16, 17, 22, 23, 27]:
        hadamard28[26, c] = -1
    for c in [1, 3, 4, 7, 9, 10, 12, 14, 16, 19, 21, 22, 25, 26, 27]:
        hadamard28[27, c] = -1

    return hadamard28
