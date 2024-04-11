from enum import Enum, auto
from math import sqrt
from typing import Union

import numpy as np
from numpy.linalg import LinAlgError
from scipy import linalg

from semester6 import utils


class SolveMethod(Enum):
    QR = auto()
    SimpleIteration = auto()
    Seidel = auto()


def _generate_t_ij(matrix: np.ndarray, i: int, j: int) -> np.ndarray:
    denom = sqrt(matrix[i][i] ** 2 + matrix[j][i] ** 2)
    cos_phi = matrix[i][i] / denom
    sin_phi = -matrix[j][i] / denom

    t_ij = np.identity(matrix.shape[0])
    t_ij[i, i] = cos_phi
    t_ij[i, j] = -sin_phi
    t_ij[j, i] = sin_phi
    t_ij[j, j] = cos_phi

    return t_ij


def qr(matrix: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    n, m = matrix.shape
    r = matrix.copy()
    q = np.identity(n)

    for i in range(0, m):
        for j in range(i + 1, n):
            t_ij = _generate_t_ij(r, i, j)
            r = t_ij @ r
            q = q @ t_ij.T

    for i in range(n):
        if r[i][i] < 0:
            r[i] = -r[i]
            q[:, i] = -q[:, i]

    return q, r


def _solve_qr(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    q, r = qr(a)
    return linalg.inv(r) @ q.T @ b


def _generate_big_b_c_diagonally_dominant(
    a: np.ndarray, b: np.ndarray
) -> tuple[np.ndarray, np.ndarray]:
    big_b = np.zeros(a.shape)
    c = np.zeros(a.shape[0])

    for i in range(a.shape[0]):
        for j in range(a.shape[1]):
            big_b[i][j] = 0 if i == j else -a[i][j] / a[i][i]
        c[i] = b[i] / a[i][i]

    return big_b, c


def _generate_big_b_c_positive_definite(
    a: np.ndarray, b: np.ndarray
) -> tuple[np.ndarray, np.ndarray]:
    eigenvalues = linalg.eigh(a)[0]
    minimum, maximum = (min(eigenvalues), max(eigenvalues))
    alpha = 2 / (maximum + minimum)

    big_b = np.identity(a.shape[0]) - alpha * a
    c = alpha * b
    return big_b, c


def _inner_simple_iteration(
    big_b: np.ndarray,
    c: np.ndarray,
    eps: float = 10e-6,
) -> tuple[np.ndarray, int]:
    norm_big_b = linalg.norm(big_b)

    prev_x = None
    x = c.copy()
    diff = 0
    counter = 0

    while prev_x is None or abs(diff) >= eps:
        prev_x = x
        x = big_b @ x + c
        diff = norm_big_b / (1 - norm_big_b) * linalg.norm(x - prev_x)
        counter += 1

    return x, counter


def _solve_simple_iteration(
    a: np.ndarray,
    b: np.ndarray,
    eps: float = 10e-6,
) -> tuple[np.ndarray, int]:
    if utils.is_diagonally_dominant(a):
        big_b, c = _generate_big_b_c_diagonally_dominant(a, b)
    elif utils.is_positive_definite(a):
        big_b, c = _generate_big_b_c_positive_definite(a, b)
    else:
        raise LinAlgError

    return _inner_simple_iteration(big_b, c, eps)


def _solve_seidel(
    a: np.ndarray,
    b: np.ndarray,
    eps: float = 10e-6,
) -> tuple[np.ndarray, int]:
    d = np.tril(np.triu(a))
    l = np.tril(a) - d
    r = np.triu(a) - d

    imm_matrix = linalg.inv(d + l)

    big_b = (-imm_matrix) @ r
    c = imm_matrix @ b

    return _inner_simple_iteration(big_b, c, eps)


def solve(
    a: np.ndarray,
    b: np.ndarray,
    method: SolveMethod = SolveMethod.QR,
    eps: float = 10e-6,
) -> Union[np.ndarray, tuple[np.ndarray, int]]:
    match method:
        case SolveMethod.QR:
            return _solve_qr(a, b), 0
        case SolveMethod.SimpleIteration:
            return _solve_simple_iteration(a, b, eps)
        case SolveMethod.Seidel:
            return _solve_seidel(a, b, eps)
