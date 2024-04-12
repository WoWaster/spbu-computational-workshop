from enum import Enum, auto
from math import sqrt

import numpy as np
from scipy import linalg


class PartialEigenproblemMethod(Enum):
    PowerMethod = auto()
    DotProductMethod = auto()


def _solve_power_method(a: np.ndarray, eps: float) -> (float, np.ndarray, int):
    eigenvector = np.random.rand(a.shape[1])
    prev_eigenvector = None
    eigenvalue = None
    prev_eigenvalue = None
    counter = 0

    while prev_eigenvalue is None or abs(eigenvalue - prev_eigenvalue) >= eps:
        if eigenvalue is not None:
            prev_eigenvalue = eigenvalue

        prev_eigenvector = eigenvector
        eigenvector = a @ eigenvector

        eigenvalue = sqrt(
            np.dot(eigenvector, eigenvector)
            / np.dot(prev_eigenvector, prev_eigenvector)
        )

        eigenvector = eigenvector / linalg.norm(eigenvector, ord=2)

        counter += 1

    return eigenvalue, eigenvector, counter


def _solve_dot_product_method(a: np.ndarray, eps: float) -> (float, np.ndarray, int):
    eigenvector = np.random.rand(a.shape[1])
    helper_eigenvector = np.random.rand(a.shape[1])
    prev_eigenvector = None
    eigenvalue = None
    prev_eigenvalue = None
    counter = 0

    while prev_eigenvalue is None or abs(eigenvalue - prev_eigenvalue) >= eps:
        if eigenvalue is not None:
            prev_eigenvalue = eigenvalue

        prev_eigenvector = eigenvector
        eigenvector = a @ eigenvector

        eigenvalue = np.dot(eigenvector, helper_eigenvector) / np.dot(
            prev_eigenvector, helper_eigenvector
        )

        eigenvector = eigenvector / linalg.norm(eigenvector, ord=2)

        helper_eigenvector = a.T @ helper_eigenvector
        helper_eigenvector = helper_eigenvector / linalg.norm(helper_eigenvector, ord=2)

        counter += 1

    return eigenvalue, eigenvector, counter


def solve_partial(
    a: np.ndarray,
    method: PartialEigenproblemMethod = PartialEigenproblemMethod.DotProductMethod,
    eps: float = 10e-6,
) -> (float, np.ndarray, int):
    match method:
        case PartialEigenproblemMethod.PowerMethod:
            return _solve_power_method(a, eps)
        case PartialEigenproblemMethod.DotProductMethod:
            return _solve_dot_product_method(a, eps)


class FullEigenproblemMethod(Enum):
    Cyclic = auto()
    Optimal = auto()


def get_r_is(a: np.ndarray) -> np.ndarray:
    rs = np.zeros(a.shape[0])
    for row in range(a.shape[0]):
        s = 0
        for column in range(a.shape[1]):
            s += abs(a[row][column]) if row != column else 0
        rs[row] = s
    return rs


def _check_rs(a: np.ndarray, eps: float) -> bool:
    return np.all(get_r_is(a) < eps)


def _gen_t_ij(a: np.ndarray, i: int, j: int) -> np.ndarray:
    y = a[i][i] - a[j][j]
    if y == 0:
        cos_phi = 1 / sqrt(2)
        sin_phi = 1 / sqrt(2)
    else:
        x = -2 * a[i][j]
        cos_phi = sqrt(1 / 2 * (1 + abs(y) / sqrt(x**2 + y**2)))
        sin_phi = np.sign(x * y) * abs(x) / (2 * cos_phi * sqrt(x**2 + y**2))

    t_ij = np.identity(a.shape[0])
    t_ij[i, i] = cos_phi
    t_ij[i, j] = -sin_phi
    t_ij[j, i] = sin_phi
    t_ij[j, j] = cos_phi

    return t_ij


def _solve_cyclic_method(
    a: np.ndarray, eps: float, max_iters: int
) -> (list[float], int):
    counter = 0

    while True:
        for i in range(a.shape[0]):
            for j in range(a.shape[1]):
                if i == j:
                    continue
                if a[i][j] == 0:
                    continue
                if counter == max_iters:
                    return np.diag(a), counter

                t_ij = _gen_t_ij(a, i, j)
                a = t_ij @ a @ t_ij.T
                counter += 1

                if _check_rs(a, eps):
                    return np.diag(a), counter


def _solve_optimal_method(
    a: np.ndarray, eps: float, max_iters: int
) -> (list[float], int):
    counter = 0
    b = np.zeros(a.shape[0])
    for row in range(a.shape[0]):
        s = 0
        for column in range(a.shape[1]):
            s += a[row][column] ** 2 if row != column else 0
        b[row] = s

    for _ in range(max_iters):
        i = np.argmax(b)
        j = 0 if i != 0 else 1
        maximum = abs(a[i][j])
        for col in range(a.shape[1]):
            if col == i:
                continue

            elem = abs(a[i][col])
            if elem > maximum:
                maximum = elem
                j = col

        t_ij = _gen_t_ij(a, i, j)
        a = t_ij @ a @ t_ij.T
        counter += 1

        if _check_rs(a, eps):
            return np.diag(a), counter

        s = 0
        for column in range(a.shape[1]):
            s += a[i][column] ** 2 if i != column else 0
        b[i] = s

        s = 0
        for column in range(a.shape[1]):
            s += a[j][column] ** 2 if j != column else 0
        b[j] = s

    return np.diag(a), counter


def solve_full(
    a: np.ndarray,
    method: FullEigenproblemMethod = FullEigenproblemMethod.Optimal,
    eps: float = 10e-6,
    max_iters: int = 10_000,
) -> (list[float], int):
    match method:
        case FullEigenproblemMethod.Cyclic:
            return _solve_cyclic_method(a, eps, max_iters)
        case FullEigenproblemMethod.Optimal:
            return _solve_optimal_method(a, eps, max_iters)
