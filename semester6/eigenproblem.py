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
            return  _solve_dot_product_method(a, eps)
