from typing import Callable

import numpy as np
from scipy import linalg
import numpy.typing as npt
from dataclasses import dataclass
import scipy.sparse as sparse


@dataclass
class Ode:
    r"""Ode in form of
    -p(x) * u'' + q(x) * u' + r(x) * u = f
    with boundary conditions:
    alpha1 * u(a) - alpha2 * u'(a) = alpha
    beta1 * u(b) + beta2 * u'(b) = beta,
    where
    p(x) >= p0 > 0, r(x) >= 0, for x in [a, b]
    |alpha1| + |alpha2| != 0, alpha1 * alpha2 >= 0
    |beta1| + |beta2| != 0, beta1 * beta2 >= 0
    """

    p: Callable[[float], float]
    q: Callable[[float], float]
    r: Callable[[float], float]
    f: Callable[[float], float]
    a: float
    b: float
    alpha1: float
    alpha2: float
    alpha: float
    beta1: float
    beta2: float
    beta: float


def _gen_a_b(
    ode: Ode, discretization: npt.NDArray[float]
) -> tuple[sparse.csr_array, sparse.csr_array]:
    n = discretization.shape[0]
    a = sparse.dok_array((n, n))
    b = sparse.dok_array((n, 1))
    h = abs(ode.b - ode.a) / (n - 1)

    a[0, 0] = h * ode.alpha1 + 3 / 2 * ode.alpha2
    a[0, 1] = -2 * ode.alpha2
    a[0, 2] = 1 / 2 * ode.alpha2
    b[0] = h * ode.alpha
    for i in range(1, n - 1):
        a[i, i - 1] = -ode.p(discretization[i]) - ode.p(discretization[i]) * h / 2
        a[i, i + 1] = -ode.p(discretization[i]) + ode.p(discretization[i]) * h / 2
        a[i, i] = -(a[i, i - 1] + a[i, i + 1] - h**2 * ode.r(discretization[i]))
        b[i] = h**2 * ode.f(discretization[i])
    a[n - 1, n - 3] = 1 / 2 * ode.beta2
    a[n - 1, n - 2] = -2 * ode.beta2
    a[n - 1, n - 1] = h * ode.beta1 + 3 / 2 * ode.beta2
    b[n - 1] = h * ode.beta

    return a.tocsr(), b.tocsr()


def _richardson(
    approx: npt.NDArray[float], prev_approx: npt.NDArray[float]
) -> npt.NDArray[float]:
    if approx.shape[0] - 1 != 2 * (prev_approx.shape[0] - 1):
        raise Exception("Must be approx.shape-1 == 2 * (prev_approx.shape-1)")

    diff = np.zeros(approx.shape)

    for i in range(prev_approx.shape[0]):
        diff[2 * i] = prev_approx[i]
        if i != (prev_approx.shape[0] - 1):
            diff[2 * i + 1] = (prev_approx[i + 1] + prev_approx[i]) / 2

    diff = (approx - diff) / 3

    return diff


def solve_fdm(
    ode: Ode, n: int, eps: float
) -> tuple[npt.NDArray[float], npt.NDArray[float], tuple[list[int], list[float]]]:
    if abs(ode.alpha1) + abs(ode.alpha2) == 0:
        raise Exception("|alpha1| + |alpha2| = 0")
    if abs(ode.beta1) + abs(ode.beta2) == 0:
        raise Exception("|b1| + |b2| = 0")
    if ode.alpha1 * ode.alpha2 < 0:
        raise Exception("alpha1 * alpha2 < 0")
    if ode.beta1 * ode.beta2 < 0:
        raise Exception("b1 * b2 < 0")

    prev_approx = None
    approx = None
    norm = None
    discretization = None
    ns = []
    norms = []

    while prev_approx is None or norm >= eps:
        if approx is not None:
            prev_approx = approx
        else:
            discretization = np.linspace(
                ode.a, ode.b, n + 1
            )  # For us N is a number of intervals, not points
            (a, b) = _gen_a_b(ode, discretization)
            prev_approx = sparse.linalg.spsolve(a, b)

        n = 2 * n
        discretization = np.linspace(ode.a, ode.b, n + 1)
        (a, b) = _gen_a_b(ode, discretization)
        approx = sparse.linalg.spsolve(a, b)
        diff = _richardson(approx, prev_approx)
        approx = approx + diff
        norm = linalg.norm(diff, ord=np.inf)
        ns.append(n)
        norms.append(norm)

    return discretization, approx, (ns, norms)
