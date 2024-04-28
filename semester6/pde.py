from dataclasses import dataclass
from enum import Enum, auto
from typing import Callable

import numpy as np
from scipy import sparse
import numpy.typing as npt


@dataclass
class Pde:
    r"""PDE in form of
    u_t(x, t) = kappa * u_xx(x, t) + f(x, t)
    kappa > 0, 0 < x < a, 0 < t < T
    u(x, 0) = mu(x), 0 <= x <= a
    u(0, t) = mu1(t), 0 <= t <= T
    u(a, t) = mu2(t), 0 <= t <= T
    """

    kappa: float
    f: Callable[[float, float], float]
    a: float
    t_big: float
    mu: Callable[[float], float]
    mu1: Callable[[float], float]
    mu2: Callable[[float], float]


class FdmMethod(Enum):
    Explicit = auto()
    Implicit = auto()


def _solve_explicit(
    pde: Pde, n: int, m: int
) -> tuple[npt.NDArray[float], npt.NDArray[float], npt.NDArray[float]]:
    xs = np.linspace(0, pde.a, n + 1)
    ts = np.linspace(0, pde.t_big, m + 1)

    h = pde.a / n
    t = pde.t_big / m

    approx = sparse.dok_array((m + 1, n + 1))  # row -- time, column -- x

    for i in range(n + 1):  # t=0 boundary
        approx[0, i] = pde.mu(xs[i])

    for j in range(1, m + 1):  # x=0 and x=a boundaries
        approx[j, 0] = pde.mu1(ts[j])
        approx[j, n] = pde.mu2(ts[j])

    for j in range(1, m + 1):
        for i in range(1, n):
            r = t * pde.kappa / h**2
            approx[j, i] = (
                r * approx[j - 1, i - 1]
                + (1 - 2 * r) * approx[j - 1, i]
                + r * approx[j - 1, i + 1]
                + t * pde.f(xs[i], ts[j - 1])
            )

    return xs, ts, approx.toarray()


def _solve_implicit(
    pde: Pde, n: int, m: int
) -> tuple[npt.NDArray[float], npt.NDArray[float], npt.NDArray[float]]:
    xs = np.linspace(0, pde.a, n + 1)
    ts = np.linspace(0, pde.t_big, m + 1)

    h = pde.a / n
    t = pde.t_big / m

    a = sparse.dok_array((n + 1, n + 1))
    a[0, 0] = 1
    for i in range(1, n):
        a[i, i - 1] = -t * pde.kappa / h**2
        a[i, i] = 2 * t * pde.kappa / h**2 + 1
        a[i, i + 1] = -t * pde.kappa / h**2
    a[n, n] = 1
    a = a.tocsr()

    approx = sparse.dok_array((m + 1, n + 1))  # row -- time, column -- x

    for i in range(n + 1):  # t=0 boundary
        approx[0, i] = pde.mu(xs[i])

    for j in range(1, m + 1):  # x=0 and x=a boundaries
        approx[j, 0] = pde.mu1(ts[j])
        approx[j, n] = pde.mu2(ts[j])

    for j in range(1, m + 1):  # time slice
        b = sparse.dok_array((n + 1, 1))
        b[0] = pde.mu1(ts[j])
        for i in range(1, n):
            b[i] = approx[j - 1, i] + t *pde.f(xs[i], ts[j])
        b[n] = pde.mu2(ts[j])

        new_approx = sparse.linalg.spsolve(a, b.tocsr())
        approx[j] = new_approx

    return xs, ts, approx.toarray()


def solve_heat_fdm(
    pde: Pde, n: int, m: int, method: FdmMethod
) -> tuple[npt.NDArray[float], npt.NDArray[float], npt.NDArray[float]]:
    match method:
        case FdmMethod.Explicit:
            return _solve_explicit(pde, n, m)
        case FdmMethod.Implicit:
            return _solve_implicit(pde, n, m)
