import numpy as np
import numpy.typing as npt
import numdifftools as nd
from scipy import linalg


def minimize_gradient(
    f: callable, x0: npt.NDArray[float], alpha: float, eps: float
) -> tuple[npt.NDArray[float], list[npt.NDArray[float]], int]:
    counter = 0
    norm = 0
    df = nd.Gradient(f)

    xs = [x0]

    while counter == 0 or norm >= eps:
        x0 = x0 - alpha * df(x0)
        xs.append(x0)
        norm = linalg.norm(df(x0), ord=np.inf)
        counter += 1

    return x0, xs, counter


def minimize_heavy_ball(
    f: callable, x0: npt.NDArray[float], alpha: float, beta: float, eps: float
) -> tuple[npt.NDArray[float], list[npt.NDArray[float]], int]:
    counter = 0
    norm = 0
    df = nd.Gradient(f)

    xs = [x0]
    prev_x = x0
    x0 = x0 - alpha * df(x0)
    xs.append(x0)
    norm = linalg.norm(df(x0), ord=np.inf)
    counter += 1

    while counter == 1 or norm >= eps:
        x = x0
        x0 = x0 - alpha * df(x0) + beta * (x0 - prev_x)
        prev_x = x
        xs.append(x0)
        norm = linalg.norm(df(x0), ord=np.inf)
        counter += 1

    return x0, xs, counter


def minimize_nesterov(
    f: callable, x0: npt.NDArray[float], alpha: float, beta: float, eps: float
) -> tuple[npt.NDArray[float], list[npt.NDArray[float]], int]:
    counter = 0
    norm = 0
    df = nd.Gradient(f)

    y = x0
    xs = [x0]

    while counter == 0 or norm >= eps:
        x = x0
        x0 = y - alpha * df(y)
        y = x0 + beta * (x0 - x)
        xs.append(y)
        norm = linalg.norm(df(y), ord=np.inf)
        counter += 1

    return y, xs, counter


def minimize_newton(
    f: callable, x0: npt.NDArray[float], eps: float
) -> tuple[npt.NDArray[float], list[npt.NDArray[float]], int]:
    counter = 0
    norm = 0
    df = nd.Gradient(f)
    hs = nd.Hessian(f)

    xs = [x0]
    while counter == 0 or norm >= eps:
        h = hs(x0)
        x0 = linalg.solve(h, h @ x0 - df(x0))
        xs.append(x0)
        norm = linalg.norm(df(x0), ord=np.inf)
        counter += 1

    return x0, xs, counter
