from typing import Callable
from scipy import integrate, linalg


def generate_points(
    left_bound: float, right_bound: float, n_of_points: int
) -> tuple[float, list[float]]:
    difference = (right_bound - left_bound) / (n_of_points - 1)
    return difference, [left_bound + i * difference for i in range(n_of_points)]


def generate_mu_n(rho: Callable[[float], float], n: int):
    return lambda x: rho(x) * x**n


def create_interpolating_quadrature(
    left_bound: float,
    right_bound: float,
    n_of_points: int,
    f: Callable[[float], float],
    rho: Callable[[float], float],
) -> float:
    difference, xs = generate_points(left_bound, right_bound, n_of_points)
    print("Используемые узлы:")
    for i, x in enumerate(xs):
        print(f"x_{i} = {x}")

    mus = list(
        map(
            lambda x: integrate.quad(x, left_bound, right_bound),
            [generate_mu_n(rho, i) for i in range(n_of_points)],
        )
    )
    print("Найденные моменты весовой функции:")
    for i, mu in enumerate(mus):
        print(f"μ_{i} = {mu[0]}")

    xs_matrix = [list(map(lambda elem: elem**i, xs)) for i in range(n_of_points)]
    mus_cut = [mu[0] for mu in mus]
    a_s = linalg.solve(xs_matrix, mus_cut)
    print("Найденные коэффициенты ИКФ:")
    for i, a in enumerate(a_s):
        print(f"A_{i} = {a}")

    return sum(map(lambda ax: ax[0] * f(ax[1]), zip(a_s, xs)))
