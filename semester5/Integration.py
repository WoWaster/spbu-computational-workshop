from typing import Callable, Any
from scipy import integrate, linalg
from numpy.polynomial import Polynomial
import numpy.typing as npt


def generate_points(
    left_bound: float, right_bound: float, n_of_points: int
) -> tuple[float, list[float]]:
    difference = (right_bound - left_bound) / (n_of_points - 1)
    return difference, [left_bound + i * difference for i in range(n_of_points)]


def generate_mu_n(
    rho: Callable[[float], float], n: int
) -> Callable[[Callable[[float], float], int], float]:
    return lambda x: rho(x) * x**n


def create_quadrature_max_algebraic_accuracy(
    left_bound: float,
    right_bound: float,
    n_of_points: int,
    f: Callable[[float], float],
    rho: Callable[[float], float],
) -> float:
    print("Вычисление КФНАСТ")
    mus = list(
        map(
            lambda x: integrate.quad(x, left_bound, right_bound)[0],
            [generate_mu_n(rho, i) for i in range(n_of_points)],
        )
    )
    mus_add = list(
        map(
            lambda x: integrate.qTaskjuad(x, left_bound, right_bound)[0],
            [generate_mu_n(rho, i) for i in range(n_of_points, 2 * n_of_points)],
        )
    )
    mus_all = mus + mus_add
    mu_matrix = [mus_all[i : n_of_points + i] for i in range(n_of_points)]
    minus_mus_add = list(map(lambda x: (-1) * x, mus_add))
    coefficients = list(map(float, linalg.solve(mu_matrix, minus_mus_add)))
    omega = Polynomial(coefficients + [1])

    print("Найденные моменты весовой функции:")
    for i, mu in enumerate(mus_all):
        print(f"μ_{i} = {mu}")

    print(f"Найденный ортогональный многочлен\nω = {omega}")

    xs = omega.roots()
    print("Найденные узлы:")
    for i, x in enumerate(xs):
        print(f"x_{i} = {x}")

    xs_matrix = [list(map(lambda elem: elem**i, xs)) for i in range(n_of_points)]
    a_s = linalg.solve(xs_matrix, mus)
    print("Найденные коэффициенты КФНАСТ:")
    for i, a in enumerate(a_s):
        print(f"A_{i} = {a}")

    return sum(map(lambda ax: ax[0] * f(ax[1]), zip(a_s, xs)))


def create_interpolating_quadrature(
    left_bound: float,
    right_bound: float,
    n_of_points: int,
    f: Callable[[float], float],
    rho: Callable[[float], float],
) -> float:
    print("Вычисление ИКФ")
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


def generate_legendre_polys(n: int) -> list[Polynomial]:
    polys = [Polynomial([1]), Polynomial([0, 1])]
    if n == 0:
        return polys[:1]
    elif n == 1:
        return polys
    else:
        for i in range(2, n + 1):
            p_i = (2 * i - 1) / i * Polynomial([0, 1]) * polys[i - 1] - (
                i - 1
            ) / i * polys[i - 2]
            polys.append(p_i)
    return polys


def generate_gauss_coefficients(
    n: int,
    left_bound: float,
    right_bound: float,
    poly: Polynomial,
    roots: npt.NDArray[Any],
) -> list[float]:
    coefficients: list[float] = []
    for i, root in enumerate(roots):
        c = 2 * (1 - root**2) / n**2 / (poly(root) ** 2)
        coefficients.append((right_bound - left_bound) / 2 * c)
    return coefficients


def calculate_gauss(
    n: int, left_bound: float, right_bound: float, f: Callable[[float], float]
) -> float:
    polys = generate_legendre_polys(n)
    roots = polys[n].roots()
    coefficients = generate_gauss_coefficients(
        n, left_bound, right_bound, polys[n - 1], roots
    )
    points = map(
        lambda t: (right_bound - left_bound) / 2 * t + (left_bound + right_bound) / 2,
        roots,
    )
    val = 0
    for c, x in zip(coefficients, points):
        val += c * f(x)
    return val
