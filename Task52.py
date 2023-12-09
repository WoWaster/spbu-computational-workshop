from math import sqrt, cos
from typing import Callable

from numpy.polynomial import Polynomial
from prettytable import PrettyTable
from scipy import integrate

from Integration import (
    generate_gauss_coefficients,
    generate_legendre_polys,
    calculate_gauss,
)
from Task51 import input_float


def show_gauss_of_n(n: int, polys: list[Polynomial]) -> None:
    roots = polys[n].roots()
    coefficients = generate_gauss_coefficients(n, -1, 1, polys[n - 1], roots)
    print(f"N={n}")
    table = PrettyTable()
    table.add_column("x_k", roots)
    table.add_column("C_k", coefficients)
    print(table, end="\n\n")


def show_canonical_gauss() -> None:
    n = 8
    polys = generate_legendre_polys(n)

    print("Узлы и коэффициенты КФ Гаусса при N = 1...8 на [-1, 1]", end="\n\n")
    for i in range(1, n + 1):
        show_gauss_of_n(i, polys)


def test_canonical_gauss() -> None:
    n = 5
    table = PrettyTable()
    table.field_names = [
        "Многочлен",
        "Точное значение",
        "Вычисленное значение",
        "Погрешность",
    ]
    print("Проверка точности КФ Гаусса на [-1, 1]", end="\n\n")
    for i in range(n - 2, n + 1):
        j = 2 * i - 1

        val = calculate_gauss(i, -1, 1, (lambda x: x**j))
        table.add_row([f"x^{j}", 0, val, abs(0 - val)])
    print(table, end="\n\n")


def integrate_gauss(f: Callable[[float], float], ns: list[int]) -> None:
    print("Вычисление интеграла от f(x) = sqrt(x) * cos(x^2)", end="\n\n")

    left_bound = input_float("Введите левую границу интегрирования: ")
    right_bound = input_float("Введите правую границу интегрирования: ")

    real_val = integrate.quad(f, left_bound, right_bound)

    for i in ns:
        polys = generate_legendre_polys(i)
        roots = polys[i].roots()
        coefficients = generate_gauss_coefficients(
            i, left_bound, right_bound, polys[i - 1], roots
        )
        points = map(
            lambda t: (right_bound - left_bound) / 2 * t
            + (left_bound + right_bound) / 2,
            roots,
        )

        gauss_val = calculate_gauss(i, left_bound, right_bound, f)
        table = PrettyTable()
        table.field_names = ["Метод", "Значение", "Погрешность"]
        table.add_row(["Точное значение (SciPy)", real_val[0], real_val[1]])
        table.add_row(["КФ Гаусса", gauss_val, abs(gauss_val - real_val[0])])
        print(f"N={i}")
        print("Узлы и коэффициенты")
        print(list(zip(points, coefficients)))
        print(table, end="\n\n")


if __name__ == "__main__":
    print("Вычисление интегралов при помощи КФ Гаусса", end="\n\n")

    show_canonical_gauss()
    test_canonical_gauss()

    def f(x: float) -> float:
        return sqrt(x) * cos(x**2)

    ns = [6, 7, 8]
    integrate_gauss(f, ns)
