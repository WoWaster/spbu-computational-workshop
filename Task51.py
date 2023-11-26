from math import cos, sin
from scipy import integrate
from prettytable import PrettyTable

from Integration import create_interpolating_quadrature


def rho(x: float) -> float:
    return cos(x)


def f(x: float) -> float:
    return sin(x)


def is_float(element: any) -> bool:
    # If you expect None to be passed:
    if element is None:
        return False
    try:
        float(element)
        return True
    except ValueError:
        return False


def is_int(element: any) -> bool:
    # If you expect None to be passed:
    if element is None:
        return False
    try:
        int(element)
        return True
    except ValueError:
        return False


def input_float(msg: str) -> float:
    while True:
        left_bound_str = input(msg)
        if is_float(left_bound_str):
            return float(left_bound_str)
        else:
            print("Введено не вещественное число")


def input_int(msg: str) -> int:
    while True:
        left_bound_str = input(msg)
        if is_float(left_bound_str):
            return int(left_bound_str)
        else:
            print("Введено не целое число")


if __name__ == "__main__":
    print(
        "Тема: Приближённое вычисление интегралов при помощи квадратурных "
        "формул Наивысшей Алгебраической Степени "
        "Точности (КФ НАСТ)"
    )
    left_bound = input_float("Введите левую границу интегрирования: ")
    right_bound = input_float("Введите левую границу интегрирования: ")
    n = input_int("Введите количество точек для построения КФ: ")
    print("ρ = cos(x)")
    print("f(x) = sin(x)")

    preciseF = integrate.quad(lambda x: rho(x) * f(x), left_bound, right_bound)
    i_quadF = create_interpolating_quadrature(left_bound, right_bound, n, f, rho)

    tableF = PrettyTable()
    tableF.field_names = ["Метод", "Значение", "Погрешность"]
    tableF.add_row(["Точное значение (SciPy)", preciseF[0], preciseF[1]])
    tableF.add_row(["ИКФ", i_quadF, abs(i_quadF - preciseF[0])])
    print(tableF)

    print(f"{80*'='}")
    print(f"f(x) = x^{n - 1}")
    preciseN = integrate.quad(
        lambda x: rho(x) * (x ** (n - 1)), left_bound, right_bound
    )
    i_quadN = create_interpolating_quadrature(
        left_bound, right_bound, n, lambda x: x ** (n - 1), rho
    )

    tableN = PrettyTable()
    tableN.field_names = ["Метод", "Значение", "Погрешность"]
    tableN.add_row(["Точное значение (SciPy)", preciseN[0], preciseN[1]])
    tableN.add_row(["ИКФ", i_quadN, abs(i_quadN - preciseN[0])])
    print(tableN)
