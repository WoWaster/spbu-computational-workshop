from math import cos, sin
from scipy import integrate
from prettytable import PrettyTable

from Integration import (
    create_interpolating_quadrature,
    create_quadrature_max_algebraic_accuracy,
)


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


def print_results_int_quad() -> None:
    print(
        "Тема: Приближённое вычисление интегралов при помощи квадратурных "
        "формул Наивысшей Алгебраической Степени "
        "Точности (КФ НАСТ)"
    )
    left_bound = input_float("Введите левую границу интегрирования: ")
    right_bound = input_float("Введите левую границу интегрирования: ")
    n = input_int("Введите количество точек для построения КФ: ")

    if n < 2:
        print("Количество узлов обязано быть больше 2")
        print_results_int_quad()

    print("ρ = cos(x)")
    print("f(x) = sin(x)")

    precise_f = integrate.quad(lambda x: rho(x) * f(x), left_bound, right_bound)
    i_quad_f = create_interpolating_quadrature(left_bound, right_bound, n, f, rho)
    quad_max_alg_acc = create_quadrature_max_algebraic_accuracy(
        left_bound, right_bound, n, f, rho
    )

    table_f = PrettyTable()
    table_f.field_names = ["Метод", "Значение", "Погрешность"]
    table_f.add_row(["Точное значение (SciPy)", precise_f[0], precise_f[1]])
    table_f.add_row(["ИКФ", i_quad_f, abs(i_quad_f - precise_f[0])])
    table_f.add_row(["КФНАСТ", quad_max_alg_acc, abs(quad_max_alg_acc - precise_f[0])])
    print(table_f)

    print(f"{80*'='}")
    print(f"f(x) = x^{n - 1}")
    precise_n = integrate.quad(
        lambda x: rho(x) * (x ** (n - 1)), left_bound, right_bound
    )
    i_quad_n = create_interpolating_quadrature(
        left_bound, right_bound, n, lambda x: x ** (n - 1), rho
    )
    quad_max_alg_acc_n = create_quadrature_max_algebraic_accuracy(
        left_bound, right_bound, n, lambda x: x ** (n - 1), rho
    )

    table_n = PrettyTable()
    table_n.field_names = ["Метод", "Значение", "Погрешность"]
    table_n.add_row(["Точное значение (SciPy)", precise_n[0], precise_n[1]])
    table_n.add_row(["ИКФ", i_quad_n, abs(i_quad_n - precise_n[0])])
    table_n.add_row(["КФНАСТ", quad_max_alg_acc_n, abs(quad_max_alg_acc_n - precise_n[0])])
    print(table_n)
    print("Хотите повторить с другими данными?")
    again = input("[Да, да, д, y, yes], чтобы продолжить: ")
    if again in ["Да", "да", "д", "y", "yes"]:
        print_results_int_quad()


if __name__ == "__main__":
    print_results_int_quad()
