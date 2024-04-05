import numpy as np
import pandas as pd
from scipy import linalg


def print_matrix(matrix: np.ndarray, eps=10e-6) -> None:
    m = pd.DataFrame(matrix)
    m.columns = [""] * m.shape[1]
    print(m.to_string(index=False))


def is_diagonally_dominant(matrix: np.ndarray) -> bool:
    abs_x = np.abs(matrix)
    return np.all(2 * np.diag(abs_x) >= np.sum(abs_x, axis=1))


def is_positive_definite(matrix: np.ndarray) -> bool:
    return np.all(linalg.eigvals(matrix) > 0)
