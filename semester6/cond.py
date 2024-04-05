import numpy as np
from scipy import linalg


def spectral(matrix: np.ndarray) -> float:
    try:
        inverse = linalg.inv(matrix)
    except linalg.LinAlgError:
        return np.inf
    return linalg.norm(matrix) * linalg.norm(inverse)


def volume(matrix: np.ndarray) -> float:
    det = linalg.det(matrix)
    row_norms = linalg.norm(matrix, axis=1)
    return np.prod(row_norms) / abs(det)


def angle(matrix: np.ndarray) -> float:
    try:
        inverse = linalg.inv(matrix)
    except linalg.LinAlgError:
        return np.inf

    row_norms = linalg.norm(matrix, axis=1)
    column_norms = linalg.norm(inverse, axis=0)

    return np.max(np.multiply(row_norms, column_norms))
