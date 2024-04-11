from enum import Enum, auto
from math import sqrt
from typing import Union

import numpy as np
from numpy.linalg import LinAlgError
from scipy import linalg

from semester6 import utils


class PartialEigenproblemMethod(Enum):
    PowerMethod = auto()
    DotProductMethod = auto()


def solve(a: np.nd)