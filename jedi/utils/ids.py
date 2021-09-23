import csv
from typing import Optional, Set

import pandas as pd


def ids_from_file(filename: str, id_col: str, sep: Optional[str] = '\t') -> Set[int]:
    return set(
        pd.read_csv(filename, sep=sep, quoting=csv.QUOTE_NONE, dtype={id_col: 'int64'}, usecols=[id_col])[id_col])


def subset_ids_from_file(filename: str, set_num: int, total_sets: int, id_col: str, sep: Optional[str] = '\t')\
        -> Set[int]:
    return {x for x in ids_from_file(filename, id_col, sep) if x % total_sets == set_num}
