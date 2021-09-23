from collections import defaultdict
import csv
from enum import Enum, auto
import os


class StorageType(Enum):
    CONTINUOUS = auto()
    CATEGORICAL_INDEX = auto()
    CATEGORICAL_FLAG = auto()
    CATEGORICAL_FROM_FLOAT = auto()
    ONE_HOT = auto()
    STRING = auto()
    BYTE_STRING = auto()


STRING_TO_TYPE = {
    'age': StorageType.STRING,
    'float': StorageType.CONTINUOUS,
    'index': StorageType.CATEGORICAL_INDEX,
    'flag': StorageType.CATEGORICAL_FLAG,
    'float_index': StorageType.CATEGORICAL_FROM_FLOAT,
    'hot': StorageType.ONE_HOT,
    'string': StorageType.STRING,
    'byte': StorageType.BYTE_STRING
}

ID = ''  # NB: replace with id field name

EXAMPLE_DATA_TYPE_FILE = os.path.join(os.path.dirname(__file__), 'example_data_types.txt')


def read_data_types_from_file():
    dtype_map = defaultdict(dict)
    with open(EXAMPLE_DATA_TYPE_FILE, 'r') as dtype_file:
        reader = csv.reader(dtype_file, delimiter='\t')
        next(reader)
        for row in reader:
            dtype_map[row[0]][row[1]] = STRING_TO_TYPE[row[2]]
    return dtype_map


def read_data_class_from_file():
    dtype_map = defaultdict(dict)
    with open(EXAMPLE_DATA_TYPE_FILE, 'r') as dtype_file:
        reader = csv.reader(dtype_file, delimiter='\t')
        next(reader)
        for row in reader:
            dtype_map[row[0]][row[1]] = row[2]
    return dtype_map


EXAMPLE_DATA_TYPE_MAP = read_data_types_from_file()
EXAMPLE_DATA_CLASS_MAP = read_data_class_from_file()

TEST_DATA_PATH = os.path.join(os.path.dirname(__file__), '../../test_data/jedi_{}/')

DATA_PATH_MAP = {
    'test_data': TEST_DATA_PATH,
    # add additional paths here
}
