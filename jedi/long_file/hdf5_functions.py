import json
import logging
import os
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Optional, Callable, Any, Iterator, TypeVar, Set, Iterable

import h5py
import numpy as np
import pandas as pd

Instance = h5py.Group
BASELINE = 'baseline'
BASELINE_AGE = ''  # NB: replace with field name for baseline age
AGE = 'age'
YEAR_UNIT_PD = 'y'
WEEK_UNIT_PD = 'W'
DAY_UNIT_PD = 'D'
ID = 'id'

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class FieldType(Enum):
    string = 'string'
    float = 'float'
    int = 'int'
    age = 'age'


T = TypeVar('T')
DEFAULT_SUFFIX = 'no_filter'


@dataclass(frozen=True)
class Field:
    name: str  # the name of the field, e.g. Date_Age
    type: FieldType
    file_type: str
    units: str = None
    column_suffix: Optional[str] = DEFAULT_SUFFIX  # should be provided if there is a filter or function
    filter: Optional[Callable[[T], bool]] = None  # if filter applied to field's value is False, instance is skipped
    function: Optional[Callable[[T], Any]] = None  # function applied to field's value

    def __str__(self) -> str:
        return '.'.join([x for x in [self.file_type, self.name, self.column_suffix] if x is not None])

    def __post_init__(self):
        if self.filter is not None or self.function is not None:
            # If you have a filter or function, you cannot use the default suffix
            assert self.column_suffix != DEFAULT_SUFFIX


def str_from_instance(field_name: str, instance) -> Optional[str]:
    if field_name in instance:
        return instance[field_name][()]


def float_from_instance(field_name: str, instance) -> Optional[float]:
    if field_name in instance:
        return float(instance[field_name][0])


def int_from_instance(field_name: str, instance) -> Optional[int]:
    if field_name in instance:
        return int(instance[field_name][0])


def age_from_instance(field_name: str, instance) -> Optional[int]:
    if field_name in instance:
        return pd.to_timedelta(instance[field_name][()])


def field_from_instance(field: Field, instance) -> Any:
    """If field is not in the instance, will return None"""
    if field.type == FieldType.float:
        return float_from_instance(field.name, instance)
    if field.type == FieldType.string:
        return str_from_instance(field.name, instance)
    if field.type == FieldType.int:
        return int_from_instance(field.name, instance)
    if field.type == FieldType.age:
        return age_from_instance(field.name, instance)
    else:
        raise NotImplementedError(f'Field type {field.type} cannot be read from hd5.')


def instances_from_hd5(hd5: h5py.File, file_type: str) -> Iterator:
    if file_type not in hd5:
        return
    for instance in hd5[file_type]:
        yield hd5[file_type][instance]


def split_file_types(fields: List[Field]) -> Dict[str, List[Field]]:
    file_type_to_field = defaultdict(list)
    for field in fields:
        file_type_to_field[field.file_type].append(field)
    return file_type_to_field


def data_frame_from_hd5(hd5_paths: Iterable[h5py.File], fields: List[Field], id: int,
                        critical_fields: Optional[Set[Field]] = None) -> Optional[pd.DataFrame]:
    if critical_fields is None:
        critical_fields = set()
    file_type_to_field = split_file_types(fields)
    rows = []
    for file_type, file_type_fields in file_type_to_field.items():
        column_names = list(map(str, file_type_fields))
        if len(column_names) > len(set(column_names)):
            raise ValueError(
                f'For file type {file_type}, there are duplicate field names in {column_names}.'
                f'Overlapping columns will overwrite each other.'
            )
        for hd5_path in hd5_paths:
            with h5py.File(hd5_path, 'r') as hd5:
                instances = instances_from_hd5(hd5, file_type)
                rows += list(
                    _data_from_instances(instances, file_type_fields, set(file_type_fields) & critical_fields, id))
    if not rows:
        return
    df = pd.DataFrame(rows)
    df.index = pd.to_timedelta(df[AGE])
    del df[AGE]
    df = df.dropna(how='all')
    if df.empty:
        return
    df[ID] = os.path.splitext(os.path.basename(hd5_paths[0]))[0]
    for field in fields:
        if str(field) not in df:
            df[str(field)] = np.nan
    return df.sort_index()


def _data_from_instances(instances: Iterator[Instance], fields: List[Field], critical_fields: Set[Field], id: int) \
        -> Dict[str, Any]:
    """
    Yields dictionaries {AGE: age, field_1: value_1, ..., field_n: value_n}
    Assumes all fields have file types that match the instances
    """
    critical_fields_empty = len(critical_fields) == 0
    age_field_name = FILE_TYPE_TO_AGE_FIELD[fields[0].file_type].name
    baseline_field_name = _baseline_field(fields[0].file_type).name
    for instance in instances:
        # If critical_fields empty, assume any field is sufficient to yield an instance
        contains_critical = critical_fields_empty
        age = age_from_instance(age_field_name, instance)
        if pd.isnull(age):  # require age
            continue
        baseline = age_from_instance(baseline_field_name, instance)
        field_to_value = {AGE: age, BASELINE: baseline}
        for field in fields:
            value = field_from_instance(field, instance)
            if value is None or (field.filter is not None and not field.filter(value)):
                continue
            if field.function is not None:
                try:
                    value = field.function(value)
                except Exception as err:
                    error_dict = {"id": id, "file": field.file_type, "field": field.name, "error": str(err)}
                    logger.warning(json.dumps(error_dict))

            # Once a critical value is found set contains_critical to True and keep it True
            contains_critical = contains_critical or (value and field in critical_fields)
            field_to_value[str(field)] = value
        else:  # happens if for loop completes with all fields found
            if contains_critical:
                yield field_to_value


FILE_TYPE_TO_AGE_FIELD: Dict[str, Field] = {
    # mappings from file types to Fields representing age fields
    # e.g.
    # file_type_name: Field(age_field_name, FieldType.age, file_type=file_type_name)
}


def _baseline_field(file_type):
    return Field(BASELINE_AGE, FieldType.age, file_type=file_type)
