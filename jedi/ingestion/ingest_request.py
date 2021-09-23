import argparse
import csv
import json
import logging
import os
import sys
from collections import defaultdict
from typing import Iterable, List, Mapping, Optional

import h5py
import numpy as np
import pandas as pd

from jedi.ingestion.data_types import StorageType, STRING_TO_TYPE, ID
from jedi.utils.ids import subset_ids_from_file

INVALID = ['#VALUE!', 'UNK', 'N/AN/A', 'N/A', 'NA']
SEP = '|'
TENSOR_EXT = '.hd5'

LOG_COUNT = 1
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


def get_data_file_names(data_dir: str, file_types: Iterable[str]) -> List[Mapping[str, str]]:
    data_dicts = []
    for file_type in file_types:
        data_files = [file for file in os.listdir(data_dir) if file_type in file and not file.startswith('._') and
                      os.path.isfile(os.path.join(data_dir, file))]
        for f in data_files:
            data_dicts.append({'dfile_name': os.path.join(data_dir, f), 'dfile_type': file_type})
    logger.info(json.dumps(data_dicts))
    return data_dicts


def read_data_types_map(data_types_file_name: str) -> Mapping[str, Mapping[str, StorageType]]:
    dtype_map = defaultdict(dict)
    with open(data_types_file_name, 'r') as dtype_file:
        reader = csv.reader(dtype_file, delimiter='\t')
        next(reader)
        for row in reader:
            dtype_map[row[0]][row[1]] = STRING_TO_TYPE[row[2]]
    return dtype_map


def valid_num(value: str) -> bool:
    return not (value == '' or any(invalid in value for invalid in INVALID) or np.isnan(float(value)))


def add_value_to_hd5(hd5: h5py.File, path: str, value: str, storage_type: StorageType, id_: int):
    try:
        if storage_type == StorageType.STRING:
            if pd.isnull(value):
                value = ''
            d = hd5.create_dataset(path, data=value, dtype=h5py.special_dtype(vlen=str))
        elif storage_type == StorageType.CONTINUOUS:
            if type(value) == float or valid_num(value):
                d = hd5.create_dataset(path, data=[float(value)])
        elif storage_type == StorageType.CATEGORICAL_INDEX:
            if type(value) == int or valid_num(value):
                d = hd5.create_dataset(path, data=[int(value)])
        elif storage_type == StorageType.CATEGORICAL_FROM_FLOAT:
            if (type(value) == float and not np.isnan(value)) or (type(value) == str and valid_num(value)):
                d = hd5.create_dataset(path, data=[int(float(value))])
    except (TypeError, OSError) as e:
        error_string = f'error creating dataset in {hd5.filename} in path {path} with data {value} of type {type(value)} ' \
                       f'that should have type {storage_type}'
        error_dict = {'id': id_, 'error': error_string}
        logger.error(json.dumps(error_dict))


def ingest_file(hd5_dir: str, dtypes: Mapping[str, StorageType], dfile_name: str, dfile_type: str,
                id_set: Optional[set] = None):
    group_path = f'/{dfile_type}/'
    ids = 0
    rows = 0
    logger.info(f'reading {dfile_name}')
    df = pd.read_csv(dfile_name, sep='|', quoting=csv.QUOTE_NONE, dtype=object)
    logger.info(f'finished reading {dfile_name}')
    to_ingest = set(df[ID].astype(int).unique())
    if id_set is not None:
        to_ingest = to_ingest.intersection(id_set)
        df = df[df[ID].astype(int).isin(to_ingest)]
    if len(to_ingest) < 1:
        logger.info(f'nothing to ingest from {dfile_name}')
        return
    logger.info(f'total ids: {len(to_ingest)} min id: {min(to_ingest)} max id: {max(to_ingest)} total rows: {df.shape[0]}')
    for id_ in to_ingest:
        tensor_path = os.path.join(hd5_dir, str(id_) + TENSOR_EXT)
        with h5py.File(tensor_path, 'a') as hd5:
            for _, row in df[df[ID] == str(id_)].iterrows():
                if group_path in hd5:
                    instance_path = f'{group_path}instance_{len(hd5[group_path])}/'
                else:
                    instance_path = f'{group_path}instance_0/'
                data_path = f'{instance_path}file_name'
                add_value_to_hd5(hd5, data_path, dfile_name, StorageType.STRING)
                for column, value in row.items():
                    if column == ID:
                        continue
                    data_path = f'{instance_path}{column}'
                    add_value_to_hd5(hd5, data_path, value, dtypes[column])
                rows += 1
        ids += 1
        if ids % LOG_COUNT == 0:
            logger.info(f'processed {rows} rows for {ids} ids')


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--file_types', nargs='*')
    parser.add_argument('--data_dir')
    parser.add_argument('--data_type_map')
    parser.add_argument('--hd5_dir')
    parser.add_argument('--id_file')
    parser.add_argument('--id_set_id', type=int)
    parser.add_argument('--total_id_sets', type=int)
    return parser.parse_args()


def main(**kwargs):
    data_dir = kwargs['data_dir']
    data_type_map_file = kwargs['data_type_map']
    file_types = kwargs['file_types']
    hd5_dir = kwargs['hd5_dir']
    id_file = kwargs.get('id_file')
    id_set_id = kwargs.get('id_set_id')
    total_id_sets = kwargs.get('total_id_sets')

    id_set = None
    if id_file is None and id_set_id is None and total_id_sets is None:
        logger.info(f'ingesting all ids in {data_dir} with types {" ".join(file_types)} into {hd5_dir}')
    elif id_file is not None and id_set_id is not None and total_id_sets is not None:
        id_set = subset_ids_from_file(id_file, id_set_id, total_id_sets, sep=SEP)
        logger.info(f'ingesting id set {id_set_id} out of {total_id_sets} in {data_dir} with types '
                     f'{" ".join(file_types)} into {hd5_dir}')
    else:
        logger.error('must specify either none or all of --id_file, --id_set_id, and --total_id_sets')
        sys.exit(1)

    data_dicts = get_data_file_names(data_dir, file_types)
    data_type_map = read_data_types_map(data_type_map_file)

    for d in data_dicts:
        ingest_file(hd5_dir=hd5_dir, dtypes=data_type_map[d['dfile_type']], id_set=id_set, **d)

    logger.info('complete')


if __name__ == '__main__':
    ARGS = parse_args()
    main(**vars(ARGS))
