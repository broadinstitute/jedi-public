import argparse
import json
import logging
import os
from typing import Iterable, Optional

import pandas as pd

from jedi.long_file.hdf5_functions import AGE

LOG_COUNT = 200


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--type')
    parser.add_argument('--def_col', nargs='*', required=False)
    parser.add_argument('--long_dir')
    parser.add_argument('--output_file')
    parser.add_argument('--min_id', type=int)
    parser.add_argument('--max_id', type=int)
    parser.add_argument('--log_file', action='store_true')
    parser.add_argument('--hard_fail', action='store_true')
    return parser.parse_args()


def wide_df_from_dir(df_dir: str, wide_fn, def_cols: Optional[Iterable[str]] = None, min_id: Optional[int] = None,
                     max_id: Optional[int] = None, hard_fail: bool = False) -> pd.DataFrame:
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    logger = logging.getLogger(__name__)
    def_cols_log = f' with definition columns {def_cols}' if def_cols is not None else ''
    logger.info(
        f'producing {wide_fn.__name__} wide files{def_cols_log} from {df_dir} for ids {min_id} through {max_id}')
    rows = []
    processed = 0
    for i, path in enumerate(os.listdir(df_dir)):
        int_id = int(os.path.splitext(path)[0])
        if (min_id is not None and int_id < min_id) or (max_id is not None and int_id > max_id):
            continue
        df_path = os.path.join(df_dir, path)
        try:
            df = pd.read_csv(df_path, sep='\t')
            df.index = pd.to_timedelta(df[AGE])
            del df[AGE]
            data = wide_fn(df, def_cols=def_cols)
            if data is None:
                continue
            rows.append(data)
            processed += 1
        except Exception as err:
            error_dict = {"id": i, "error": str(err)}
            logger.error(json.dumps(error_dict))
            if hard_fail:
                raise err
        if processed % LOG_COUNT == 0:
            logger.info(f'processed {processed} files')
    return pd.DataFrame(rows)


WIDE_MAP = {
    # NB: add references to hard-coded wide file definitions here
    # must be of the form:
    # (pd.DataFrame, Optional[Iterable[str]] = None) -> Dict[str, Any]:
}


def main(**kwargs):
    wide_type = kwargs['type']
    def_cols = kwargs.get('def_col')
    df_dir = kwargs['long_dir']
    output_file = kwargs['output_file']
    min_id = kwargs.get('min_id')
    max_id = kwargs.get('max_id')
    hard_fail = kwargs.get('hard_fail')

    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    logger = logging.getLogger(__name__)
    df = wide_df_from_dir(df_dir, WIDE_MAP[wide_type], def_cols, min_id, max_id, hard_fail)
    logger.info(f'writing wide file to {output_file}')
    df.to_csv(output_file, sep='\t', index=False)
    logger.info('complete')


if __name__ == '__main__':
    ARGS = parse_args()
    main(**vars(ARGS))
