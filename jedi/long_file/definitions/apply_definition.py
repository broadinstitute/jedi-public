import argparse
import datetime
import logging
import os
import json
from functools import partial

from jedi.long_file.config_processing.custom_long import extract_fields
from jedi.long_file.config_processing.parse_configs import parse_configs
from jedi.utils.ids import ids_from_file

CUSTOM = "custom"
LOG_COUNT = 20


DEF_MAP = {
    # NB: add hard coded definitions here
    # must be of the form:
    # (Iterable[h5py.File], int) -> Optional[pd.DataFrame]
}


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--def')
    parser.add_argument('--hd5_dir', nargs='*')
    parser.add_argument('--output_dir')
    parser.add_argument('--id_name')
    parser.add_argument('--min_id')
    parser.add_argument('--max_id')
    parser.add_argument('--hard_fail', action='store_true')
    parser.add_argument(
        '--code_config',
        help='Path to code config for custom long file definition. See jedi/long_file/config_processing/README.md',
    )
    parser.add_argument(
        '--field_config',
        help='Path to field config for custom long file definition. See jedi/long_file/config_processing/README.md',
    )
    parser.add_argument(
        '--time_config',
        help='Path to time_config for custom long file definition. See jedi/long_file/config_processing/README.md',
    )
    parser.add_argument(
        '--id_file', required=False,
        help='File with ids to restrict production to. File should have 1 column and no header.',
    )
    return parser.parse_args()


def main(**kwargs):
    def_name = kwargs['def']
    hd5_dirs = kwargs['hd5_dir']
    output_dir = kwargs['output_dir']
    id_name = kwargs['id_name']
    min_id = int(kwargs['min_id'])
    max_id = int(kwargs['max_id'])
    code_config = kwargs['code_config']
    field_config = kwargs['field_config']
    time_config = kwargs['time_config']
    hard_fail = kwargs.get('hard_fail')

    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    logger = logging.getLogger(__name__)

    id_file = kwargs.get('id_file', None)
    ids = list(range(min_id, max_id + 1))
    if id_file is not None:
        ids = list(ids_from_file(id_file, id_name).intersection(ids))

    logger.info(f'applying {def_name} to {min_id} through {max_id} in {hd5_dirs}')

    if def_name == CUSTOM:
        config_dict = parse_configs(code_config, field_config, time_config)
        DEF_MAP[CUSTOM] = partial(extract_fields, config_dict=config_dict)

    def_func = DEF_MAP[def_name]
    processed = 0
    for i in ids:
        try:
            f = f'{i}.hd5'
            paths = []
            for hd5_dir in hd5_dirs:
                full_path = os.path.join(hd5_dir, f)
                if os.path.isfile(full_path):
                    paths.append(full_path)
            if len(paths) > 0:
                df = def_func(hd5_paths=paths, id=i)
                if df is not None:
                    df.to_csv(os.path.join(output_dir, f'{i}.tsv'), sep='\t')
                processed += 1
                if processed % LOG_COUNT == 0:
                    logger.info(f'processed: {processed} most recent id: {i}')
        except Exception as err:
            error_dict = {"id": i, "error": str(err)}
            logger.error(json.dumps(error_dict))
            if hard_fail:
                raise err
    logger.info('finished')


if __name__ == '__main__':
    ARGS = parse_args()
    main(**vars(ARGS))
