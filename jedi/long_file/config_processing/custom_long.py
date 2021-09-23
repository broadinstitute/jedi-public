import h5py
from typing import List, Dict

import h5py
import pandas as pd
from jedi.long_file.hdf5_functions import FILE_TYPE_TO_AGE_FIELD

from jedi.long_file.config_processing.extraction_utils import AGE, \
    FIELD_CONFIG, FILE_TYPE, FIELD_NAME, TIME_CONFIG, LINKER_ID, meets_time_criteria, \
    extract_column, id_from_path


def extract_fields(hd5_paths: List[str], config_dict: Dict[str, pd.DataFrame]) \
        -> pd.DataFrame:
    """
    Extracts fields for a custom long file as specifed in config_dict, from
    hd5_paths. Refer to rpdr/long_file/config_processing/README.md for config
    specifications.
    """
    long_df = []
    id = id_from_path(hd5_paths)
    extraction_df = config_dict[FIELD_CONFIG]
    encounters = None
    if TIME_CONFIG in config_dict:
        encounters = config_dict[TIME_CONFIG]
    # for each hd5 containing patient information
    for hd5_path in hd5_paths:
        hd5 = h5py.File(hd5_path, "r")
        hd5_types = hd5.keys()
        # for each relevant file type
        for type, sub_df in extraction_df.groupby(FILE_TYPE):
            if type not in hd5_types:
                continue
            AGE_FIELD = FILE_TYPE_TO_AGE_FIELD[type].name
            # for each instance of the file
            for instance in hd5[type].keys():
                age_text = hd5[type][instance][AGE_FIELD][()]
                instance_age = pd.to_timedelta(age_text)
                # checking if instance meets time criteria (if any)
                if not meets_time_criteria(id, instance_age, encounters):
                    continue
                # for each field to extract
                for _, row in sub_df.iterrows():
                    # extracting field from hd5
                    if row[FIELD_NAME] not in hd5[type][instance].keys():
                        continue
                    extracted = hd5[type][instance][row[FIELD_NAME]][()]
                    # applying any applicable filters, returning dict of 
                    # output column
                    extracted_dict = extract_column(row, extracted)
                    if not extracted_dict:
                        continue
                    # creating and populating information in long row dict
                    long_row = {}
                    long_row[AGE] = age_text
                    long_row[LINKER_ID] = id
                    long_row.update(extracted_dict)
                    long_df.append(long_row)
    # converting to dataframe
    custom_df = pd.DataFrame(long_df)
    if custom_df.empty:
        raise ValueError("No covariates found; skipping.")
    # changing index to age, sorting rows
    custom_df.sort_values(by=AGE, inplace=True)
    custom_df.index = custom_df[AGE]
    del custom_df[AGE]
    return custom_df
