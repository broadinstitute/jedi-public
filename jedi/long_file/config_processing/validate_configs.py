import os
import re

import pandas as pd

from jedi.long_file.config_processing.extraction_utils import \
    CODE_CONFIG, COV, COV_CODE, COV_CODE_TYPE, \
    FIELD_CONFIG, FILE_TYPE, FIELD_NAME, FILTER_NAME, FILTER, \
    TIME_CONFIG, LINKER_ID, START_AGE, END_AGE, BEFORE_DAYS, AFTER_DAYS, \
    CONFIG_LIST

required_cols = {
    CODE_CONFIG: [COV, COV_CODE, COV_CODE_TYPE],
    FIELD_CONFIG: [FILE_TYPE, FIELD_NAME],
    TIME_CONFIG: [LINKER_ID],
}

config_sep = {
    "tsv": "\t",
    "csv": ",",
}


def validate_config_dfs(code_config, field_config, time_config):
    """
    Takes paths to config files as input, ensures required config files are 
    specified, file paths are valid, delimited files can be converted to 
    dataframes, and config dataframes contain minimal set of required columns.
    """
    # ensuring sufficient configuration of files
    if code_config is None and field_config is None:
        raise FileNotFoundError("Missing code and field configs, provide at \
            least one.")
    df_dict = {}
    path_list = [code_config, field_config, time_config]
    # for each config file
    for config_type, path in zip(CONFIG_LIST, path_list):
        # validating file path
        if path:
            if not os.path.exists(path):
                raise FileNotFoundError(f'Missing {config_type}: {path} is \
                    not a file.')
            # ensuring valid dataframe format, required columns
            try:
                sep = config_sep[path.split('.')[-1]]
                df = pd.read_csv(path, sep=sep, encoding='ISO-8859-1')
            except Exception as e:
                raise ValueError(f'Invalid {config_type} format: {e}')
            for col in required_cols[config_type]:
                if col not in df.columns:
                    raise ValueError(f'Missing {col} column in {config_type}')
            df_dict[config_type] = df
    return df_dict


def validate_field_config(field_df: pd.DataFrame):
    """
    Helper function that validates (optional) regex column in the fields config
    """
    cols = field_df.columns
    # if no regex columns, then no further validation necessary
    no_regex_cols = FILTER_NAME not in cols and FILTER not in cols
    if no_regex_cols:
        return
    # ensuring both necessary filter columns are present
    only_regex_name = FILTER_NAME in cols and FILTER not in cols
    only_regex = FILTER_NAME not in cols and FILTER in cols
    if only_regex_name or only_regex:
        raise ValueError(f"Missing one of {FILTER_NAME} or {FILTER} in \
            {FIELD_CONFIG}")
    # ensuring there are no duplicate filter names -- which would lead to 
    # identically named columns in the long files
    for type, sub_df in field_df.groupby([FILE_TYPE, FIELD_NAME]):
        if not sub_df[FILTER_NAME].is_unique:
            raise ValueError(f"Duplicate values in {FILTER_NAME} column. \
                Please rename for distinct column names in output file.")
    # ensuring that all filters are valid 
    for regex_string in field_df[FILTER]:
        if not pd.isnull(regex_string):
            if not regex_string.startswith("|"):
                try:
                    re.compile(regex_string)
                except re.error:
                    raise ValueError(f"Invalid regex found in file:{regex_string}")


def validate_time_config(time_df: pd.DataFrame):
    """
    Helper function to ensure there exists a minimum valid configuration of age
    columns in the time config; in order to define time windows of interest.
    """
    cols = time_df.columns
    # ensuring there is a minimum valid configuration of age columns in the
    # spec file to specify a time window of interest
    only_start_age = START_AGE in cols and END_AGE not in cols and \
                     BEFORE_DAYS not in cols and AFTER_DAYS not in cols
    only_end_age = END_AGE in cols and START_AGE not in cols and BEFORE_DAYS \
                   not in cols and AFTER_DAYS not in cols
    no_start_or_end_age = START_AGE not in cols and END_AGE not in cols
    if only_start_age or only_end_age or no_start_or_end_age:
        raise ValueError("Insufficient information to specify time period of \
            interest in spec file. Refer to documentation for help.")
    # ensuring all age columns are in a format compatible with timedeltas
    for age_column in [START_AGE, END_AGE, BEFORE_DAYS, AFTER_DAYS]:
        if age_column in cols:
            try:
                time_df[age_column] = pd.to_timedelta(time_df[age_column])
            except:
                raise ValueError(f"{age_column} in spec file contains invalid \
                    values")
