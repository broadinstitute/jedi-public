from collections import defaultdict

import pandas as pd

from jedi.long_file.config_processing.extraction_utils import \
    CODE_CONFIG, COV, COV_CODE, CODE_FIELD, \
    FIELD_CONFIG, FILE_TYPE, FIELD_NAME, FILTER_NAME, FILTER, \
    TIME_CONFIG, LINKER_ID, CODE_FILE_TYPES, parse_time_window
from jedi.long_file.config_processing.validate_configs import \
    validate_config_dfs, validate_field_config, validate_time_config


def parse_time_config(time_df: pd.DataFrame):
    """
    Takes time config as input, parses into a dictionary of relevant 
    encounters, with linker_ids as keys and lists of relevant time windows as 
    values.
    """
    encounters = defaultdict(list)
    for _, row in time_df.iterrows():
        encounters[row[LINKER_ID]].append(parse_time_window(row))
    return encounters


def parse_code_config(code_df: pd.DataFrame):
    """
    Converts code config into field config format
    """
    converted = []
    for cov, df in code_df.groupby(COV):
        if df[COV_CODE].isnull().all():
            continue
        codes = '|' + '|'.join(list(map(str, df[COV_CODE]))) + '|'
        for type in CODE_FILE_TYPES:
            row = {}
            row[FILE_TYPE] = type
            row[FIELD_NAME] = CODE_FIELD
            row[FILTER_NAME] = cov
            row[FILTER] = codes
            converted.append(row)
    return pd.DataFrame(converted)


validation_functions = {
    FIELD_CONFIG: validate_field_config,
    TIME_CONFIG: validate_time_config,
}
parse_functions = {
    CODE_CONFIG: parse_code_config,
    TIME_CONFIG: parse_time_config,
}


def parse_configs(code_config, field_config, time_config):
    """
    Wrapper to validate and parse each of the config files. Returns a 
    a dictionary with config types as keys and parsed config files as values.
    """
    # performing basic validation of config paths, obtaining dictionary of 
    # config types and correpsonding raw dataframes
    raw_dfs = validate_config_dfs(code_config, field_config, time_config)

    # performing additional config-specific validation and parsing 
    config_dict = {}
    for config_type, df in raw_dfs.items():
        if config_type in validation_functions:
            validation_functions[config_type](df)
        if config_type in parse_functions:
            config_dict[config_type] = parse_functions[config_type](df)
        else:
            config_dict[config_type] = df

    # concatenating code and field configs
    if CODE_CONFIG in config_dict:
        if FIELD_CONFIG in config_dict:
            config_dict[FIELD_CONFIG] = pd.concat([config_dict[CODE_CONFIG],
                                                   config_dict[FIELD_CONFIG]], sort=True)
        else:
            config_dict[FIELD_CONFIG] = config_dict[CODE_CONFIG]
        config_dict.pop(CODE_CONFIG)

    return config_dict
