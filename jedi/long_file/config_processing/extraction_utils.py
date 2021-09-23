import re
from typing import List, Optional, Dict

import pandas as pd

# config file names
CODE_CONFIG = "code_config"
FIELD_CONFIG = "field_config"
TIME_CONFIG = "time_config"
CONFIG_LIST = [CODE_CONFIG, FIELD_CONFIG, TIME_CONFIG]

# column names for the code config file
COV = 'cov'
COV_CODE = 'cov.code'
COV_CODE_TYPE = 'cov.code.type'

# column names for the field config file
FILE_TYPE = "file_type"
FIELD_NAME = "field_name"
LONG_NAME = "long_name"
FILTER_NAME = "filter_name"
FILTER = "filter"

# column names for the time config file
LINKER_ID = "linker_id"
START_AGE = "start_age"
END_AGE = "end_age"
BEFORE_DAYS = "before_days"
AFTER_DAYS = "after_days"

LOG_COUNT = 20
ZERO_DAYS = pd.to_timedelta('0d')
CODE_FILE_TYPES = []  # NB: add list of file types containing codes (e.g. diagnostic, procedure, medicine, ...)
CODE_FIELD = ''  # NB: replace with name of the field containing codes
AGE = "age"


def id_from_path(hd5_paths: List[str]):
    return int(hd5_paths[0].split("/")[-1].split(".")[0])


def parse_time_window(time_row: pd.Series):
    """ Parses time window of interest from various age columns """
    # filling in zero days for ages that are not present
    for age_column in [START_AGE, END_AGE, BEFORE_DAYS, AFTER_DAYS]:
        if age_column not in time_row or pd.isnull(time_row[age_column]):
            time_row[age_column] = ZERO_DAYS
        else:
            time_row[age_column] = pd.to_timedelta(time_row[age_column])
    start_time = time_row[START_AGE] - time_row[BEFORE_DAYS]
    end_time = time_row[END_AGE] + time_row[AFTER_DAYS]
    return start_time, end_time


def meets_time_criteria(id_, instance_age, encounters):
    """ Checks whether instance age falls within any specified time window """
    # no time config or no specified time period for this id
    if encounters is None or id_ not in encounters:
        return True
    # checking instance against all time windows
    matches = []
    for start_time, end_time in encounters[id_]:
        no_window = (start_time == ZERO_DAYS and end_time == ZERO_DAYS)
        in_window = (start_time <= instance_age <= end_time)
        matches.append((no_window or in_window))
    return any(matches)


def apply_filter(filter_, extracted):
    """ Helper function to look for exact matches or apply regexes """
    if str(filter_).startswith('|'):
        matches = filter_.split('|')
        if extracted in matches:
            return extracted
    else:
        return ','.join(re.findall(filter_, extracted))


def extract_column(row, extracted) -> Optional[Dict[str, str]]:
    """ 
    Takes config row and extracted value as input. Returns None if filter
    fails on extracted value, or a dictionary with appropriate column if filter
    matches.
    """
    extracted_dict = {}
    if LONG_NAME in row and not pd.isnull(row[LONG_NAME]):
        out_name = row[LONG_NAME]
    else:
        out_name = row[FIELD_NAME]
    if FILTER not in row or pd.isna(row[FILTER]):
        col = f"{row[FILE_TYPE]}.{out_name}"
        extracted_dict[col] = extracted
    else:
        col = f"{row[FILE_TYPE]}.{out_name}.{row[FILTER_NAME]}"
        matches = apply_filter(row[FILTER], extracted)
        if not matches:
            return
        extracted_dict[col] = matches
    return extracted_dict
