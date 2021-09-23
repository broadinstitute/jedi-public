import os
import json
import argparse
import pandas as pd

LEVEL = 1
JSON  = 2
WARNING = "WARNING"
ERROR = "ERROR"


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--log_dir', required=True)
    parser.add_argument('--output_dir', required=True)
    return parser.parse_args()


def parse_log(path: str):
    """ Takes path to log file as input, returns dictionary with error levels
    as keys and lists of error dicts as values """
    with open(path, "r") as handle:
        lines = handle.readlines()
    warning_list = []
    error_list = []
    for line in lines:
        try:
            args = line.split(" - ")
            if args[LEVEL] == WARNING:
                warning_list.append(json.loads(args[JSON]))
            elif args[LEVEL] == ERROR:
                error_list.append(json.loads(args[JSON]))
        except:
            continue
    return warning_list, error_list


def parse_log_directory(log_dir: str):
    """ Takes a directory of log files as input, returns two dataframes of 
    warnings and errors respectively """
    out_list = [parse_log(f"{log_dir}/{f}") for f in os.listdir(log_dir)]
    warning_df = pd.DataFrame([item for out in out_list for item in out[0]])
    error_df = pd.DataFrame([item for out in out_list for item in out[1]])
    return warning_df, error_df


def main(**kwargs):
    log_dir = kwargs['log_dir']
    output_dir = kwargs['output_dir']
    
    if not os.path.exists(log_dir):
        raise FileNotFoundError("Invalid log directory")
    if not os.path.exists(output_dir):
        raise FileNotFoundError("Invalid output directory")

    warning_df, error_df = parse_log_directory(log_dir)
    if warning_df is not None:
        warning_df.to_csv(f"{output_dir}/warnings.tsv", sep="\t", index=False)
    if error_df is not None:
        error_df.to_csv(f"{output_dir}/errors.tsv", sep="\t", index=False)


if __name__ == '__main__':
    ARGS = parse_args()
    main(**vars(ARGS))

