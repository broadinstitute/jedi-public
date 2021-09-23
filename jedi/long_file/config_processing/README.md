# Config-based Long File Generation

This section of the pipeline defines functionality to generate custom long files from delimited text configuration files. This README provides an overview of current functionality, specifies the format of the config files and details corresponding functionality, and provides usage instructions. 

Contents:
- [Current Functionality](#current-functionality)
- [Overview of Config Files and Functionality](#overview-of-config-files-and-functionality)
  - [Code Config](#code-config)
  - [Field Config](#field-config)
  - [Time Config](#time-config)
- [Usage Instructions](#usage-instructions)
  - [General Usage](#general-usage)
- [Merging Long Files](#merging-long-files)



## Current Functionality
The current system allows generation of long file columns that:
- Filter for any diagnostic code
- Extract any arbitrary column from the structured flat files, and select for desired values/ apply regex to extracted values
- Combine multiple flat file columns into a single long file one
- Extract report text from any notes file and apply regexes
- Restrict for time periods for all of the above


## Overview of Config Files and Functionality
The aforementioned functionality is implemented using the following three config types:
- Code Config: provides a mapping between covariates and diagnostic codes
- Field Config: specifies mapping between file types, the field names in those files (eg: Age), and filters/regexes to apply to extracted values
- Time Config (optional): provides mapping between ids and time periods of interest

At least a code config or a field config needs to be provided to specify a valid set of long files. Any combination meeting that minimum criteria is valid. The following subsections detail the columns and corresponding extraction behavior of each of these config files. 

Sample config files can be found at [test_data/configs](https://github.com/broadinstitute/ml_partners_rpdr/tree/ps_custom_long_consolidate/test_data/configs). It should be noted that these files can be in either `.csv` or `.tsv` format. 

### Code Config
The code config maps covariate names to diagnostic codes. A sample code config could look like:

|cov|cov.code|cov.code.type|
|-|:--:|:--:|
heartFailure|428.0|ICD9|
heartFailure|428.1|ICD9|
heartFailure|I11.0|ICD10|
heartFailure|I97.130|ICD10|	
|cardSurg|33405|CPT|
|cardSurg|33406|CPT|
|cardSurg|I97.190|ICD10|
|cardSurg|35.42|ICD9|

This config would produce long file rows with the specified diagnostic codes. The required columns for this file are `cov` and `cov.code`.

### Field Config
The field config has the following columns:

- `file_type`: name of the file type
- `field_name`: name of the field in that file
- `long_name`: name of the extracted column in the generated long file
- `filter_name` (optional): name of the filter to apply to the extracted value
- `filter` (optional): filter to apply:
  - For exact matches, this takes the form: |option_1|option_2|...|option_n|
  - For regexes to apply, this is just the regex text

The required columns in this case are `file_type` and `field_name`. However, if a filter is being used, then the `filter_name` and `filter` columns of a row must both be populated to define a valid application. 


### Time Config

The time config maps `linker_ids` to time periods of interest. Each row of the file is parsed into a window corresponding to (`start_age - before_days`, `end_age + after_days`). A sample time config could look like:

|linker_id|start_age  |end_age|before_days|after_days|
|---------|-----------|-------|-----------|----------|
|1        |26000 days |26010 days|3 days     |          |
|1        |26015 days |26025 days|           |2 days    |
|2        |20010 days |20012 days|    3 days       |       7 days   |
|2        |20000 days |20025 days|           |          |

This config is optional, but if specified, it must contain the `linker_id` column and sufficiently populated age columns that define a time window instead of just a point in time. If a `linker_id` is not specified in this config, but is contained in the ids to generate long files from, it is assumed that no time constraints need to be enforced for that id. 

## Usage Instructions

The following commands can be used to generate a set of long files:

### General Usage
```
conda activate jedi
cd repos/jedi
python jedi/long_file/definitions/apply_definition.py
    --def custom
    --hd5_dir [PATH TO HD5 DIRECTORY]
    --output_dir [PATH TO OUTPUT DIRECTORY]
    --code_config [PATH TO CODE CONFIG]
    --field_config [PATH TO FIELD CONFIG]
    --time_config [PATH TO TIME CONFIG]
    --min_id [START OF ID RANGE TO USE]
    --max_id [END OF ID RANGE TO USE]
```


