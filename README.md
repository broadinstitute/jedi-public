
# JEDI
The JEDI Extractive Data Infrastructure (JEDI) is a Python-based library for ingestion and harmonization of clinical 
data files suitable for research, while 
remaining agnostic to the specific clinical domain. JEDI was originally developed to ingest raw text files 
("flat files") from the Research Patient Data Registry (RPDR) of the Mass General Brigham Health System (MGB). This 
public version of JEDI is a generalization of this functionality that is applicable to a wide range of EHR-like
datasets. The 
areas of applicability include clinical research with data from electronic health records, clinical machine learning, 
and clinical trial design.

JEDI transforms data between four file types, each suitable for different stages of analyzing and modeling clinical data.

* **Flat Files**: raw data extracted from a healthcare system's EHR system. These files are assumed to belong to 
 some number of file types (e.g. demographics, medications, vital signs, etc.) each with a distinct schema. Each patient
must be associated with a unique identifier. The flat file schema must include an age field corresponding to the patient's
age in days (can be 0 days for data such as demographics).
* **HDF5 Files**: produced by ingestion to represent all data for a given patient. The datasets in these files each 
correspond to a value in the flat files. Rows of data from the flat files are collected into instances. These files are 
organized hierarchically:
    * file type
    * instance
    * fields from file type
* **Long Files**: flat files containing data aggregated for a given patient. The fields included in a given long file
are specified by a config file. Values in long files can be transformed by a regular expression and filtered by a time
window on a patient-specific basis.
* **Wide Files**: flat files containing data for a cohort of patients relative to one or more temporal anchors. 
Currently, wide files must be specified by a function that takes a long files as input (as a pandas dataframe) and
produces a python dict that maps field names to values. The intention is for there to be one or more sets of mappings
from field names to values. Each set of mappings corresponds to a temporal anchor for each patient (e.g. start of 
follow up or an outcome such as the initial diagnosis of heart failure.) The resulting wide file contains one row per
patient with many columns corresponding to values relative to one or more temporal anchors.

## Setup

### Clone JEDI
```
git clone git@github.com:broadinstitute/jedi-public.git
```

### Setup conda environment
```bash
cd jedi
conda env create -f envs/conda_env.yml
pip install .
```

# Usage
<img width="785" alt="Screen Shot 2021-04-27 at 9 50 41 AM" src="https://user-images.githubusercontent.com/1573896/116255352-28409a00-a740-11eb-894c-2b7d82461fbf.png">



# JEDI and C3PO
JEDI is the infrastructure used to construct C3PO, the Community Care Cohort Project, a 520k patient cohort from the Mass General Brigham health system described in this paper:

Shaan Khurshid, Christopher Reeder, Lia X. Harrington, Pulkit Singh, Gopal Sarma, Samuel F. Friedman, Paolo Di Achille, Nathaniel Diamant, Jonathan W. Cunningham, Ashby C. Turner, Emily S. Lau, Julian S. Haimovich, Mostafa A. Al-Alusi, Xin Wang, Marcus D.R. Klarqvist, Jeffrey M. Ashburner, Christian Diedrich, Mercedeh Ghadessi, Johanna Mielke, Hanna M. Eilken, Alice McElhinney, Andrea Derix, Steven J. Atlas, Patrick T. Ellinor, Anthony A. Philippakis, Christopher D. Anderson, Jennifer E. Ho, Puneet Batra, Steven A. Lubitz. "Cohort Design and Natural Language Processing to Reduce Bias in Electronic Health Records Research: The Community Care Cohort Project." _medRxiv_ 2021.05.26.21257872; doi: https://doi.org/10.1101/2021.05.26.21257872 
