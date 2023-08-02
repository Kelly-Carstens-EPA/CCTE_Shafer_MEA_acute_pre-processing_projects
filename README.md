# Repo for CCTE_Shafer_MEA_acute pre-processing projects

Refer to `MEA Acute pre-processing Notes.docx` for a brief guide of how to use the scripts in `mea-acute-neural-stats-to-mc0-scripts/` to pre-process data from MEA Acute experiments.

## Current repo organization

* `mea-acute-neural-stats-to-mc0-scripts/`
    * generic scripts for pre-processing the data. These functions are called by the `run_me_[project_name].R` files
    * `run_me/` - the final version of the `run_me_[project_name].R[md]` scripts used to process the data for each project. 
* Project-specific folders (e.g., `APCRA2019`) contain all files specific to each project EXCEPT the final `run_me_[project_name].R` script
    * `output` subfolders - contain the intermediate and final output from the `run_me_[project_name].R`
    

Note that the generic scripts under `mea-acute-neural-stats-to-mc0-scripts/` may be updated over time. In order to obtain versions of the used to pre-process the data for a particular project, you can checkout how the repository appeared under a past commit with git.

For example, 

* Determine the date range in which the project was pre-processed (for older data, see the date associated with the most recent "dat4_" object in the output; for newer data, see the date in the 'acn.dat.description' saved under the `[project_name]_MEA_Acute_for_tcpl_lvl0.RData')
* Find the commit id SHA associated with a commit from the desired date range, e.g., `git log --since='July 1 2023' --until 'Aug 1 2023'
* Create a new branch that checks out the repository as it appeared with that commit, e.g., `git checkout -b test-branch first-6-digits-of-commit-SHA`

** Note that before the commit on Jan 10, 2023, this repo only contained the scripts under `mea-acute-neural-stats-to-mc0-scripts`. This repo was expanded to include the project folders on Jan 10, 2023.
