# Project log

This file documents the progress of the project, including completed work,
important observations, encountered problems, and planned next steps.

## 2026-07-14 — Gitlab repo

### Completed
Created the initial Gitlab repo:
- Folderstructure
- Added the readme.md files
- Set up .gitignore
- Added an analysis R script in folder scripts as a placeholder
- Added a template for a future dynamic reporting for the techincal report

Read the Primer (Find on CRS webpage) on Dynamical Reporting

### Main findings
What is Dynamical Reporting and how to set it up in R
Find a good initial repo structure

### Next steps
Add a section to re-identification risk & get familiar with the data structure


## 2026-07-15 — Familiarity with Data & Re-identification risk

### Completed
- Analysed the raw data of the CRS Seed Grant 2026
- Created a new text file with a paragraph to re-identification risk
- excluded the re-identification risk file from .gitignore

### Main findings
- Differentiated between analytical vs qualitative data
- Familiarity with how the raw evaluation data was summarized/analysed by CRS
- Risk awareness to re-identification

### Next steps
- Anonymize the raw data

## 2026-07-16 — Pseudonymisation of raw data

### Completed
Created a new folder CRS_private locally with:
- Anonymization R script that take as input
the summary table of the raw data including every review done (2 for each proposal)
and as output it produces a anonoymized csv file of the raw data & a key (Pseudonymization)
- Validation R script that independently of the anonymization R script compares
the raw data to the produced anonymized csv file.
- Record session info R script that documents the versions of packages dplyr & readr
and produces a text file with all of the info for documentation sake.

### Main findings
- Pseudonymization != Anonymization
- To Pseudonymize the raw data should suffice but is a matter of discussion on Friday's meeting
- Main Learnings: Use of readr and dplyr to go from raw data to pseudonymized data & R coding workflow

### Next steps
- Discuss Pseudonymization with Supervisor and which files can be released on Zenodo
- Document the anonymization process

## 2026-07-17 — Documentation, Meeting & start with literature research

### Completed

### Main findings

### Next steps


#Template
## YYYY-MM-DD — Short description

### Completed

### Main findings

### Next steps