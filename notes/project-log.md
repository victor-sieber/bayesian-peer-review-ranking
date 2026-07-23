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
- Discuss Pseudonymization with Supervisor and which files can be released on Zenodo (accessibility)
- Document the anonymization process

## 2026-07-17 — Documentation, Meeting & FAIR Data

### Completed
Documented the 1st weeks progress and infrastructure set up of the project

Improve FAIRness of Data, created in CRS_private under the release candidates:
- data_dictionary.csv which defines all variables, data types, score scales
- anonymization_log.csv gives an overview of which variables were retained, removed or replaced
- README.md which describes the dataset, evaluation process, file structure and validation
- re-identification-risk.md which discloses the already mentioned re-id risk.

Reviewed Peter Degen's Code for the evaluation of the Seed Grants 2026, additionally
tried to run the code but as of now missing the necessary packages.
Cloned his code from gitlab and downloaded the necessary packages -- ran the code.

Added some questions in question text file for meeting with rachel.

### Main findings
- What it means for data to be FAIR and which documentation steps are needed
- Raise question to Data release on gitlab from the originial Seed Grant 2026 evaluation.


### Next steps
- Make sure to computationally reproduce the CRS 2026 Grant Evaluation process.
- Begin with Literature review


## 2026-07-20 — Reproduced 2026 CRS Grant Evaluation Process Code & Discussion Budget as Datapoint

### Completed
Fully ran Peter Degen's Code pipeline for the CRS Evaluation process
Discussed release of Budget datapoint with Data Steward

### Main findings
The Budget data point is a secondary variable that can be intersting to analyze, however we want to focus on the overall-grade and bayesian ranking first.
I would say for now we focus more on scores based on proposal quality and reviewer variability than the financial allocation of the budget,
note:

For the primary analysis, I assume that funding outcomes were driven by proposal quality and reviewer-specific scoring variability,
while the requested budget served only as a separate justification check and did not directly determine the ranking,
since all four proposals meeting the original qualification rule could be funded within the call’s approximate capacity of six projects.

### Next steps
- Literature Review

## 2026-07-21 — Literature Review: Alvin's Msc Thesis (Introduction) & Rachel Heyard et al 2022 

### Completed
Went through Alvin's Msc Thesis Introduction which showes a mathematical abstraction of scores and rankings.
Read Rachel Heyard's et al 2022 paper and made notes.

### Main findings
Mathematical abstraction of scores and ranks is useful for the notation in the technical report
Rachel Heyard's Paper covers the bayesian ranking method that could be implemented in the small data set that I work with as an initial try.

### Next steps
- Structure of Technical Report and Research questions

## 2026-07-22 — Documentation, Concrete Structure Technical Report & Research Questions

### Completed
Documented the last three days work

Defined the research questions (3 primary, 1 secondary question)

Based on the research questions edited the structure of the technical report and wrote the to do's inside of the chapters.

### Main findings
The research questions helped to define the scope of the report as it's clear where to start (Base model) and where to invest most amount of time (Bayesian ranking)

### Next steps
- Start with chapters: 1.2 CRS Seed Grant evaluation 2026, 1.3 Original qualification procedure, 1.5 Research questions, 1.6 Scope and intended contribution


#Template
## YYYY-MM-DD — Short description

### Completed

### Main findings

### Next steps