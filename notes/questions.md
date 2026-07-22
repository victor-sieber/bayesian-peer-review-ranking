# Open questions

This file collects questions that arise during the project and may require
further investigation or discussion.

## Research Questions to be answered in the technical report:
# Primary:
- RQ1 (Bayesian ranking): What posterior ranking is obtained from a hierarchical Bayesian model of the overall grades that accounts for reviewer-specific scoring tendencies, and how uncertain are the resulting ranks?
- RQ2 (Model comparison): How does the Bayesian ranking compare with the original ranking and qualification procedure based on the two overall grades per proposal?
- RQ3 (Model stability): How stable is Bayesian ranking when each proposal receives only two reviews and reviewer workloads are unequal?
# Secondary:
- RQ4 (Supporting criteria): How closely do the four supporting criterion scores correspond to the holistic overall grade, and would their equal-weight average have produced a materially different proposal ordering?


## Evaluation procedure

- How exactly was the final ranking determined among proposals satisfying the
  evaluation rule? [x] --> Top 4 candidates
- How should the separate overall score be interpreted relative to the
  criterion scores?[x] --> Overall score is determined by the reviewer after having filled out the criterion scores,
  however it is subjectively given independently of the values of the criterion scores. The overall scores determine funding.
  
  
## Data preparation

- Which variables are required in the anonymized dataset?[x]
- Which anonymized outputs are intended for later publication?[x]
- Would the size of the budget for each proposal be another datapoint to analyze? [x] --> Yes but only in a second analyses

## How to make data FAIR (Findable, Accesible, Interoperable, Reusable)
- F: What should the final dataset title be []
who should be listed as creators and contributors, and which keywords and project description should we use? []
- A: After reviewing the residual risks, should the anonymized analytical dataset be openly available on Zenodo,
restricted, or only described through metadata? [x] --> Pseudononymized dataset should be available on Zenodo
