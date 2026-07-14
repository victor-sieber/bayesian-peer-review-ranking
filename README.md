# Bayesian Modelling of Grant Evaluation and Ranking

This repository contains the code, documentation, literature notes, and
technical report for a statistical re-analysis of the CRS Seed Grant 2026
evaluation.

The project first aims to understand and reproduce the original evaluation
procedure and existing analysis. It then explores statistical and Bayesian
approaches to reviewer scores, variation in scoring behaviour, proposal
rankings, ranking uncertainty, and funding decisions.

The CRS Seed Grant 2026 evaluation serves as the empirical application. More
generally, the project considers how rankings can be inferred from scores
provided by a relatively small panel of reviewers.

## Project objectives

The main objectives are to:

1. understand and document the CRS Seed Grant 2026 evaluation procedure;
2. reproduce the existing analysis;
3. investigate the relationship between criterion scores and overall scores;
4. examine variation in scoring behaviour between reviewers;
5. explore statistical and Bayesian approaches to proposal ranking;
6. quantify uncertainty in rankings and funding decisions;
7. compare alternative evaluation approaches and derive practical
   recommendations.

The precise research questions and modelling choices may evolve as the
existing analysis, relevant literature, and data structure are examined.

## Repository structure

- `analysis/`: R scripts for reproduction, data preparation, and modelling
- `data/`: documentation on obtaining and using the data; no data files are
  stored in GitLab
- `notes/`: project log, open questions, and literature notes
- `report/`: evolving technical report

## Data availability and protection

No evaluation data are stored or version-controlled in this GitLab repository.

During the project, restricted source data and intermediate files are stored
locally or in approved UZH storage and are excluded from Git. The finalized
anonymized dataset is intended to be archived separately on Zenodo.

Once the dataset is available, its citation, persistent identifier, and
instructions for obtaining it will be added to this repository. The analysis
scripts will then be linked to the archived dataset so that approved results
can be reproduced without storing data in GitLab.

Qualitative free-text responses, direct identifiers, and re-identification keys
are not intended for publication.

## Reproducibility and FAIR principles

The project aims to document:

- the evaluation procedure and data structure;
- data-processing and anonymization steps;
- software and package dependencies;
- statistical assumptions and modelling decisions;
- scripts used to generate reported tables and figures;
- limitations affecting interpretation and reuse.

The separation of code and data allows the analysis repository to remain
version-controlled while the approved anonymized dataset is archived and
cited independently.

## Current project stage

The initial phase consists of:

1. understanding the evaluation rules and data structure;
2. running and documenting the existing analysis;
3. reviewing literature on reviewer variability and Bayesian ranking;
4. developing a baseline representation of the 2026 evaluation process.

## Project context

This work is conducted as a summer research project at the Center for
Reproducible Science, University of Zurich. The project started on
13 July 2026.

**Student:** Victor Sieber  
**Supervision:** Dr. Rachel Heyard