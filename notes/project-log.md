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


## 2026-07-24 - Weekly Meeting, Kaplan et al. & filled out notes for literature read

### Completed
- Weekly meeting and discussed technical report structure
- Read Kaplan et al.
- Made notes for Kaplan -- keep in mind for doing motivation in introduction!

### Main findings
- Use Kaplan to show the problem of a small pool of reviewers for accuracy

### Next steps
- Add notes to github repo


## 2026-07-25 - Writing 1.2, 1.3 & research on bayesian modelling

### Completed
- Wrote part of the introduction, 1.2, 1.3
- Studied the theoretical foundation of Rachel Heyard's continuous bayesian model

### Next steps
- Adapt Rachel's method with the R packages used in her paper (2022)


## 2026-07-27 - Prepared the Bayesian modelling setup

### Completed
- Reviewed the continuous hierarchical model from Heyard et al. and identified the required inputs: proposal_id, reviewer_id, and overall_grade.
- Installed the required R packages and began configuring JAGS on macOS.
- Defined the first model as a baseline using only the holistic overall grades.

### Main findings
- The model separates proposal effects, reviewer scoring tendencies, and residual variation.
- The four supporting criteria are not part of this first model and will be analysed separately.

### Next steps
- Complete the JAGS configuration.
- Adapt the ERforResearch workflow to the anonymized CRS dataset.


## 2026-07-28 - Configured JAGS and resolved package issues

### Completed
- Installed JAGS through Homebrew and connected it to R through runjags.
- Investigated problems with rjags and determined that the model could be run through the external JAGS executable instead.
- Confirmed that JAGS 4.3.2 was detected successfully.

### Main findings
- rjags was not required for the selected runjags_method = "parallel" workflow.
- The repeated macOS lipo warning did not prevent JAGS from running.

### Next steps
- Create the complete model-fitting script.
- Validate the structure and contents of the anonymized dataset.


## 2026-07-29 - Implemented the initial continuous Bayesian model

### Completed
- Created analysis/01_fit_heyard_model.R.
- Added checks for 84 reviews, 42 proposals, 9 reviewers, valid 1–5 grades, and exactly two reviews per proposal.
- Reconstructed the original CRS qualification rule and confirmed four qualifying proposals.
- Started fitting the continuous Bayesian hierarchical model with four MCMC chains.

### Main findings
- The package’s strict convergence threshold caused repeated automatic extensions of the MCMC sampling.
- The original qualification benchmark could be reproduced correctly before fitting the Bayesian model.

### Next steps
- Adjust the MCMC settings to prevent excessively long automatic extensions.
- Complete the fit and inspect convergence diagnostics.


## 2026-07-30 - Completed and validated the baseline model

### Completed
- Successfully fitted the initial continuous Bayesian model using overall_grade.
- Generated the raw-average ranking, posterior-mean ranking, and expected ranking.
- Saved the model summaries, ranking tables, qualification benchmark, session information, and comparison figure.
- Checked MCMC convergence and obtained a maximum PSRF of 1.007729.
- Committed and pushed the scripts on gitlab

### Main findings
- The four originally qualified proposals also occupied the first four posterior-mean ranking positions.
- Proposals tied under the raw average were separated after accounting for reviewer scoring tendencies.
- Exact proposal ranks remained uncertain despite good computational convergence.
- Reviewer effects should be interpreted as scoring tendencies rather than proven bias.

### Next steps
Discuss the baseline model, priors, reviewer structure, and continuous-versus-ordinal specification with Rachel.
Produce posterior rank uncertainty summaries before extending the analysis.


## 2026-08-03 — Revisited Bayesian model structure

### Completed:
- clarified the Heyard reference model and interpretation;
- distinguished proposal effects, reviewer effects and proposal-reviewer effects;
- considered consequences of only two reviews per proposal.

### Main finding:
- original Heyard structure is relatively complex for the sparse CRS setting.


## 2026-08-04 — Defined V0 and V1

### Completed:
- retained original Heyard model as V0;
- implemented Rachel’s cross-classified simplification as V1;
- reorganized model files/scripts.

### Main finding:
- V1 uses one shared proposal effect and one shared reviewer-associated scoring effect.


## 2026-08-05 — Fresh V0 fit

### Completed:
- reran V0 from clean setup;
- checked convergence and qualification reconstruction.

### Main finding:
- V0 converged well, but rank uncertainty remained large.


## 2026-08-06 — Fresh V1 fit

### Completed:
- fitted V1;
- checked the expected 9 reviewer effects;
- verified MCMC convergence.

### Main finding:
- V1 was computationally stable and suitable for direct comparison with V0.


## 2026-08-07 — Model comparison and meeting with Rachel

### Completed:
- created 03_compare_v0_v1.R;
- compared posterior ranks, expected ranks and rank uncertainty;
- investigated P023/P032 pairwise probability;
- meeting with Rachel.

### Main findings:
- V0/V1 rankings are very similar;
- P023/P032 apparent reversal is essentially a posterior tie;
- broad ranking uncertainty remains;
- Rachel supported pursuing the graph/data-design direction after V1 validation and criterion analysis.

### Next steps:
- validate/stress-test V1;
- descriptive criterion analysis;
- revise technical-report structure;
- begin graph-design literature/theory.


## 2026-08-09 to 2026-08-11 — Technical report writing

### Completed
- Substantially expanded and revised the technical report.
- Wrote up V0/V1 methodology, expected-rank theory, ranking results and model comparison.
- Reduced repetition between methodology, results and validation sections.
- Added the main V1 ranking and V0/V1 comparison figures.
- Planned a focused V1 validation chapter.

### Main findings
- V1 gives almost the same substantive ranking as V0 despite being more parsimonious.
- Exact rank changes can be misleading when posterior pairwise probabilities are close to 0.5.
- The report now separates methodology, empirical results and model validation more clearly.

### Next steps
- Validate V1 before treating it as the main model.
- Afterwards perform the criterion analysis and begin the graph-based review-design extension.


## 2026-08-11 to 2026-08-12 — V1 model validation

### Completed
- Implemented convergence diagnostics, trace plots and posterior variance decomposition.
- Implemented posterior predictive checks for mean, dispersion, within-proposal disagreement and the bounded 1–5 score range.
- Re-fitted V1 with Half-Normal(0,1) scale priors as a prior-sensitivity analysis.
- Compared rankings, variance components and rank uncertainty between the baseline and sensitivity models.

### Main findings
- V1 showed good computational convergence.
- Most posterior variance is attributed to residual review-level variation; proposal and reviewer variance shares are smaller and uncertain.
- Posterior predictive checks reproduce the observed mean, spread and reviewer disagreement well.
- The Gaussian model predicts around 6% of replicated grades outside the possible 1–5 range, which is the main model limitation identified.
- Prior sensitivity is extremely small: expected ranks and variance conclusions are essentially unchanged under Half-Normal priors.
- The large rank uncertainty is therefore not driven by the original Uniform(0,2) prior choice.

### Next steps
- Write the validation results into the technical report and connect them to the relevant literature.
- Discuss whether the bounded-score limitation requires an ordinal sensitivity check.
- Perform the criterion-versus-overall-grade analysis.
- Move to the graph-based review-design analysis.

## 2026-08-16 to 2026-08-17 — Validation write-up and report refit

### Completed
- Wrote the V1 validation results into the technical report.
- Added computational diagnostics, posterior variance shares, posterior predictive checks and prior-sensitivity results.
- Reorganized the validation chapter to separate convergence, model adequacy and prior robustness.
- Added interpretation of the proposal variance share as the model-implied intraproposal correlation.
- Revised wording around the Gaussian 1--5 score limitation.
- Moved technical Bayesian updating and partial-pooling derivations into the appendix.

### Main findings
- V1 is computationally stable and robust to the tested prior specification.
- The proposal variance share is small and can be interpreted as weak model-implied agreement between two ratings of the same proposal.
- Most fitted variation is residual review-level variation.
- The main remaining model-form limitation is the unbounded Gaussian score model.

### Next steps
- Complete the detailed reviewer-disagreement diagnostic.
- Perform and write up the criterion analysis.
- Continue restructuring the report before the graph-based extension.


## 2026-08-18 — Criterion analysis and criterion-average V1 refit

### Completed
- Compared each of the four supporting criteria with the holistic overall grade using Pearson and Spearman correlations.
- Constructed an equal-weight criterion average at review and proposal level.
- Added proposal-level descriptive comparison figures.
- Re-fitted V1 using the equal-weight criterion average as the response.
- Compared posterior-mean ranks, expected ranks and rank uncertainty between the holistic-grade and criterion-average fits.
- Added the criterion-analysis results to the technical report.

### Main findings
- Scientific quality is much more strongly associated with the holistic overall grade than the other three criteria.
- The equal-weight criterion average is strongly related to the holistic grade but does not reproduce the same proposal ordering.
- Changing the response definition has a substantially larger effect on ranking than changing from V0 to V1.
- The criterion-average model produces somewhat narrower rank intervals, but this cannot be interpreted as showing that the criterion average is a better evaluation measure.

### Next steps
- Finish the detailed disagreement analysis.
- Refine the report structure and reduce repetition.
- Prepare the graph-based review-design chapter.


## 2026-08-19 — Reviewer-disagreement analysis and appendix expansion

### Completed
- Investigated why the posterior predictive mean within-proposal reviewer disagreement is close to the observed value.
- Derived the distribution of the difference between two reviews of the same proposal under V1.
- Showed analytically that the shared proposal effect cancels from the reviewer difference.
- Derived the folded-normal expectation for absolute reviewer disagreement.
- Compared analytic and simulated disagreement values.
- Decomposed the model-implied disagreement into residual and persistent reviewer components.
- Added detailed disagreement derivations to the appendix.
- Added threshold-based disagreement checks and additional posterior predictive summaries.

### Main findings
- The close posterior predictive mean disagreement is not mechanically caused by the shared proposal effect.
- The analytic expected disagreement closely matches the posterior predictive simulation.
- Most disagreement is explained by residual review-level variation rather than persistent reviewer effects.
- The continuous Gaussian model underpredicts the frequency of disagreements of at least two score points.

### Next steps
- Investigate whether mapping replicated scores to the observed integer 1--5 scale changes the disagreement diagnostic.
- Continue polishing the validation chapter and appendix.
- Begin literature review for the graph-based design extension.


## 2026-08-20 — Discrete-score PPC refinement and graph-design literature

### Completed
- Mapped posterior predictive replicated grades to the observed integer-valued 1--5 grid.
- Recomputed reviewer-disagreement summaries after discretization.
- Added the discretized disagreement results to the report and appendix.
- Reviewed literature on incomplete block designs, graph Laplacians, reviewer allocation and targeted data collection.
- Identified Bailey and Cameron, Cook et al., Osting et al., PeerReview4All and Simpson & Roberts as the main references for Chapter 6.
- Reworked the conceptual framing of the graph-based extension to avoid unsupported novelty claims.
- Defined the reviewer--proposal assignment structure as a bipartite graph / Levi-graph analogue.

### Main findings
- Mapping replicated scores to the 1--5 grid does not improve the mean disagreement match but substantially reduces the discrepancy in the threshold-based disagreement statistic.
- The graph-design extension can be grounded in established block-design, ranking and assignment literature.
- Bailey and Cameron provide the block-design and Laplacian foundation, while Osting et al. provide the closest precedent for targeted augmentation of an existing ranking graph.
- The CRS extension should be presented as an application and combination of established ideas, not as a new graph-theoretic method.

### Next steps
- Write Sections 6.1--6.4.
- Derive the connection between the graph Laplacian and V1 posterior precision.
- Define appropriate A-, D- and E-optimality criteria for proposal contrasts.
- Discuss the planned Chapter 6 methodology with Rachel.


## 2026-08-21 — Meeting with Rachel and substantial Chapter 6 implementation

### Completed
- Discussed the current report and validation results with Rachel.
- Rachel considered the computational validation and prior-sensitivity analysis sufficient.
- Discussed the Gaussian 1--5 support mismatch and agreed that it should be stated explicitly as a modelling caveat.
- Discussed the reviewer-disagreement PPC and clarified the need to understand why the simulated disagreement is close to the observed value.
- Confirmed the graph-based review-design direction and the need to ground the design criterion in existing literature.
- Finalized Sections 6.1--6.5 of the graph-based review-design chapter.
- Added the bipartite reviewer--proposal graph representation and an illustrative subset figure.
- Derived the incidence-matrix and graph-Laplacian representation of V1.
- Derived the conditional posterior precision
  $Q=P_0+\sigma^{-2}L_G$ and moved the full derivation to the appendix.
- Defined proposal-contrast uncertainty and A-, D- and E-optimality criteria.
- Restricted the design problem to one additional currently unobserved reviewer--proposal edge.
- Implemented the single-review design analysis for all 294 candidate edges.
- Used posterior averaging over V1 scale-parameter draws rather than a single plug-in estimate.
- Implemented the Sherman--Morrison rank-one covariance update.
- Ran the review-design analysis using 1,000 and 5,000 posterior draws.
- Added Monte Carlo stability wording to Section 6.5.
- Added new analysis and visualization scripts and review-design result folders.

### Main findings
- The Chapter 6 problem can be expressed directly through the existing V1 covariance structure and the reviewer--proposal graph.
- A- and D-optimality give strongly similar candidate-edge rankings.
- E-optimality behaves substantially differently because it targets the worst posterior contrast direction rather than average or global uncertainty.
- The 1,000- and 5,000-draw analyses give materially the same scientific conclusions, so 5,000 draws are sufficient for the final analysis.
- Several top candidate edges are nearly tied, so exact first-place assignment should not be overinterpreted.
- The review-design output is a statistical information ranking only; reviewer expertise, conflicts and operational eligibility are not included.

### Next steps
- Add a posterior-median plug-in sensitivity check for the review-design analysis.
- Write the numerical results into Sections 6.5 and 6.6.
- Create the candidate-edge heatmap and criterion-comparison visualizations.
- Write Section 6.7 on generalization and practical constraints.
- Finish the discussion, recommendations, abstract and conclusion.
- Complete one full self-review before Rachel's detailed report read.

## 2026-08-22 — Finalized single-review design results

### Completed
- Completed the 5,000-draw posterior-averaged single-review design analysis.
- Added the posterior-median plug-in sensitivity calculation.
- Compared A-, D- and E-optimal candidate rankings.
- Created the A-optimality candidate heatmap.
- Wrote the numerical single-review results and sensitivity analysis into Chapter 6.

### Main findings
- P026--R02, P005--R02 and P015--R02 form an almost tied highest-value A-optimal group.
- A- and D-optimality produce strongly similar candidate rankings, while E-optimality differs substantially.
- The highest-ranked assignments are stable to the posterior-median plug-in calculation.
- High utility for assignments involving R02 reflects the fitted design structure and should not be interpreted as reviewer quality.

### Next steps
- Extend the single-review analysis to small multi-review budgets.
- Compare random, disagreement-targeted and sequential model-based allocation.


## 2026-08-23 — Multi-review allocation policy comparison

### Completed
- Implemented the comparison of random, disagreement-targeted and sequential A-optimal allocation.
- Evaluated budgets of one to six additional reviews with at most one extra review per proposal.
- Used 5,000 posterior draws and 5,000 balanced random allocation paths.
- Added the review-policy comparison figure and results to Chapter 6.
- Extended the discussion to practical constraints and future graph-design questions.

### Main findings
- At six additional reviews, global A-optimality improved by 1.592% under random allocation, 1.604% under disagreement-targeting and 1.609% under sequential A-optimal allocation.
- The three policies therefore perform almost identically for the global A-optimality objective in the present CRS design.
- This result does not imply that the policies are equivalent for local or decision-focused objectives.

### Next steps
- Complete the discussion, recommendations and conclusion.
- Perform a systematic final edit of the full technical report.


## 2026-08-24 to 2026-08-25 — Final report restructuring and editing

### Completed
- Systematically edited Chapters 1--8 to reduce repetition while retaining the substantive explanations and previous supervisor feedback.
- Revised the abstract for accessibility and clarified the distinction between the CRS qualification rule, observed-score ranking and Bayesian ranking.
- Improved cross-references, figure/table captions and notation throughout the report.
- Refined the validation, criterion-analysis and graph-design interpretations.
- Reviewed and cleaned the supplementary derivations and diagnostics.
- Reduced PDF margins, retained A4 formatting and separated the supplement onto a new page.

### Main findings
- The report now distinguishes more clearly between model uncertainty, computational stability and deterministic decision rules.
- Changing the evaluation outcome has a larger effect on ranking than changing from V0 to V1.
- The graph-design extension can be presented as an application of established optimal-design ideas rather than as a new graph-theoretic method.

### Next steps
- Discuss the near-final report with Rachel.
- Incorporate final supervisor comments.
- Begin preparing the project presentation.


## 2026-08-26 — Final meeting with Rachel and notation/interpretation revisions

### Completed
- Discussed the near-final technical report with Rachel.
- Identified ambiguity caused by switching between review-level notation and proposal/reviewer indices.
- Revised V0 and V1 so that review $r$ is linked explicitly to proposal $p(r)$ and reviewer $a(r)$.
- Made the likelihood dependencies explicit using $\theta_{p(r)}$, $\lambda_{p(r),a(r)}$ and $b_{a(r)}$.
- Clarified the interpretation of the ranking-comparison figure.
- Added that the deterministic observed-score ranking is already highly clustered because only seven distinct proposal mean grades occur.
- Discussed the need to explain the graph-based review-design chapter much more intuitively in the presentation.

### Main findings
- The deterministic observed-score rule is fixed given the observed scores, but this does not imply a precisely identified underlying proposal ordering.
- Large score clusters are already present before Bayesian modelling; expected ranks make uncertainty in the latent ordering explicit.
- The notation is clearer when $r$ indexes reviews while $i$ and $j$ index the distinct proposal and reviewer effects.
- Chapter 6 is mathematically documented in the report, but the presentation should focus on the practical question and intuition rather than the full derivation.

### Next steps
- Complete one final proofread of the technical report.
- Build an intuitive understanding of Chapter 6 for oral explanation.
- Prepare the final project presentation.

#Template
## YYYY-MM-DD — Short description

### Completed

### Main findings

### Next steps