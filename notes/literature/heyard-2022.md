# Heyard et al. (2022)

## Full reference

Heyard, R., Ott, M., Salanti, G., & Egger, M. (2022). Rethinking the Funding Line at the Swiss National Science Foundation: Bayesian Ranking and Lottery. Statistics and Public Policy, 9(1), 110–121.

## Research question

- How can grant proposals be ranked while accounting for reviewer differences and uncertainty?
- How can proposals that are too similar to separate clearly near the funding line be identified?

## Evaluation setting and data

- The method is applied to two SNSF funding schemes.
- Proposals receive numerical scores from several reviewers or panel members.
- The paper compares simple average rankings with Bayesian rankings.

## Statistical approach

- Bayesian hierarchical model separating proposal quality, reviewer scoring behaviour and remaining variation.
- Proposal rankings are summarized using expected ranks and credible intervals.
- Proposals with uncertain ranks around the funding line may enter a lottery.

## Main assumptions

- Each proposal has an underlying quality that cannot be observed directly.
- Some reviewers are generally stricter or more generous than others.
- The basic model treats the score scale as approximately continuous.

## Main findings

- Bayesian rankings were often similar to average rankings, but some proposals changed position.
- The Bayesian model revealed considerable uncertainty that was hidden by simple averages.
- A relatively simple model worked well and was recommended over more complicated versions.

## Relevance to this project

- Provides the main model and ranking method for the CRS analysis.
- Allows reviewer scoring tendencies and proposal quality to be considered separately.
- CRS differs because it used a qualification threshold and only two reviews per proposal.

## Questions and unclear points

- Can reviewer effects be estimated reliably with the small CRS dataset?
- Are the paper’s default model and priors suitable for the CRS 1–5 scale?
- Which Bayesian output is most useful when there is no fixed funding line?

## Possible ideas

- Compare raw mean ranking with Bayesian expected ranking.
- Plot rank uncertainty and reviewer scoring tendencies.
- Test how sensitive the results are to model assumptions.