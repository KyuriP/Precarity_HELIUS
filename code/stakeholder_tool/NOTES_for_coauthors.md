# Notes for co-authors: Park et al. (2026), SSM – Mental Health

Found while building and auditing the DINAMICS-2 stakeholder tool (7 Oct 2026). Checked against the
revision-2 draft (`draft_v4_SSM_anonymized_LE_KS_clean.qmd`); please check whether the published
version has the same text.

## 1. Appendix calibration table lists correlations, not covariances

The appendix lists Cov DP = 0.374, Cov DS = 0.357, Cov PS = 0.256.
These are the *correlations* (0.3735, 0.3574, 0.2552), not the covariances. The simulations in the
paper are not affected: `04_linear_model.R` computes `cov()` from the data, and the saved results
(`summarysnap_linearmodel.rds`) match the covariance-based model within Monte Carlo error
(e.g. full support, t = 1000 steps: paper D ≈ −0.35, covariance model −0.353, appendix values −0.409).
What needs correcting in the paper: the appendix table and the admissible range (0.672, not 0.698).

Exact values from `HELIUS_LEONIE.sav` with the paper's preprocessing (n = 21,628):

| | Covariance | Correlation |
|---|---|---|
| D, P | 0.335533 | 0.373477 |
| D, S | 0.308064 | 0.357440 |
| P, S | 0.197564 | 0.255151 |

Var D = 1, Var P = 0.807129, Var S = 0.742806.

## 2. Admissible range for α_DP

The upper bound 0.698 (where α_PD reaches 0) follows from the correlations. With the covariances it is
**0.672**. The simulations used α_DP ≤ 0.65, which is inside the correct range (α_PD = 0.019 at 0.65),
so the results stand; only the stated range in the methods and appendix needs updating.

## 3. σ_P in the simulation script

`04_linear_model.R` sets `sigma_p = sqrt(sigma_d2)` in `get_model_parameters_from_alpha_dp()`, so the
published runs used σ_P = σ_D instead of the appendix formula. This affects only the spread between
individuals, not the population means shown in the figures.
