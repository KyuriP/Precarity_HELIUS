# Precarity–Depression Explorer (Shiny)

DINAMICS-2 stakeholder tool (WP2.2: communicating leverage points).
Simulates the calibrated linear feedback model from Park et al. (2026),
*SSM – Mental Health* 9, 100637, using the **causalnet** package.

## Run locally
```r
install.packages(c("shiny", "ggplot2", "causalnet"))
shiny::runApp("dinamics-shiny")
```

## Deploy (shinyapps.io)
```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp("dinamics-shiny", appName = "precarity-depression-explorer")
```

## What it does
- **Explore support**: financial support (each person's financial stress is shifted toward the
  lowest HELIUS level, as in the paper), plus two discussion levers that are not in the paper
  (mental health support on D, social support on P). The uncertain feedback strength
  alpha_DP (0–0.65) can be compared against the opposite end of its admissible range.
- **Possible structures**: `generate_directed_networks()` enumerates the D–P orientations
  compatible with the skeleton (S fixed as exogenous); `summarize_network_metrics()`
  reports the loops.

## Implementation notes
- Model: dD = (a_DS S + a_DP P − D) dt + σ_D dW, dP = (a_PS S + a_PD D − P) dt + σ_P dW.
  Run as `simulate_dynamics(model_type = "linear")` on a weighted S/D/P network with
  `alpha_self = -1` for D and P; interventions enter through `stress_event`.
- Calibration uses the HELIUS covariances (Var D = 1, Var P = 0.807, Var S = 0.743,
  Cov DP = 0.336, Cov DS = 0.308, Cov PS = 0.198) and reproduces them exactly. (The paper's appendix
  lists the correlations 0.374 / 0.357 / 0.256 under the covariance label; its simulation script uses
  the covariances, as here.)
- Baseline financial stress is drawn from its HELIUS distribution (8 levels of the composite).
- Social precarity is shown in its own standard deviations (SD = √0.807).
- Verified against the web dashboard: identical trajectories (max difference 0.000005).
- Population means are computed without noise by default (in a linear model, noise averages out);
  tick "Include day-to-day fluctuation" to see the spread between residents.
- σ_P uses the paper's appendix formula. Note: `04_linear_model.R` sets
  `sigma_p = sqrt(sigma_d2)`, so the published runs used σ_P = σ_D. This affects only
  the spread between individuals, not the population means.
