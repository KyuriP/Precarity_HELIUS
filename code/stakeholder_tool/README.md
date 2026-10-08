# Precarity–Depression Explorer

Stakeholder tool for the **DINAMICS-2** project (WP2.2: communicating leverage points).
Live at **https://kyuripark.net/misc/tools/precarity-explorer/**
User guide (EN/NL): https://kyuripark.net/misc/tools/precarity-explorer/guide/

It shows a simulated neighbourhood of 300 residents whose financial stress, social precarity
and depressive symptoms are calibrated to HELIUS (n = 21,628 Amsterdam adults). Users switch
support measures on and off, change how the system works, and see the result straight away.

Based on: Park, Elsenburg, Nicolaou, Stronks & Vasconcelos (2026). *Connecting precariousness and
depression: From causal discovery to intervention simulation.* SSM – Mental Health 9, 100637.
https://doi.org/10.1016/j.ssmmh.2026.100637

---

## 1. Files

| File | What it is |
|---|---|
| `precarity-explorer.html` | The tool. One self-contained file (HTML + CSS + JavaScript, no libraries). Double-click to open in any browser. |
| `shiny/app.R`, `shiny/README.md` | Same model as an R Shiny app built on `causalnet`, for researchers. See `shiny/README.md`. |
| `README.md` | This file. |

The website copy lives in the site repo at `kyurip.github.io/misc/tools/precarity-explorer/index.html`
(the same page with a full `<head>`), listed on `kyuripark.net/misc/tools/` (`_pages/tools.md`).

---

## 2. What a user sees

| Part | What it does |
|---|---|
| **Guided tour** (top left) | Five steps that set up a scenario and explain it: a support programme; making it short; a strong loop; adding mental health support; free exploration. Each step pins the previous scenario as a dashed line for comparison. |
| **1 · Plan the support** | Three measures, each with an on/off switch, *strength*, *start time* and *duration*: **financial support** (as in the paper), **mental health support** and **social support** (both marked *extra*, see §5). |
| **2 · Tweak the system** | Three **HELIUS presets** (weak / medium / strong loop) that fit the data, plus sliders for each arrow strength, recovery speed and day-to-day fluctuation. Clicking an arrow in the diagram jumps to its slider. |
| **Graph: what happens over time** | Top: % of residents with PHQ-9 ≥ 10. Bottom: average change in social precarity, in standard deviations of social precarity (SD = √0.807). Coloured strips above the graph show when each measure is on. The full scenario is drawn faintly; the solid line draws up to the playhead. Redraws immediately when anything changes. |
| **Play controls** | Play/Pause (also the space bar), *From the start*, speed (Slow = 0.5, Normal = 1, Fast = 2 time units per second). Plays once and stops; dragging across the graph moves through time. *Pin for comparison* keeps the current scenario as a dashed line. |
| **Tiles** | At the playhead: residents with PHQ-9 ≥ 10, average PHQ-9 (and change from the start), social precarity change, financial stress as % of usual. |
| **The neighbourhood** | One dot per simulated resident, coloured by PHQ-9 severity band (0–4, 5–9, 10–14, 15–19, 20+) at the playhead. Hover for that resident's financial-stress level and PHQ-9. The grid has no geographic meaning. |
| **What drives what** | System map. Arrow thickness and flow speed = arrow strength. Node fill shows the current level. Badges show which measures are on. |
| **Leverage points / notes** | Four take-home points from the paper, model summary, caveats and sources. EN/NL toggle at the top right. |

---

## 3. The model

Each resident *i* has a fixed financial stress level *S_i*. Depressive symptoms *D* and social
precarity *P* (both standardized) move toward a target set by *S* and by each other:

```
dD = λ (a_SD·S + a_PD·P − D − u_D(t)) dt + σ_D dW
dP = λ (a_SP·S + a_DP·D − P − u_P(t)) dt + σ_P dW
```

- `a_SD`, `a_SP`: financial stress → depression / → social precarity
- `a_PD`, `a_DP`: social precarity → depression / depression → social precarity (the loop)
- `λ`: recovery speed (1 in the paper). Time is in model units: 1 unit ≈ the time the system needs to adjust.
- `σ_D`, `σ_P`: day-to-day fluctuation
- `u_D`, `u_P`: mental health / social support while switched on (0 otherwise)

Note on naming: the paper writes the P→D arrow as α_DP and D→P as α_PD. The tool labels arrows by
direction in plain words, so there is no ambiguity on screen.

**Financial support** follows the paper: while on, each resident's stress is moved a fraction *f*
toward the lowest level in HELIUS: `S_i → S_i − f·(S_i − S_min)`, with `S_min = −0.852`.

**Numerics.** Euler–Maruyama, dt = 0.025, 40 time units, 300 residents. The noise is scaled by
√(1 − λ·dt/2), which removes the small variance inflation of the Euler step (without it, variances come
out about 2% too high). Residents first settle for 10 units without support. The random shocks come from one fixed table, so every scenario gets the
same "luck" and only the user's changes move the curves. Curves are smoothed over ±0.8 units for display.

### Calibration (HELIUS presets)

Closed-form solution from the paper's appendix, applied to the observed HELIUS **covariances**
(n = 21,628, computed with the paper's preprocessing):
Var D = 1, Var P = 0.807129, Var S = 0.742806, Cov DP = 0.335533, Cov DS = 0.308064, Cov PS = 0.197564.
The P→D strength is free and everything else follows:

| Preset | P→D | D→P | S→D | S→P | σ_D | σ_P |
|---|---|---|---|---|---|---|
| Weak loop | 0.00 | 0.582 | 0.415 | 0.025 | 1.321 | 1.102 |
| Medium | 0.30 | 0.322 | 0.335 | 0.132 | 1.262 | 1.160 |
| Strong loop | 0.65 | 0.019 | 0.242 | 0.258 | 1.190 | 1.225 |

All three reproduce the observed moments exactly (checked analytically with the Lyapunov equation).
P→D can go up to 0.672 before D→P turns negative.

(See `NOTES_for_coauthors.md` for a note on the covariance values in the paper's appendix.)

If a user sets the loop so strong that
P→D × D→P ≥ 0.95, the tool shows a "runs away" warning.

---

## 4. Data used and how PHQ-9 is shown

Only **summary distributions** from HELIUS are in the page, no individual records:

- **Financial stress (S.fin):** the composite takes 8 standardized levels; their frequencies set how many of
  the 300 residents get each level (largest-remainder allocation, then shuffled across the grid).
- **PHQ-9 sum score:** the cumulative distribution (share with score ≤ k, k = 0…27).

**Why the conversion matters.** The model runs on standardized (z) scores, which are symmetric. Real
PHQ-9 scores are not: 62% score 0–4 and 22% score exactly 0. A straight back-transformation
(PHQ = 4.65 + 5.13·z) gets the mean right but puts far too few people in 0–4 and almost nobody at 20+.
The tool therefore converts **by percentile**: a resident at the 85th percentile of the model gets the
PHQ-9 score at the 85th percentile of HELIUS. The PHQ-9 ≥ 10 cut-off is z = 1.062.

Check of the starting state (no support; average over the three presets):

| | 0–4 | 5–9 | 10–14 | 15–19 | 20+ | Mean PHQ-9 | ≥ 10 |
|---|---|---|---|---|---|---|---|
| HELIUS | 62.4% | 23.2% | 8.4% | 3.7% | 2.4% | 4.65 | 14.4% |
| Tool | 63.0% | 22.7% | 8.2% | 3.6% | 2.5% | 4.61 | 14.2% |

The tiles and graph use this percentile conversion. (Average *changes* in PHQ-9 points in the paper and
the Shiny app use the linear conversion, 1 SD = 5.13 points, which is exact for means of the z-score.)

---

## 5. What goes beyond the paper

| Feature | Status |
|---|---|
| Financial support, HELIUS presets, loop-strength comparison | As in the paper |
| Mental health support, social support | **Extra**: simple shifts in the D or P target while switched on; for discussion |
| Free arrow sliders, recovery speed, fluctuation | **Extra**: what-if exploration; once changed, the model no longer matches HELIUS moments |
| Support start/duration per measure | Generalises the paper's intervention on/off window |

The page says this in the "Keep in mind" notes and labels the extra measures.

---

## 6. Caveats to mention when presenting

- Residents are simulated, not real people. Numbers show direction and relative size, not forecasts.
- HELIUS is cross-sectional: arrow directions are data-compatible hypotheses, not identified causal effects.
- The model is linear (no thresholds or tipping points).
- Model time units are not calendar time.

---

## 7. Updating the tool

1. Edit `precarity-explorer.html` (all model code is in the `<script>` at the bottom; constants and
   calibration are at the top of the script; texts for Dutch are in the `NL` object).
2. Copy it into the site repo as `misc/tools/precarity-explorer/index.html`, keeping the `<!doctype>`,
   `<html>`, `<head>` lines at the top and `</body></html>` at the end.
3. Push. The site rebuilds in about 15 minutes. `_config.yml` excludes `misc/tools/**/*` from the
   minifier so the JavaScript is not altered.

## 8. Checks done (audit, 7 Oct 2026)

| Check | Result |
|---|---|
| Calibration formulas vs paper appendix and `04_linear_model.R` | Same closed form; JS values equal R values to 4 decimals |
| Presets reproduce all six HELIUS moments (analytic Lyapunov solution, in R and in JS) | Exact (to 4 decimals) |
| Simulated moments, 20,000 residents, the tool's own step function | Var D 0.996–0.998, Var P 0.806–0.807, Cov DP 0.331, Cov DS 0.305–0.306, Cov PS 0.195 (targets 1, 0.807, 0.336, 0.308, 0.198; the 300 residents' S variance is 0.7385 vs 0.7428) |
| Paper's saved simulation output vs this model | Matches within Monte Carlo error (see §3 note) |
| JS model vs `causalnet::simulate_dynamics(model_type = "linear")`, 12 scenarios × 9 time points | Max difference 0.000005 |
| Support effects vs closed-form equilibrium shifts | Exact (e.g. mental health 0.25 → ΔD = −0.25/(1 − P→D·D→P)) |
| Financial stress of the 300 residents vs HELIUS frequencies | Allocation matches expected counts (83/93/51/3/16/7/20/27) |
| PHQ-9 conversion cut-offs vs scipy `norm.ppf` | Max difference 5×10⁻⁷; PHQ-9 ≥ 10 cut-off falls exactly between 9 and 10 |
| Starting PHQ-9 bands, mean, % ≥ 10 vs HELIUS | See §4 table |
| Tour statements | Strong loop: slower early drop (−0.52 vs −0.69 PHQ-9 points 1 unit in), slower fade (−0.56 vs −0.39, 1 unit after stop); 3-unit support reaches 10.7% vs 7.4% for 15 units; financial + mental health 5.0% vs 8.7% |
| On-screen tiles, map labels, slider labels vs the simulation | Agree |
