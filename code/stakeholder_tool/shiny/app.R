# =============================================================================
# DINAMICS-2 stakeholder tool: Precarity–Depression Explorer (Shiny)
# -----------------------------------------------------------------------------
# Simulates the calibrated linear feedback model from
#   Park, Elsenburg, Nicolaou, Stronks & Vasconcelos (2026). Connecting
#   precariousness and depression: From causal discovery to intervention
#   simulation. SSM - Mental Health, 9, 100637.
# using the causalnet package (Park, Vasconcelos & Lees, 2026, BRM).
#
# Model (lambda_D = lambda_P = 1):
#   dD = (a_DS*S + a_DP*P - D - u_D(t)) dt + sigma_D dW
#   dP = (a_PS*S + a_PD*D - P - u_P(t)) dt + sigma_P dW
# Implemented in causalnet::simulate_dynamics(model_type = "linear") on a
# weighted 3-node network (S, D, P) with alpha_self = -1 for D and P.
# Interventions enter through causalnet's `stress_event` hook.
# =============================================================================

library(shiny)
library(causalnet)
library(ggplot2)

# ---- Calibration (paper appendix; HELIUS second-order moments) --------------
# Observed HELIUS variances and covariances (D = PHQ-9 z-score, P = social precarity, S = financial stress).
# These are covariances, as used by 04_linear_model.R; the paper's appendix table lists the correlations.
MOM    <- list(vD = 1, vP = 0.807129, vS = 0.742806, cDP = 0.335533, cDS = 0.308064, cPS = 0.197564)
# Financial-stress composite: its 8 standardized levels and their frequencies in HELIUS (n = 21,628)
S_LEVELS <- c(-0.8524, -0.3616, 0.1291, 0.4147, 0.6198, 0.9054, 1.3961, 1.8869)
S_COUNTS <- c(5966, 6705, 3657, 195, 1127, 536, 1470, 1972)
S_MIN  <- -0.852379   # lowest financial-stress level in HELIUS (standardized composite, mean 0)
PHQ_SD <- 5.13     # SD of PHQ-9 sum score in the analytic sample
T0     <- 3        # support starts here (model time units)
TMAX   <- 30

calibrate <- function(a_dp, m = MOM) {
  with(m, {
    den  <- cDS^2 - vD * vS
    a_ds <- (cDS - a_dp * cPS) / vS
    a_pd <- (-a_dp * cPS^2 + a_dp * vP * vS - 2 * cDP * vS + 2 * cDS * cPS) / den
    a_ps <- (-cDS^2 * cPS + cDS * (a_dp * cPS^2 - a_dp * vP * vS + 2 * cDP * vS) - cPS * vD * vS) / (vS * den)
    sd2  <- -(2 * (cDS^2 - vD * vS - cDS * cPS * a_dp + cDP * vS * a_dp)) / vS
    sp2  <- (8 * cDP * cDS * cPS * vS - 4 * cDP^2 * vS^2 - 2 * cDS^2 * (cPS^2 + vP * vS) +
               2 * cDS * cPS * (cPS^2 - vP * vS) * a_dp +
               2 * vS * (-cPS^2 + vP * vS) * (vD + cDP * a_dp)) / (vS * (-cDS^2 + vD * vS))
    list(a_dp = a_dp, a_ds = a_ds, a_pd = a_pd, a_ps = a_ps, sig_d = sqrt(sd2), sig_p = sqrt(sp2))
  })
}

# Weighted adjacency (row -> column), as causalnet expects
build_network <- function(p) {
  nodes <- c("S", "D", "P")
  A <- matrix(0, 3, 3, dimnames = list(nodes, nodes))
  A["S", "D"] <- p$a_ds; A["S", "P"] <- p$a_ps
  A["P", "D"] <- p$a_dp; A["D", "P"] <- p$a_pd
  A
}

# Simulate one person with causalnet; S0 = their baseline financial stress
simulate_person <- function(p, S0, fin, mh, soc, dur, dt = 0.05, noise = TRUE) {
  A <- build_network(p)
  params <- list(
    beta       = c(S = 0, D = 0, P = 0),
    alpha_self = c(S = 0, D = -1, P = -1),           # relaxation toward target
    sigma      = if (noise) c(S = 0, D = p$sig_d, P = p$sig_p) else c(S = 0, D = 0, P = 0)
  )
  t1 <- T0 + dur
  dS <- -fin * (S0 - S_MIN)                          # shift toward lowest observed stress
  ev <- function(t, state) {
    out <- c(0, 0, 0)
    if (t < T0 && t + dt >= T0) out[1] <- dS         # support switches on
    if (t < t1 && t + dt >= t1) out[1] <- -dS        # support switches off
    if (t >= T0 && t < t1) out[2:3] <- -c(mh, soc) * dt
    out
  }
  # start at the person's stationary mean so trajectories begin in equilibrium
  M  <- solve(matrix(c(1, -p$a_dp, -p$a_pd, 1), 2, byrow = TRUE), c(p$a_ds, p$a_ps) * S0)
  simulate_dynamics(A, params, t_max = TMAX, dt = dt, S0 = c(S0, M[1], M[2]),
                    model_type = "linear", stress_event = ev, boundary = "none")
}

simulate_population <- function(a_dp, fin, mh, soc, dur, n = 150, seed = 1, noise = FALSE) {
  set.seed(seed)
  p  <- calibrate(a_dp)
  S0 <- sample(S_LEVELS, n, replace = TRUE, prob = S_COUNTS)   # HELIUS distribution of financial stress
  sims <- lapply(S0, function(s) simulate_person(p, s, fin, mh, soc, dur, noise = noise))
  time <- attr(sims[[1]], "time")
  base <- t(sapply(sims, function(m) m[1, c("D", "P")]))
  chg  <- function(v) sapply(seq_along(sims), function(i) sims[[i]][, v] - base[i, v])
  dD <- chg("D") * PHQ_SD; dP <- chg("P") / sqrt(MOM$vP)   # P in its own SDs
  rbind(
    data.frame(time, var = "Depressive symptoms (PHQ-9 points)", mean = rowMeans(dD),
               lo = apply(dD, 1, quantile, .25), hi = apply(dD, 1, quantile, .75)),
    data.frame(time, var = "Social precarity (SD)", mean = rowMeans(dP),
               lo = apply(dP, 1, quantile, .25), hi = apply(dP, 1, quantile, .75))
  )
}

# ---- Candidate structures (causalnet enumeration) --------------------------
skeleton <- matrix(1, 3, 3, dimnames = list(c("S", "D", "P"), c("S", "D", "P"))); diag(skeleton) <- 0
fixed <- matrix(NA_real_, 3, 3, dimnames = dimnames(skeleton))
fixed["S", "D"] <- 1; fixed["S", "P"] <- 1          # stress is exogenous in the scenario model
candidate_nets <- generate_directed_networks(skeleton, allow_bidirectional = TRUE,
                                             fixed_edges = fixed, show_progress = FALSE)

draw_net <- function(A, title) {
  xy <- rbind(S = c(0.5, 0.9), D = c(0.1, 0.15), P = c(0.9, 0.15))
  plot.new(); plot.window(c(-0.1, 1.1), c(-0.05, 1.05)); title(title, cex.main = 1.4)
  cols <- c(S = "#7a3b52", D = "#b8780a", P = "#2c5f86")
  for (i in rownames(A)) for (j in colnames(A)) if (A[i, j] != 0) {
    bend <- if (A[j, i] != 0) 0.04 else 0
    off  <- c(-(xy[j, 2] - xy[i, 2]), xy[j, 1] - xy[i, 1]) * bend * 3
    a <- xy[i, ] + 0.16 * (xy[j, ] - xy[i, ]) + off
    b <- xy[j, ] - 0.16 * (xy[j, ] - xy[i, ]) + off
    arrows(a[1], a[2], b[1], b[2], length = 0.1, lwd = 2, col = cols[i])
  }
  symbols(xy[, 1], xy[, 2], circles = rep(0.08, 3), inches = FALSE, add = TRUE, bg = "white", fg = cols)
  text(xy[, 1], xy[, 2], rownames(xy), font = 2, col = cols, cex = 1.6)
}

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Precarity–Depression Explorer", windowTitle = "Precarity–Depression Explorer"),
  p("DINAMICS-2 stakeholder tool. Model calibrated to the HELIUS study (N > 21,000, Amsterdam).",
    "Directions of influence are data-compatible hypotheses, not proven causal effects."),
  tabsetPanel(
    tabPanel("Explore support",
      sidebarLayout(
        sidebarPanel(
          sliderInput("fin", "Financial support: lower financial stress toward the lowest HELIUS level (%)", 0, 100, 60, 5),
          sliderInput("mh",  "Mental health support (beyond paper, SD units)", 0, 0.5, 0, 0.05),
          sliderInput("soc", "Social support (beyond paper, SD units)", 0, 0.5, 0, 0.05),
          sliderInput("dur", "Duration of support (model time units)", 1, 24, 12, 1),
          hr(),
          sliderInput("adp", "Uncertain: how strongly precarity drives depression (alpha_DP)", 0, 0.65, 0, 0.05),
          checkboxInput("cmp", "Compare with the opposite end of the uncertainty range", TRUE),
          numericInput("n", "Simulated residents", 150, 20, 1000, 10),
          checkboxInput("noise", "Include day-to-day fluctuation (shows spread between residents; slower and noisier)", FALSE)
        ),
        mainPanel(
          plotOutput("traj", height = "460px"),
          tableOutput("params"),
          helpText("Lines: population mean change from baseline. Bands (with fluctuation on): middle 50% of simulated residents.",
                   "Shaded: support switched on. Simulation via causalnet::simulate_dynamics(model_type = 'linear').")
        )
      )
    ),
    tabPanel("Possible structures",
      br(),
      p(sprintf("With financial stress (S) fixed as an outside driver, causalnet enumerates %d orientation-consistent networks for depression (D) and social precarity (P).",
                length(candidate_nets)),
        "The cross-sectional data cannot tell these apart; the explorer's uncertainty slider moves between them."),
      plotOutput("nets", height = "300px"),
      tableOutput("loops")
    ),
    tabPanel("About",
      br(),
      p("Model: Park et al. (2026), SSM - Mental Health 9, 100637, https://doi.org/10.1016/j.ssmmh.2026.100637"),
      p("Software: causalnet (CRAN), Park, Vasconcelos & Lees (2026), Behavior Research Methods 58, 204."),
      p("The mental health and social support levers are discussion extensions and were not part of the published simulation.",
        "Baseline financial stress is drawn from its HELIUS distribution (8 levels of the standardized composite).")
    )
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {
  sim <- reactive({
    a_opp <- if (input$adp <= 0.325) 0.65 else 0
    cur <- simulate_population(input$adp, input$fin / 100, input$mh, input$soc, input$dur, input$n, noise = input$noise)
    cur$setting <- sprintf("alpha_DP = %.2f (current)", input$adp)
    if (isTRUE(input$cmp)) {
      opp <- simulate_population(a_opp, input$fin / 100, input$mh, input$soc, input$dur, input$n, noise = input$noise)
      opp$setting <- sprintf("alpha_DP = %.2f (opposite end)", a_opp)
      rbind(cur, opp)
    } else cur
  })

  output$traj <- renderPlot({
    d <- sim()
    ggplot(d, aes(time, mean, colour = setting, fill = setting)) +
      annotate("rect", xmin = T0, xmax = min(TMAX, T0 + input$dur), ymin = -Inf, ymax = Inf, alpha = 0.12) +
      geom_hline(yintercept = 0, colour = "grey60") +
      { if (isTRUE(input$noise)) geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) } +
      geom_line(linewidth = 1) +
      facet_wrap(~var, scales = "free_y", ncol = 2) +
      scale_colour_manual(values = c("#b8780a", "#2c5f86")) +
      scale_fill_manual(values = c("#b8780a", "#2c5f86")) +
      labs(x = "Time (model units)", y = "Change from baseline", colour = NULL, fill = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })

  output$params <- renderTable({
    p <- calibrate(input$adp)
    data.frame(`S->D` = p$a_ds, `S->P` = p$a_ps, `P->D` = p$a_dp, `D->P` = p$a_pd,
               sigma_D = p$sig_d, sigma_P = p$sig_p, check.names = FALSE)
  }, digits = 3, caption = "Calibrated parameters at the current setting")

  output$nets <- renderPlot({
    k <- length(candidate_nets)
    par(mfrow = c(1, k), mar = c(0, 0, 2, 0))
    for (i in seq_len(k)) {
      A <- candidate_nets[[i]]
      lab <- if (A["D", "P"] && A["P", "D"]) "D <-> P (feedback)" else if (A["D", "P"]) "D -> P" else "P -> D"
      draw_net(A, lab)
    }
  })

  output$loops <- renderTable({
    s <- summarize_network_metrics(candidate_nets)
    s$structure <- sapply(candidate_nets, function(A)
      if (A["D", "P"] && A["P", "D"]) "D <-> P" else if (A["D", "P"]) "D -> P" else "P -> D")
    s[, c("structure", setdiff(names(s), c("structure", "net_id")))]
  }, caption = "causalnet::summarize_network_metrics")
}

shinyApp(ui, server)
