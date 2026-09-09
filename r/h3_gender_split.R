#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# h3_gender_split.R
#
# Hypothesis 3 - "Unexpectedness mediates" - split by gender.
#
#   Oddity raises perceived unexpectedness, and unexpectedness carries the
#   effect of oddity on comfort.
#
# This is h3_full_set.R run twice, once over each level of `gender`:
#
#   gender = "Female"
#   gender = "Male"
#
# Each group is analysed completely independently - separate models, separate
# HC4 standard errors, separate bootstrap. This is a stratified re-run, NOT a
# moderated mediation: no cross-group test is performed, so any difference
# between the two sets of estimates is descriptive only. If you need a formal
# test of whether gender moderates a path, that is PROCESS Model 59 (or a
# bootstrap of the between-group difference), not this script.
#
# Note also that gender is a measured participant characteristic, not a
# manipulated factor, so the groups are not randomly equivalent on anything
# else; any difference is confounded with whatever else differs between them.
#
# The underlying model, per group, is the PROCESS v4.2 (Hayes, 2022) Model 4
# specification documented in recreate3.md:
#
#   PROCESS
#       y=c_discom /x=c_oddity /m=c_expect /cov=c_noise
#       /model=4 /boot=10000 /conf=95 /hc=4
#       /effsize=1 /stand=1 /total=1 /seed=20260727.
#
#   X (c_oddity)  ->  M (c_expect)  ->  Y (c_discom),   covariate: c_noise
#
#     M model:  c_expect ~ c_oddity + c_noise                 (path a)
#     Y model:  c_discom ~ c_oddity + c_expect + c_noise      (paths c', b)
#     Total:    c_discom ~ c_oddity + c_noise                 (path c)
#
# All OLS point estimates are reported with HC4 (Cribari-Neto, 2004)
# heteroscedasticity-consistent standard errors. The indirect effect a*b is
# tested with a percentile bootstrap over 10,000 case resamples.
#
# Usage:
#   Rscript source/h3_gender_split.R        # run from the project root
#   source("source/h3_gender_split.R")      # or from within RStudio
#
# Base R only - no external packages required (no `sandwich`, no `boot`,
# no `lavaan`). Results are reproducible: the RNG seed is fixed below and is
# re-set at the start of each group's bootstrap, so the two groups do not
# share a random stream and either can be reproduced in isolation.
#
# NOTE: bootstrap CIs will differ in the third decimal place from SPSS output,
# because PROCESS and R draw their resamples from different RNGs. Every OLS
# quantity (a, b, c, c', SEs, t, p, R-squared, F) is exact.
# ---------------------------------------------------------------------------


# --- Configuration ---------------------------------------------------------

SEED    <- 20260727    # matches /seed= in the SPSS syntax
N_BOOT  <- 10000       # matches /boot=
CONF    <- 95          # matches /conf=
HC_TYPE <- "HC4"       # matches /hc=4

Y_VAR   <- "c_discom"  # outcome    - discomfort  (1 = comfortable .. 5 = not)
X_VAR   <- "c_oddity"  # antecedent - oddity      (0 = absent, 1 = present)
M_VAR   <- "c_expect"  # mediator   - unexpectedness (1 .. 5)
COV_VAR <- "c_noise"   # covariate  - background activity (0 = low, 1 = high)

# The split. Each entry is one complete, independent re-run of the model.
# `gender` is a free-text column, so values are matched after trimming.
SPLIT_VAR <- "gender"
GROUPS <- list(
  list(value = "Female", label = "Female"),
  list(value = "Male",   label = "Male")
)


# --- Locate the data file --------------------------------------------------
# Works whether the script is run from the project root or from source/.

find_data <- function() {
  stem <- "data/data_ODDITY_exp_274266-v20_task-8m3r.clean.csv"
  candidates <- c(stem, file.path("..", stem))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("Could not find ", stem, " - run this from the project root.")
  }
  hit[1L]
}

path <- find_data()
raw  <- read.csv(path, stringsAsFactors = FALSE)

needed <- c(Y_VAR, X_VAR, M_VAR, COV_VAR, SPLIT_VAR)
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0L) {
  stop("Missing column(s) in ", path, ": ", paste(missing_cols, collapse = ", "))
}

dat_all <- raw[needed]
dat_all[[SPLIT_VAR]] <- trimws(as.character(dat_all[[SPLIT_VAR]]))
dat_all[[SPLIT_VAR]][dat_all[[SPLIT_VAR]] == ""] <- NA_character_
dat_all <- dat_all[stats::complete.cases(dat_all), , drop = FALSE]

n_total    <- nrow(raw)
n_complete <- nrow(dat_all)
n_dropped  <- n_total - n_complete

# Guard against unexpected codings in the split column.
seen <- sort(unique(dat_all[[SPLIT_VAR]]))
want <- vapply(GROUPS, function(g) g$value, character(1))
if (!all(want %in% seen)) {
  stop("Expected ", SPLIT_VAR, " values ", paste(want, collapse = "/"),
       " but found ", paste(seen, collapse = "/"))
}
if (!all(seen %in% want)) {
  warning(SPLIT_VAR, " contains unhandled value(s): ",
          paste(setdiff(seen, want), collapse = ", "),
          " - those rows are excluded from both groups.")
}


# --- OLS with HC4 standard errors ------------------------------------------
#
# HC4 (Cribari-Neto, 2004) inflates each squared residual by the leverage of
# its own case:
#
#   w_i     = e_i^2 / (1 - h_i)^delta_i
#   delta_i = min(4, h_i / mean(h))        and mean(h) = p / n
#
#   V(b)    = (X'X)^-1  X' diag(w) X  (X'X)^-1
#
# This is what PROCESS emits under /hc=4, and what sandwich::vcovHC(type =
# "HC4") would return - reimplemented here to keep the script dependency-free.

hc4_fit <- function(y, preds, labels) {
  n <- length(y)
  Xm <- cbind(Constant = 1, do.call(cbind, preds))
  colnames(Xm) <- c("Constant", labels)
  p <- ncol(Xm)

  XtXi <- solve(crossprod(Xm))
  b    <- as.vector(XtXi %*% crossprod(Xm, y))
  e    <- as.vector(y - Xm %*% b)

  # leverages: h_i = x_i' (X'X)^-1 x_i, without forming the full hat matrix
  h     <- rowSums((Xm %*% XtXi) * Xm)
  delta <- pmin(4, h / (p / n))
  w     <- e^2 / (1 - h)^delta

  # crossprod(Xm * sqrt(w)) == X' diag(w) X
  V  <- XtXi %*% crossprod(Xm * sqrt(w)) %*% XtXi
  se <- sqrt(diag(V))

  df_res <- n - p
  tval   <- b / se
  pval   <- 2 * stats::pt(-abs(tval), df_res)
  tcrit  <- stats::qt(1 - (1 - CONF / 100) / 2, df_res)

  ss_tot <- sum((y - mean(y))^2)
  r2     <- 1 - sum(e^2) / ss_tot

  # Omnibus test on the slopes only, using the HC4 covariance matrix. This is
  # a Wald F, not the classical R-squared F, and is what PROCESS prints when
  # an HC estimator is requested.
  slopes  <- b[-1]
  V_slope <- V[-1, -1, drop = FALSE]
  df_mod  <- p - 1
  Fstat   <- as.numeric(t(slopes) %*% solve(V_slope) %*% slopes) / df_mod
  Fp      <- stats::pf(Fstat, df_mod, df_res, lower.tail = FALSE)

  # standardised (beta) coefficients, for reference
  sd_x  <- apply(Xm, 2, stats::sd)
  beta  <- b * sd_x / stats::sd(y)
  beta[1] <- NA_real_

  names(b) <- names(se) <- names(tval) <- names(pval) <- colnames(Xm)

  list(
    term = colnames(Xm), b = b, se = se, t = tval, p = pval, beta = beta,
    lo = b - tcrit * se, hi = b + tcrit * se,
    r2 = r2, F = Fstat, df_mod = df_mod, df_res = df_res, Fp = Fp,
    n = n, resid = e, fitted = as.vector(Xm %*% b)
  )
}

# Plain OLS coefficients - used inside the bootstrap loop, where the HC4
# machinery is unnecessary (only the point estimates a and b are resampled).
ols_coef <- function(Xm, y) {
  as.vector(solve(crossprod(Xm), crossprod(Xm, y)))
}


# --- One complete H3 analysis over one subset ------------------------------
#
# Everything below the data step is identical to h3_full_set.R; it is just
# wrapped in a function so it can be applied to each gender group.

run_h3 <- function(dat, label) {

  Y  <- as.numeric(dat[[Y_VAR]])
  X  <- as.numeric(dat[[X_VAR]])
  M  <- as.numeric(dat[[M_VAR]])
  CV <- as.numeric(dat[[COV_VAR]])

  m_model <- hc4_fit(M, list(X, CV),    c(X_VAR, COV_VAR))            # a
  y_model <- hc4_fit(Y, list(X, M, CV), c(X_VAR, M_VAR, COV_VAR))     # c', b
  t_model <- hc4_fit(Y, list(X, CV),    c(X_VAR, COV_VAR))            # c

  a_eff  <- unname(m_model$b[X_VAR])
  b_eff  <- unname(y_model$b[M_VAR])
  cp_eff <- unname(y_model$b[X_VAR])
  c_eff  <- unname(t_model$b[X_VAR])
  ab_eff <- a_eff * b_eff

  sd_y <- stats::sd(Y)
  sd_m <- stats::sd(M)

  # --- Bootstrap the indirect effect ---------------------------------------
  #
  # Percentile bootstrap, case resampling, N_BOOT replicates. Both the raw
  # indirect effect a*b and the partially standardised form a*b / SD(Y) are
  # resampled. PROCESS reports the *partially* standardised effect (not the
  # completely standardised one) because X is dichotomous.
  #
  # The seed is re-set here so each group gets the same random stream and can
  # be reproduced without running the other group first.

  set.seed(SEED)

  n_obs <- nrow(dat)
  Xm_m  <- cbind(1, X, CV)        # design for the M model
  Xm_y  <- cbind(1, X, M, CV)     # design for the Y model

  boot_ab <- numeric(N_BOOT)
  boot_ps <- numeric(N_BOOT)
  n_fail  <- 0L

  for (i in seq_len(N_BOOT)) {
    idx <- sample.int(n_obs, n_obs, replace = TRUE)
    fit <- try({
      a_i <- ols_coef(Xm_m[idx, , drop = FALSE], M[idx])[2]
      b_i <- ols_coef(Xm_y[idx, , drop = FALSE], Y[idx])[3]
      c(a_i * b_i, a_i * b_i / stats::sd(Y[idx]))
    }, silent = TRUE)

    if (inherits(fit, "try-error")) {
      n_fail <- n_fail + 1L
      boot_ab[i] <- NA_real_
      boot_ps[i] <- NA_real_
    } else {
      boot_ab[i] <- fit[1]
      boot_ps[i] <- fit[2]
    }
  }

  boot_ab <- boot_ab[!is.na(boot_ab)]
  boot_ps <- boot_ps[!is.na(boot_ps)]

  alpha <- (1 - CONF / 100) / 2
  probs <- c(alpha, 1 - alpha)

  ab_se <- stats::sd(boot_ab)
  ab_ci <- unname(stats::quantile(boot_ab, probs))

  ps_eff <- ab_eff / sd_y
  ps_se  <- stats::sd(boot_ps)
  ps_ci  <- unname(stats::quantile(boot_ps, probs))

  # partially standardised total and direct effects (point estimates only)
  c_ps  <- c_eff  / sd_y
  cp_ps <- cp_eff / sd_y

  list(
    label = label, n = n_obs,
    X = X, CV = CV, M = M, Y = Y,
    m_model = m_model, y_model = y_model, t_model = t_model,
    a = a_eff, b = b_eff, c = c_eff, cp = cp_eff, ab = ab_eff,
    ab_se = ab_se, ab_ci = ab_ci,
    ps = ps_eff, ps_se = ps_se, ps_ci = ps_ci,
    c_ps = c_ps, cp_ps = cp_ps,
    sd_y = sd_y, sd_m = sd_m,
    boot_ab = boot_ab, boot_ps = boot_ps, n_fail = n_fail
  )
}


# --- Formatting helpers ----------------------------------------------------

# ".036" rather than "0.036" - APA style for quantities bounded by 1.
no_zero <- function(x, digits = 3) {
  s <- formatC(x, format = "f", digits = digits)
  sub("^(-?)0\\.", "\\1.", s)
}

num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

ci <- function(lo, hi, digits = 3) {
  sprintf("[%s, %s]", num(lo, digits), num(hi, digits))
}

pfmt <- function(p) {
  if (is.na(p)) return("-")
  if (p < .001) "< .001" else no_zero(p, 3)
}

rule <- function(width = 78, ch = "-") cat(strrep(ch, width), "\n", sep = "")

# Prints a coefficient table in the house layout:
#   Term | b | SE | 95% CI | beta | t | p
coef_table <- function(fit, title) {
  cat(title, "\n", sep = "")
  rule()
  cat(sprintf("%-14s %9s %8s %22s %7s %9s %9s\n",
              "Term", "b", "SE", sprintf("%d%% CI", CONF), "beta", "t", "p"))
  rule()
  for (k in seq_along(fit$term)) {
    cat(sprintf("%-14s %9s %8s %22s %7s %9s %9s\n",
                fit$term[k],
                num(fit$b[k], 4),
                no_zero(fit$se[k], 4),
                ci(fit$lo[k], fit$hi[k], 4),
                if (is.na(fit$beta[k])) "-" else no_zero(fit$beta[k], 2),
                num(fit$t[k], 4),
                pfmt(fit$p[k])))
  }
  rule()
  cat(sprintf("R-squared = %s   F(%d, %d) = %s, p %s   [%s Wald]   n = %d\n\n",
              no_zero(fit$r2, 4), fit$df_mod, fit$df_res,
              num(fit$F, 4), pfmt(fit$Fp), HC_TYPE, fit$n))
}


# --- Per-group report ------------------------------------------------------

report_group <- function(r, split_value) {

  m_model <- r$m_model
  y_model <- r$y_model
  t_model <- r$t_model

  cat("\n")
  rule(78, "=")
  cat(sprintf("GROUP: %s   (%s = \"%s\")   n = %d\n",
              toupper(r$label), SPLIT_VAR, split_value, r$n))
  rule(78, "=")
  cat("\n")

  cat("Design check\n")
  cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n",
              X_VAR, sum(r$X == 0), sum(r$X == 1)))
  cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n",
              COV_VAR, sum(r$CV == 0), sum(r$CV == 1)))
  cat(sprintf("  %-10s M = %s, SD = %s\n",
              M_VAR, num(mean(r$M), 4), num(r$sd_m, 4)))
  cat(sprintf("  %-10s M = %s, SD = %s\n",
              Y_VAR, num(mean(r$Y), 4), num(r$sd_y, 4)))
  if (r$n_fail > 0L) {
    cat(sprintf("  %d bootstrap sample(s) failed and were discarded\n", r$n_fail))
  }
  cat("\n")

  cat("The paths\n")
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n",
              "Path", "Effect", "SE", sprintf("%d%% CI", CONF)))
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n",
              "a  - oddity -> unexpectedness",
              num(r$a, 4), no_zero(unname(m_model$se[X_VAR]), 4),
              ci(m_model$lo[X_VAR], m_model$hi[X_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n",
              "b  - unexpectedness -> discomfort",
              num(r$b, 4), no_zero(unname(y_model$se[M_VAR]), 4),
              ci(y_model$lo[M_VAR], y_model$hi[M_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n",
              "c  - total effect of oddity",
              num(r$c, 4), no_zero(unname(t_model$se[X_VAR]), 4),
              ci(t_model$lo[X_VAR], t_model$hi[X_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n",
              "c' - direct effect of oddity",
              num(r$cp, 4), no_zero(unname(y_model$se[X_VAR]), 4),
              ci(y_model$lo[X_VAR], y_model$hi[X_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n",
              "ab - indirect effect",
              num(r$ab, 4), no_zero(r$ab_se, 4), ci(r$ab_ci[1], r$ab_ci[2], 4)))
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n",
              "ab - indirect, partially standardised",
              num(r$ps, 4), no_zero(r$ps_se, 4), ci(r$ps_ci[1], r$ps_ci[2], 4)))
  rule()
  cat("SE for a, b, c and c' is the ", HC_TYPE,
      " standard error; SE for ab is the bootstrap SE.\n", sep = "")
  cat("CI for a, b, c and c' is the t-based ", CONF,
      "% interval; CI for ab is the percentile bootstrap interval.\n", sep = "")
  cat("\n")

  cat("Inference on the individual paths\n")
  rule()
  cat(sprintf("  a   t(%d) = %s, p %s\n",
              m_model$df_res, num(m_model$t[X_VAR], 4), pfmt(m_model$p[X_VAR])))
  cat(sprintf("  b   t(%d) = %s, p %s\n",
              y_model$df_res, num(y_model$t[M_VAR], 4), pfmt(y_model$p[M_VAR])))
  cat(sprintf("  c   t(%d) = %s, p %s\n",
              t_model$df_res, num(t_model$t[X_VAR], 4), pfmt(t_model$p[X_VAR])))
  cat(sprintf("  c'  t(%d) = %s, p %s\n",
              y_model$df_res, num(y_model$t[X_VAR], 4), pfmt(y_model$p[X_VAR])))
  cat("\n")

  cat("Partially standardised effects (divided by SD of ", Y_VAR, ")\n", sep = "")
  rule()
  cat(sprintf("  c_ps   = %s\n", num(r$c_ps, 4)))
  cat(sprintf("  c'_ps  = %s\n", num(r$cp_ps, 4)))
  cat(sprintf("  ab_ps  = %s   BootSE = %s   %d%% CI %s\n",
              num(r$ps, 4), no_zero(r$ps_se, 4), CONF,
              ci(r$ps_ci[1], r$ps_ci[2], 4)))
  cat("\n")

  cat("Component models\n")
  rule()
  cat("\n")
  coef_table(m_model, sprintf("Mediator model:      %s ~ %s + %s",
                              M_VAR, X_VAR, COV_VAR))
  coef_table(y_model, sprintf("Outcome model:       %s ~ %s + %s + %s",
                              Y_VAR, X_VAR, M_VAR, COV_VAR))
  coef_table(t_model, sprintf("Total effect model:  %s ~ %s + %s",
                              Y_VAR, X_VAR, COV_VAR))

  cat("Reading the result\n")
  rule()
  supported <- (r$ab_ci[1] > 0 && r$ab_ci[2] > 0) ||
               (r$ab_ci[1] < 0 && r$ab_ci[2] < 0)
  cat("  Within ", r$label, ", H3 is ",
      if (supported) "SUPPORTED" else "NOT SUPPORTED",
      " - the bootstrap CI for ab ",
      if (supported) "excludes" else "includes", " zero.\n", sep = "")

  inconsistent <- sign(r$cp) != sign(r$ab) && y_model$p[X_VAR] < .05
  if (inconsistent) {
    cat("  Direct and indirect paths carry opposite signs and the direct effect is\n")
    cat("  significant: this is INCONSISTENT MEDIATION (suppression). Do not use\n")
    cat("  partial/full mediation language, and do not compute a proportion\n")
    cat(sprintf("  mediated (ab / c = %s is uninterpretable under sign reversal).\n",
                num(r$ab / r$c, 4)))
  }
  cat(sprintf("  Oddity moves unexpectedness by %s points, or %s SD of %s.\n",
              num(r$a, 4), num(r$a / r$sd_m, 4), M_VAR))
  cat(sprintf("  The covariate %s: b = %s (p %s) on %s, b = %s (p %s) on %s.\n",
              COV_VAR,
              num(y_model$b[COV_VAR], 4), pfmt(y_model$p[COV_VAR]), Y_VAR,
              num(m_model$b[COV_VAR], 4), pfmt(m_model$p[COV_VAR]), M_VAR))
  cat("\n")

  invisible(NULL)
}


# --- Run both groups -------------------------------------------------------

cat("\n")
rule(78, "=")
cat("H3 - Unexpectedness mediates the effect of oddity on discomfort\n")
cat("Simple mediation (PROCESS Model 4 equivalent), split by ", SPLIT_VAR,
    "\n", sep = "")
rule(78, "=")
cat("\n")

cat("Data      ", path, "\n", sep = "")
cat(sprintf("Rows      %d read, %d complete, %d dropped for missing data\n",
            n_total, n_complete, n_dropped))
cat(sprintf("Model     Y = %s   X = %s   M = %s   covariate = %s\n",
            Y_VAR, X_VAR, M_VAR, COV_VAR))
cat(sprintf("Split     %s: %s\n", SPLIT_VAR,
            paste(vapply(GROUPS,
                         function(g) sprintf("\"%s\"", g$value),
                         character(1)),
                  collapse = ",  ")))
cat(sprintf("Inference %s standard errors; %d bootstrap samples; %d%% CI; seed %d per group\n",
            HC_TYPE, N_BOOT, CONF, SEED))
cat("\n")

cat("Group sizes\n")
for (g in GROUPS) {
  cat(sprintf("  %-14s %s = \"%s\"   n = %d\n", g$label, SPLIT_VAR, g$value,
              sum(dat_all[[SPLIT_VAR]] == g$value)))
}
cat("\n")

results <- list()
for (g in GROUPS) {
  sub <- dat_all[dat_all[[SPLIT_VAR]] == g$value, , drop = FALSE]
  if (nrow(sub) == 0L) {
    stop("No rows for ", SPLIT_VAR, " = ", g$value)
  }
  r <- run_h3(sub, g$label)
  report_group(r, g$value)
  results[[g$label]] <- r
}


# --- Side-by-side comparison -----------------------------------------------
#
# Descriptive only. The two groups were fitted independently, so a difference
# between the columns is NOT a test of moderated mediation - overlapping CIs
# and non-overlapping CIs are both uninformative about that question.

labels <- vapply(GROUPS, function(g) g$label, character(1))

rule(78, "=")
cat("SIDE BY SIDE\n")
rule(78, "=")
cat("\n")

cat(sprintf("%-16s %34s %34s\n", "", labels[1], labels[2]))
cat(sprintf("%-16s %34s %34s\n", "Path",
            sprintf("n = %d", results[[labels[1]]]$n),
            sprintf("n = %d", results[[labels[2]]]$n)))
rule()

cmp_row <- function(name, getter) {
  cells <- vapply(labels, function(l) getter(results[[l]]), character(1))
  cat(sprintf("%-16s %34s %34s\n", name, cells[1], cells[2]))
}

eff_ci <- function(est, lo, hi) {
  sprintf("%s  %s", num(est, 4), ci(lo, hi, 4))
}

cmp_row("a",       function(r) eff_ci(r$a,  r$m_model$lo[X_VAR], r$m_model$hi[X_VAR]))
cmp_row("b",       function(r) eff_ci(r$b,  r$y_model$lo[M_VAR], r$y_model$hi[M_VAR]))
cmp_row("c total", function(r) eff_ci(r$c,  r$t_model$lo[X_VAR], r$t_model$hi[X_VAR]))
cmp_row("c' direct", function(r) eff_ci(r$cp, r$y_model$lo[X_VAR], r$y_model$hi[X_VAR]))
cmp_row("ab indirect", function(r) eff_ci(r$ab, r$ab_ci[1], r$ab_ci[2]))
cmp_row("ab_ps",   function(r) eff_ci(r$ps, r$ps_ci[1], r$ps_ci[2]))
rule()
cmp_row("M model R2", function(r) no_zero(r$m_model$r2, 4))
cmp_row("Y model R2", function(r) no_zero(r$y_model$r2, 4))
cmp_row("Total R2",   function(r) no_zero(r$t_model$r2, 4))
rule()
cat("Groups were fitted independently. Differences between the columns are\n")
cat("descriptive; this script does not test whether ", SPLIT_VAR,
    " moderates any path.\n", sep = "")
cat("\n")


# --- Machine-readable version (handy for pasting into the write-up) --------

paths_of <- function(r) {
  data.frame(
    group  = r$label,
    n      = r$n,
    path   = c("a", "b", "c", "c_prime", "ab", "ab_partially_std"),
    label  = c("oddity -> unexpectedness",
               "unexpectedness -> discomfort",
               "total effect of oddity",
               "direct effect of oddity",
               "indirect effect",
               "indirect effect, partially standardised"),
    effect = c(r$a, r$b, r$c, r$cp, r$ab, r$ps),
    se     = c(unname(r$m_model$se[X_VAR]), unname(r$y_model$se[M_VAR]),
               unname(r$t_model$se[X_VAR]), unname(r$y_model$se[X_VAR]),
               r$ab_se, r$ps_se),
    ci_lo  = c(unname(r$m_model$lo[X_VAR]), unname(r$y_model$lo[M_VAR]),
               unname(r$t_model$lo[X_VAR]), unname(r$y_model$lo[X_VAR]),
               r$ab_ci[1], r$ps_ci[1]),
    ci_hi  = c(unname(r$m_model$hi[X_VAR]), unname(r$y_model$hi[M_VAR]),
               unname(r$t_model$hi[X_VAR]), unname(r$y_model$hi[X_VAR]),
               r$ab_ci[2], r$ps_ci[2]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

paths_df <- do.call(rbind, lapply(labels, function(l) paths_of(results[[l]])))
rownames(paths_df) <- NULL

print(paths_df, digits = 5)
cat("\n")

invisible(list(paths = paths_df, groups = results, seed = SEED))
