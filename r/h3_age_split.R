#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# h3_age_split.R
#
# Hypothesis 3 - "Unexpectedness mediates" - split by age decade.
#
#   Oddity raises perceived unexpectedness, and unexpectedness carries the
#   effect of oddity on comfort.
#
# This is h3_full_set.R run once per decade of `age`. Unlike the other split
# scripts the grouping variable is not pre-coded in the data, so the bins are
# cut here from AGE_BREAKS below: (10,20] -> "11-20", (20,30] -> "21-30", and
# so on to "61-70". Observed ages run 18 to 69.
#
# Each group is analysed completely independently - separate models, separate
# HC4 standard errors, separate bootstrap. This is a stratified re-run, NOT a
# moderated mediation: no cross-group test is performed, so any difference
# between the rows is descriptive only.
#
# ===========================================================================
# READ THIS BEFORE USING THE OUTPUT
# ===========================================================================
#
# Every rating is one trial, and each participant contributed 32 trials. The
# row counts below are therefore NOT sample sizes in the usual sense - the
# "11-20" decade is 64 rows, but those 64 rows are TWO PEOPLE.
#
# This matters more here than in any other split in this project, because
# age is the only variable that carves the sample into groups small enough
# for the distinction to change the conclusion:
#
#     decade    rows   participants
#     11-20       64        2          <- cannot support inference
#     21-30      576       18
#     31-40      800       25
#     41-50      608       19
#     51-60      384       12
#     61-70      256        8          <- treat with great caution
#
# Two consequences:
#
#  1. The standard row bootstrap (what PROCESS does, and what every other
#     script in this project does) resamples ROWS, which assumes the 64 rows
#     in "11-20" are 64 independent observations. They are not. Its interval
#     for that decade is meaningless - far too narrow for what it claims to
#     measure, because it is really measuring within-person consistency.
#
#  2. So this script ALSO runs a CLUSTER bootstrap, resampling PARTICIPANTS
#     and taking all 32 of their rows together. That respects the nesting.
#     Both intervals are printed side by side. Where they disagree, the
#     cluster interval is the honest one.
#
#     But the cluster bootstrap does not rescue a tiny group either. With K
#     participants there are only choose(2K-1, K) distinct resamples: for
#     K = 2 that is THREE. The "11-20" cluster interval is an artifact of
#     three possible draws, not an estimate. The script prints this count so
#     the degeneracy is visible rather than implied.
#
# The clustering caveat applies to every analysis in this project, including
# the pooled recreate3.md run - 2,688 rows are 84 participants. It is simply
# invisible at full-sample size and unmissable at decade size.
#
# If you want the decades without the degenerate bin, merge the first two by
# setting:  AGE_BREAKS <- c(10, 30, 40, 50, 60, 70)   ->  "11-30", "31-40", ...
# No other change is needed; labels and groups are derived from the breaks.
#
# ===========================================================================
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
# Usage:
#   Rscript source/h3_age_split.R           # run from the project root
#   source("source/h3_age_split.R")         # or from within RStudio
#
# Base R only. Reproducible: the seed is fixed and re-set per group per
# bootstrap type. Runs six groups with two bootstraps each, so expect this
# one to take a few minutes.
# ---------------------------------------------------------------------------


# --- Configuration ---------------------------------------------------------

SEED    <- 20260727    # matches /seed= in the SPSS syntax
N_BOOT  <- 10000       # matches /boot=
CONF    <- 95          # matches /conf=
HC_TYPE <- "HC4"       # matches /hc=4

Y_VAR   <- "c_discom"  # outcome    - discomfort (1 .. 4)
X_VAR   <- "c_oddity"  # antecedent - oddity      (0 = absent, 1 = present)
M_VAR   <- "c_expect"  # mediator   - unexpectedness (1 .. 5)
COV_VAR <- "c_noise"   # covariate  - background activity (0 = low, 1 = high)

# The split. Bins are half-open on the left: (10,20] is labelled "11-20".
AGE_VAR    <- "age"
AGE_BREAKS <- c(10, 20, 30, 40, 50, 60, 70)

# Cluster identifier - one row per trial, 32 trials per participant.
ID_VAR <- "participant_index"

# Run the cluster bootstrap alongside the row bootstrap. Turn off only if you
# want output directly comparable with the other split scripts.
CLUSTER_BOOT <- TRUE

# Reporting thresholds, in PARTICIPANTS (not rows).
MIN_PARTICIPANTS  <- 20   # below this, results are flagged as imprecise
TINY_PARTICIPANTS <- 5    # below this, results are flagged uninterpretable


# --- Locate the data file --------------------------------------------------

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

needed <- c(Y_VAR, X_VAR, M_VAR, COV_VAR, AGE_VAR)
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0L) {
  stop("Missing column(s) in ", path, ": ", paste(missing_cols, collapse = ", "))
}

# The cluster id is optional - degrade gracefully rather than refuse to run.
have_id <- ID_VAR %in% names(raw)
if (!have_id) {
  warning("Column '", ID_VAR, "' not found - participant counts and the ",
          "cluster bootstrap are unavailable. Row counts will be reported ",
          "as if independent, which OVERSTATES precision.")
  CLUSTER_BOOT <- FALSE
} else {
  needed <- c(needed, ID_VAR)
}

dat_all <- raw[needed]
dat_all <- dat_all[stats::complete.cases(dat_all), , drop = FALSE]


# --- Cut the age bins ------------------------------------------------------
#
# Labels are derived from the breaks so that changing AGE_BREAKS changes
# everything downstream: "(10,20]" is reported as "11-20".

n_bins <- length(AGE_BREAKS) - 1L
bin_labels <- sprintf("%d-%d",
                      AGE_BREAKS[-length(AGE_BREAKS)] + 1L,
                      AGE_BREAKS[-1L])

age_vals <- as.numeric(dat_all[[AGE_VAR]])
dat_all$.bin <- cut(age_vals, breaks = AGE_BREAKS,
                    labels = bin_labels, right = TRUE)

n_total     <- nrow(raw)
n_unbinned  <- sum(is.na(dat_all$.bin))
if (n_unbinned > 0L) {
  out_of_range <- age_vals[is.na(dat_all$.bin)]
  warning(n_unbinned, " row(s) fall outside AGE_BREAKS [",
          min(AGE_BREAKS), ", ", max(AGE_BREAKS), "] - observed ",
          min(out_of_range), " to ", max(out_of_range),
          ". They are excluded from every group.")
  dat_all <- dat_all[!is.na(dat_all$.bin), , drop = FALSE]
}

n_complete <- nrow(dat_all)
n_dropped  <- n_total - n_complete

# Drop empty bins rather than crash on them.
bin_counts <- table(dat_all$.bin)
empty_bins <- names(bin_counts)[bin_counts == 0L]
if (length(empty_bins) > 0L) {
  warning("Empty age bin(s) skipped: ", paste(empty_bins, collapse = ", "))
}
labels   <- bin_labels[bin_counts > 0L]
n_groups <- length(labels)
if (n_groups == 0L) stop("No non-empty age bins - check AGE_BREAKS.")


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
# This is what PROCESS emits under /hc=4. Note it corrects for
# heteroscedasticity, NOT for clustering - see the header. A rating from a
# participant who has already given 31 others is not an independent
# observation, and no HC variant fixes that.

hc4_fit <- function(y, preds, labels) {
  n <- length(y)
  Xm <- cbind(Constant = 1, do.call(cbind, preds))
  colnames(Xm) <- c("Constant", labels)
  p <- ncol(Xm)

  XtXi <- solve(crossprod(Xm))
  b    <- as.vector(XtXi %*% crossprod(Xm, y))
  e    <- as.vector(y - Xm %*% b)

  h     <- rowSums((Xm %*% XtXi) * Xm)
  delta <- pmin(4, h / (p / n))
  w     <- e^2 / (1 - h)^delta

  V  <- XtXi %*% crossprod(Xm * sqrt(w)) %*% XtXi
  se <- sqrt(diag(V))

  df_res <- n - p
  tval   <- b / se
  pval   <- 2 * stats::pt(-abs(tval), df_res)
  tcrit  <- stats::qt(1 - (1 - CONF / 100) / 2, df_res)

  r2 <- 1 - sum(e^2) / sum((y - mean(y))^2)

  slopes  <- b[-1]
  V_slope <- V[-1, -1, drop = FALSE]
  df_mod  <- p - 1
  Fstat   <- as.numeric(t(slopes) %*% solve(V_slope) %*% slopes) / df_mod
  Fp      <- stats::pf(Fstat, df_mod, df_res, lower.tail = FALSE)

  sd_x  <- apply(Xm, 2, stats::sd)
  beta  <- b * sd_x / stats::sd(y)
  beta[1] <- NA_real_

  names(b) <- names(se) <- names(tval) <- names(pval) <- colnames(Xm)

  list(
    term = colnames(Xm), b = b, se = se, t = tval, p = pval, beta = beta,
    lo = b - tcrit * se, hi = b + tcrit * se,
    r2 = r2, F = Fstat, df_mod = df_mod, df_res = df_res, Fp = Fp, n = n
  )
}

ols_coef <- function(Xm, y) {
  as.vector(solve(crossprod(Xm), crossprod(Xm, y)))
}

# Number of distinct multisets of size K drawn from K clusters. Small values
# mean the cluster bootstrap has almost nothing to resample.
n_distinct_resamples <- function(K) choose(2 * K - 1, K)


# --- One complete H3 analysis over one subset ------------------------------

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

  n_obs <- nrow(dat)
  Xm_m  <- cbind(1, X, CV)
  Xm_y  <- cbind(1, X, M, CV)

  # Row groupings for the cluster bootstrap.
  if (have_id) {
    ids      <- dat[[ID_VAR]]
    rows_by  <- split(seq_len(n_obs), ids)
    n_people <- length(rows_by)
  } else {
    rows_by  <- NULL
    n_people <- NA_integer_
  }

  # One bootstrap pass. `mode` is "row" (resample trials) or "cluster"
  # (resample participants, taking all their trials).
  boot_pass <- function(mode) {
    set.seed(SEED)
    ab <- numeric(N_BOOT)
    ps <- numeric(N_BOOT)
    fails <- 0L
    K <- if (mode == "cluster") length(rows_by) else NA_integer_

    for (i in seq_len(N_BOOT)) {
      idx <- if (mode == "cluster") {
        unlist(rows_by[sample.int(K, K, replace = TRUE)], use.names = FALSE)
      } else {
        sample.int(n_obs, n_obs, replace = TRUE)
      }

      fit <- try({
        a_i <- ols_coef(Xm_m[idx, , drop = FALSE], M[idx])[2]
        b_i <- ols_coef(Xm_y[idx, , drop = FALSE], Y[idx])[3]
        c(a_i * b_i, a_i * b_i / stats::sd(Y[idx]))
      }, silent = TRUE)

      if (inherits(fit, "try-error")) {
        fails <- fails + 1L
        ab[i] <- NA_real_; ps[i] <- NA_real_
      } else {
        ab[i] <- fit[1]; ps[i] <- fit[2]
      }
    }

    ab <- ab[!is.na(ab)]; ps <- ps[!is.na(ps)]
    alpha <- (1 - CONF / 100) / 2
    probs <- c(alpha, 1 - alpha)
    list(
      ab_se = stats::sd(ab), ab_ci = unname(stats::quantile(ab, probs)),
      ps_se = stats::sd(ps), ps_ci = unname(stats::quantile(ps, probs)),
      n_ok = length(ab), n_fail = fails
    )
  }

  boot_row <- boot_pass("row")
  boot_clu <- if (CLUSTER_BOOT && have_id) boot_pass("cluster") else NULL

  list(
    label = label, n = n_obs, n_people = n_people,
    X = X, CV = CV, M = M, Y = Y,
    m_model = m_model, y_model = y_model, t_model = t_model,
    a = a_eff, b = b_eff, c = c_eff, cp = cp_eff, ab = ab_eff,
    ps = ab_eff / sd_y, c_ps = c_eff / sd_y, cp_ps = cp_eff / sd_y,
    sd_y = sd_y, sd_m = sd_m,
    row = boot_row, clu = boot_clu
  )
}


# --- Formatting helpers ----------------------------------------------------

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

excl_zero <- function(v) (v[1] > 0 && v[2] > 0) || (v[1] < 0 && v[2] < 0)

# One-word reliability tag driven by participant count, not row count.
tier <- function(n_people) {
  if (is.na(n_people)) return("UNKNOWN")
  if (n_people < TINY_PARTICIPANTS) return("UNUSABLE")
  if (n_people < MIN_PARTICIPANTS)  return("WEAK")
  "OK"
}

coef_table <- function(fit, title) {
  cat(title, "\n", sep = "")
  rule()
  cat(sprintf("%-14s %9s %8s %22s %7s %9s %9s\n",
              "Term", "b", "SE", sprintf("%d%% CI", CONF), "beta", "t", "p"))
  rule()
  for (k in seq_along(fit$term)) {
    cat(sprintf("%-14s %9s %8s %22s %7s %9s %9s\n",
                fit$term[k], num(fit$b[k], 4), no_zero(fit$se[k], 4),
                ci(fit$lo[k], fit$hi[k], 4),
                if (is.na(fit$beta[k])) "-" else no_zero(fit$beta[k], 2),
                num(fit$t[k], 4), pfmt(fit$p[k])))
  }
  rule()
  cat(sprintf("R-squared = %s   F(%d, %d) = %s, p %s   [%s Wald]   n = %d\n\n",
              no_zero(fit$r2, 4), fit$df_mod, fit$df_res,
              num(fit$F, 4), pfmt(fit$Fp), HC_TYPE, fit$n))
}


# --- Per-group report ------------------------------------------------------

report_group <- function(r) {

  m_model <- r$m_model; y_model <- r$y_model; t_model <- r$t_model
  tg <- tier(r$n_people)

  cat("\n")
  rule(78, "=")
  cat(sprintf("AGE %s   n = %d rows", r$label, r$n))
  if (!is.na(r$n_people)) cat(sprintf(" from %d participants", r$n_people))
  cat(sprintf("   [%s]\n", tg))
  rule(78, "=")
  cat("\n")

  if (tg == "UNUSABLE") {
    cat("  *** ", r$n_people, " participants. Nothing below supports inference. ***\n",
        sep = "")
    cat("  The model is reported for completeness only. Row-bootstrap intervals\n")
    cat("  here describe within-person consistency, not sampling error, and the\n")
    cat("  cluster bootstrap has too few clusters to resample meaningfully.\n\n")
  } else if (tg == "WEAK") {
    cat(sprintf("  NOTE: %d participants (< %d). Intervals are wide and the cluster\n",
                r$n_people, MIN_PARTICIPANTS))
    cat("  bootstrap is itself imprecise below roughly 30 clusters. Read the\n")
    cat("  point estimates as indicative only.\n\n")
  }

  cat("Design check\n")
  cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n",
              X_VAR, sum(r$X == 0), sum(r$X == 1)))
  cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n",
              COV_VAR, sum(r$CV == 0), sum(r$CV == 1)))
  cat(sprintf("  %-10s M = %s, SD = %s\n",
              M_VAR, num(mean(r$M), 4), num(r$sd_m, 4)))
  cat(sprintf("  %-10s M = %s, SD = %s\n",
              Y_VAR, num(mean(r$Y), 4), num(r$sd_y, 4)))
  if (!is.na(r$n_people)) {
    nd <- n_distinct_resamples(r$n_people)
    cat(sprintf("  clusters   %d participants; %s distinct cluster resamples\n",
                r$n_people,
                if (is.finite(nd) && nd < 1e7) format(nd, big.mark = ",")
                else "many"))
  }
  cat("\n")

  cat("The paths\n")
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n",
              "Path", "Effect", "SE", sprintf("%d%% CI", CONF)))
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n", "a  - oddity -> unexpectedness",
              num(r$a, 4), no_zero(unname(m_model$se[X_VAR]), 4),
              ci(m_model$lo[X_VAR], m_model$hi[X_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n", "b  - unexpectedness -> discomfort",
              num(r$b, 4), no_zero(unname(y_model$se[M_VAR]), 4),
              ci(y_model$lo[M_VAR], y_model$hi[M_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n", "c  - total effect of oddity",
              num(r$c, 4), no_zero(unname(t_model$se[X_VAR]), 4),
              ci(t_model$lo[X_VAR], t_model$hi[X_VAR], 4)))
  cat(sprintf("%-42s %9s %8s %22s\n", "c' - direct effect of oddity",
              num(r$cp, 4), no_zero(unname(y_model$se[X_VAR]), 4),
              ci(y_model$lo[X_VAR], y_model$hi[X_VAR], 4)))
  rule()
  cat(sprintf("%-42s %9s %8s %22s\n", "ab - indirect  (row bootstrap)",
              num(r$ab, 4), no_zero(r$row$ab_se, 4),
              ci(r$row$ab_ci[1], r$row$ab_ci[2], 4)))
  if (!is.null(r$clu)) {
    cat(sprintf("%-42s %9s %8s %22s\n", "ab - indirect  (CLUSTER bootstrap)",
                num(r$ab, 4), no_zero(r$clu$ab_se, 4),
                ci(r$clu$ab_ci[1], r$clu$ab_ci[2], 4)))
  }
  cat(sprintf("%-42s %9s %8s %22s\n", "ab - partially standardised (row)",
              num(r$ps, 4), no_zero(r$row$ps_se, 4),
              ci(r$row$ps_ci[1], r$row$ps_ci[2], 4)))
  rule()
  cat("SE for a, b, c and c' is the ", HC_TYPE,
      " standard error, which corrects for\n", sep = "")
  cat("heteroscedasticity but NOT for the 32-trials-per-participant nesting.\n")
  if (!is.null(r$clu)) {
    wr <- r$row$ab_ci[2] - r$row$ab_ci[1]
    wc <- r$clu$ab_ci[2] - r$clu$ab_ci[1]
    cat(sprintf("Cluster interval is %s the width of the row interval (%s vs %s).\n",
                paste0(num(wc / wr, 2), "x"), num(wc, 4), num(wr, 4)))
    if (wc < wr && !is.na(r$n_people) && r$n_people < TINY_PARTICIPANTS) {
      cat("Narrower is NOT better here - with this few clusters the cluster\n")
      cat("bootstrap is degenerate, so its quantiles mean very little.\n")
    }
  }
  cat("\n")

  cat("Inference on the individual paths\n")
  rule()
  cat(sprintf("  a   t(%d) = %s, p %s\n", m_model$df_res,
              num(m_model$t[X_VAR], 4), pfmt(m_model$p[X_VAR])))
  cat(sprintf("  b   t(%d) = %s, p %s\n", y_model$df_res,
              num(y_model$t[M_VAR], 4), pfmt(y_model$p[M_VAR])))
  cat(sprintf("  c   t(%d) = %s, p %s\n", t_model$df_res,
              num(t_model$t[X_VAR], 4), pfmt(t_model$p[X_VAR])))
  cat(sprintf("  c'  t(%d) = %s, p %s\n", y_model$df_res,
              num(y_model$t[X_VAR], 4), pfmt(y_model$p[X_VAR])))
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
  use_ci <- if (!is.null(r$clu)) r$clu$ab_ci else r$row$ab_ci
  which_ci <- if (!is.null(r$clu)) "cluster" else "row"
  supported <- excl_zero(use_ci)
  if (tg == "UNUSABLE") {
    cat("  No verdict - too few participants.\n")
  } else {
    cat("  Within age ", r$label, ", H3 is ",
        if (supported) "SUPPORTED" else "NOT SUPPORTED",
        " on the ", which_ci, " bootstrap.\n", sep = "")
  }
  inconsistent <- sign(r$cp) != sign(r$ab) && y_model$p[X_VAR] < .05
  if (inconsistent) {
    cat("  Direct and indirect paths carry opposite signs and the direct effect\n")
    cat("  is significant: INCONSISTENT MEDIATION. Do not use partial/full\n")
    cat("  mediation language and do not compute a proportion mediated.\n")
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


# --- Run every group -------------------------------------------------------

cat("\n")
rule(78, "=")
cat("H3 - Unexpectedness mediates the effect of oddity on discomfort\n")
cat("Simple mediation (PROCESS Model 4 equivalent), split by age decade\n")
rule(78, "=")
cat("\n")

cat("Data      ", path, "\n", sep = "")
cat(sprintf("Rows      %d read, %d used, %d dropped\n",
            n_total, n_complete, n_dropped))
cat(sprintf("Model     Y = %s   X = %s   M = %s   covariate = %s\n",
            Y_VAR, X_VAR, M_VAR, COV_VAR))
cat(sprintf("Bins      %s from breaks %s\n",
            paste(labels, collapse = ", "),
            paste(AGE_BREAKS, collapse = "/")))
cat(sprintf("Age       observed %g to %g\n", min(age_vals), max(age_vals)))
cat(sprintf("Bootstrap %d samples, %d%% CI, seed %d per group%s\n",
            N_BOOT, CONF, SEED,
            if (CLUSTER_BOOT) "; row AND cluster" else "; row only"))
cat(sprintf("Inference %s standard errors (heteroscedasticity only)\n", HC_TYPE))
cat("\n")

cat("Group sizes - ROWS ARE TRIALS, NOT PEOPLE\n")
rule()
cat(sprintf("  %-10s %8s %14s %10s\n", "Decade", "rows", "participants", "status"))
rule()
group_rows <- integer(n_groups)
group_ppl  <- rep(NA_integer_, n_groups)
for (k in seq_len(n_groups)) {
  sub <- dat_all[dat_all$.bin == labels[k], , drop = FALSE]
  group_rows[k] <- nrow(sub)
  if (have_id) group_ppl[k] <- length(unique(sub[[ID_VAR]]))
  cat(sprintf("  %-10s %8d %14s %10s\n", labels[k], group_rows[k],
              if (is.na(group_ppl[k])) "?" else as.character(group_ppl[k]),
              tier(group_ppl[k])))
}
rule()
if (have_id && any(group_ppl < MIN_PARTICIPANTS, na.rm = TRUE)) {
  cat("  Each participant contributed 32 trials, so a decade with a large row\n")
  cat("  count can still rest on very few people. See the header.\n")
}
cat("\n")

results <- list()
for (lab in labels) {
  sub <- dat_all[dat_all$.bin == lab, , drop = FALSE]
  r <- run_h3(sub, lab)
  report_group(r)
  results[[lab]] <- r
}


# --- Summary tables ---------------------------------------------------------
#
# Six groups is too many for a column-per-group layout, so these are
# transposed: one row per decade.

rule(86, "=")
cat("SUMMARY 1 - POINT ESTIMATES BY DECADE\n")
rule(86, "=")
cat("\n")
cat(sprintf("%-8s %6s %5s %9s %9s %9s %9s %9s %8s\n",
            "Decade", "rows", "ppl", "a", "b", "c", "c'", "ab", "status"))
rule(86)
for (lab in labels) {
  r <- results[[lab]]
  cat(sprintf("%-8s %6d %5s %9s %9s %9s %9s %9s %8s\n",
              lab, r$n,
              if (is.na(r$n_people)) "?" else as.character(r$n_people),
              num(r$a, 3), num(r$b, 3), num(r$c, 3), num(r$cp, 3),
              num(r$ab, 3), tier(r$n_people)))
}
rule(86)
cat("\n")

rule(86, "=")
cat("SUMMARY 2 - INDIRECT EFFECT, ROW vs CLUSTER BOOTSTRAP\n")
rule(86, "=")
cat("\n")
cat(sprintf("%-8s %8s %20s %7s %20s %7s\n",
            "Decade", "ab", "row-boot CI", "width", "cluster CI", "width"))
rule(86)
for (lab in labels) {
  r <- results[[lab]]
  rw <- r$row$ab_ci[2] - r$row$ab_ci[1]
  if (is.null(r$clu)) {
    cat(sprintf("%-8s %8s %20s %7s %20s %7s\n", lab, num(r$ab, 3),
                ci(r$row$ab_ci[1], r$row$ab_ci[2], 3), num(rw, 3), "-", "-"))
  } else {
    cw <- r$clu$ab_ci[2] - r$clu$ab_ci[1]
    cat(sprintf("%-8s %8s %20s %7s %20s %7s\n", lab, num(r$ab, 3),
                ci(r$row$ab_ci[1], r$row$ab_ci[2], 3), num(rw, 3),
                ci(r$clu$ab_ci[1], r$clu$ab_ci[2], 3), num(cw, 3)))
  }
}
rule(86)
cat("Where the two intervals disagree, prefer the cluster interval - unless\n")
cat("the decade is flagged UNUSABLE, in which case prefer neither.\n")
cat("Groups were fitted independently; nothing here tests whether age\n")
cat("moderates any path. That needs PROCESS Model 59 with age as a\n")
cat("continuous moderator, which also avoids throwing away information by\n")
cat("binning in the first place.\n")
cat("\n")


# --- Machine-readable version ----------------------------------------------

paths_of <- function(r) {
  data.frame(
    decade = r$label, n_rows = r$n, n_people = r$n_people,
    status = tier(r$n_people),
    path   = c("a", "b", "c", "c_prime", "ab", "ab_partially_std"),
    effect = c(r$a, r$b, r$c, r$cp, r$ab, r$ps),
    se     = c(unname(r$m_model$se[X_VAR]), unname(r$y_model$se[M_VAR]),
               unname(r$t_model$se[X_VAR]), unname(r$y_model$se[X_VAR]),
               r$row$ab_se, r$row$ps_se),
    ci_lo  = c(unname(r$m_model$lo[X_VAR]), unname(r$y_model$lo[M_VAR]),
               unname(r$t_model$lo[X_VAR]), unname(r$y_model$lo[X_VAR]),
               r$row$ab_ci[1], r$row$ps_ci[1]),
    ci_hi  = c(unname(r$m_model$hi[X_VAR]), unname(r$y_model$hi[M_VAR]),
               unname(r$t_model$hi[X_VAR]), unname(r$y_model$hi[X_VAR]),
               r$row$ab_ci[2], r$row$ps_ci[2]),
    clu_lo = c(NA, NA, NA, NA,
               if (is.null(r$clu)) NA else r$clu$ab_ci[1],
               if (is.null(r$clu)) NA else r$clu$ps_ci[1]),
    clu_hi = c(NA, NA, NA, NA,
               if (is.null(r$clu)) NA else r$clu$ab_ci[2],
               if (is.null(r$clu)) NA else r$clu$ps_ci[2]),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

paths_df <- do.call(rbind, lapply(labels, function(l) paths_of(results[[l]])))
rownames(paths_df) <- NULL

print(paths_df, digits = 5)
cat("\n")

invisible(list(paths = paths_df, groups = results, seed = SEED,
               breaks = AGE_BREAKS))
