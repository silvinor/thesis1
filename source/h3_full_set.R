#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# h3_full_set.R
#
# Hypothesis 3 - "Unexpectedness mediates".
#
#   Oddity raises perceived unexpectedness, and unexpectedness carries the
#   effect of oddity on comfort.
#
# This is a base-R reimplementation of the PROCESS v4.2 (Hayes, 2022) Model 4
# run documented in recreate3.md, which was originally produced in
# IBM SPSS Statistics 31.0.1.0:
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
#   Rscript source/h3_full_set.R            # run from the project root
#   source("source/h3_full_set.R")          # or from within RStudio
#
# Base R only - no external packages required (no `sandwich`, no `boot`,
# no `lavaan`). Results are reproducible: the RNG seed is fixed below.
#
# NOTE: the bootstrap CI will differ in the third decimal place from the SPSS
# output, because PROCESS and R draw their resamples from different RNGs.
# Every OLS quantity (a, b, c, c', SEs, t, p, R-squared, F) is exact.
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

needed <- c(Y_VAR, X_VAR, M_VAR, COV_VAR)
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0L) {
  stop("Missing column(s) in ", path, ": ", paste(missing_cols, collapse = ", "))
}

dat <- raw[needed]
dat <- dat[stats::complete.cases(dat), , drop = FALSE]

n_total   <- nrow(raw)
n_used    <- nrow(dat)
n_dropped <- n_total - n_used

Y <- as.numeric(dat[[Y_VAR]])
X <- as.numeric(dat[[X_VAR]])
M <- as.numeric(dat[[M_VAR]])
CV <- as.numeric(dat[[COV_VAR]])


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


# --- Fit the three models --------------------------------------------------

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


# --- Bootstrap the indirect effect -----------------------------------------
#
# Percentile bootstrap, case resampling, N_BOOT replicates. Both the raw
# indirect effect a*b and the partially standardised form a*b / SD(Y) are
# resampled. PROCESS reports the *partially* standardised effect (not the
# completely standardised one) because X is dichotomous.

set.seed(SEED)

n_obs  <- n_used
Xm_m   <- cbind(1, X, CV)        # design for the M model
Xm_y   <- cbind(1, X, M, CV)     # design for the Y model

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

alpha  <- (1 - CONF / 100) / 2
probs  <- c(alpha, 1 - alpha)

ab_se <- stats::sd(boot_ab)
ab_ci <- unname(stats::quantile(boot_ab, probs))

ps_eff <- ab_eff / sd_y
ps_se  <- stats::sd(boot_ps)
ps_ci  <- unname(stats::quantile(boot_ps, probs))

# partially standardised total and direct effects (point estimates only)
c_ps  <- c_eff  / sd_y
cp_ps <- cp_eff / sd_y


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


# --- Report ----------------------------------------------------------------

cat("\n")
rule(78, "=")
cat("H3 - Unexpectedness mediates the effect of oddity on discomfort\n")
cat("Simple mediation (PROCESS Model 4 equivalent), base R\n")
rule(78, "=")
cat("\n")

cat("Data      ", path, "\n", sep = "")
cat(sprintf("Rows      %d read, %d used, %d dropped for missing data\n",
            n_total, n_used, n_dropped))
cat(sprintf("Model     Y = %s   X = %s   M = %s   covariate = %s\n",
            Y_VAR, X_VAR, M_VAR, COV_VAR))
cat(sprintf("Inference %s standard errors; %d bootstrap samples; %d%% CI; seed %d\n",
            HC_TYPE, length(boot_ab), CONF, SEED))
if (n_fail > 0L) {
  cat(sprintf("          %d bootstrap sample(s) failed and were discarded\n", n_fail))
}
cat("\n")

cat("Design check\n")
cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n", X_VAR, sum(X == 0), sum(X == 1)))
cat(sprintf("  %-10s n(0) = %4d   n(1) = %4d\n", COV_VAR, sum(CV == 0), sum(CV == 1)))
cat(sprintf("  %-10s M = %s, SD = %s\n", M_VAR, num(mean(M), 4), num(sd_m, 4)))
cat(sprintf("  %-10s M = %s, SD = %s\n", Y_VAR, num(mean(Y), 4), num(sd_y, 4)))
cat("\n")

rule(78, "=")
cat("THE PATHS\n")
rule(78, "=")
cat("\n")

cat(sprintf("%-42s %9s %8s %22s\n",
            "Path", "Effect", "SE", sprintf("%d%% CI", CONF)))
rule()
cat(sprintf("%-42s %9s %8s %22s\n",
            sprintf("a  - %s -> %s", "oddity", "unexpectedness"),
            num(a_eff, 4), no_zero(unname(m_model$se[X_VAR]), 4),
            ci(m_model$lo[X_VAR], m_model$hi[X_VAR], 4)))
cat(sprintf("%-42s %9s %8s %22s\n",
            sprintf("b  - %s -> %s", "unexpectedness", "discomfort"),
            num(b_eff, 4), no_zero(unname(y_model$se[M_VAR]), 4),
            ci(y_model$lo[M_VAR], y_model$hi[M_VAR], 4)))
cat(sprintf("%-42s %9s %8s %22s\n",
            "c  - total effect of oddity",
            num(c_eff, 4), no_zero(unname(t_model$se[X_VAR]), 4),
            ci(t_model$lo[X_VAR], t_model$hi[X_VAR], 4)))
cat(sprintf("%-42s %9s %8s %22s\n",
            "c' - direct effect of oddity",
            num(cp_eff, 4), no_zero(unname(y_model$se[X_VAR]), 4),
            ci(y_model$lo[X_VAR], y_model$hi[X_VAR], 4)))
cat(sprintf("%-42s %9s %8s %22s\n",
            "ab - indirect effect",
            num(ab_eff, 4), no_zero(ab_se, 4), ci(ab_ci[1], ab_ci[2], 4)))
rule()
cat(sprintf("%-42s %9s %8s %22s\n",
            "ab - indirect, partially standardised",
            num(ps_eff, 4), no_zero(ps_se, 4), ci(ps_ci[1], ps_ci[2], 4)))
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
cat(sprintf("  c_ps   = %s\n",  num(c_ps, 4)))
cat(sprintf("  c'_ps  = %s\n",  num(cp_ps, 4)))
cat(sprintf("  ab_ps  = %s   BootSE = %s   %d%% CI %s\n",
            num(ps_eff, 4), no_zero(ps_se, 4), CONF, ci(ps_ci[1], ps_ci[2], 4)))
cat("\n")
cat("  X is dichotomous, so the partially standardised indirect effect is the\n")
cat("  appropriate effect size; the completely standardised form is not reported.\n")
cat("\n")

rule(78, "=")
cat("COMPONENT MODELS\n")
rule(78, "=")
cat("\n")

coef_table(m_model, sprintf("Mediator model:      %s ~ %s + %s",
                            M_VAR, X_VAR, COV_VAR))
coef_table(y_model, sprintf("Outcome model:       %s ~ %s + %s + %s",
                            Y_VAR, X_VAR, M_VAR, COV_VAR))
coef_table(t_model, sprintf("Total effect model:  %s ~ %s + %s",
                            Y_VAR, X_VAR, COV_VAR))

rule(78, "=")
cat("READING THE RESULT\n")
rule(78, "=")
cat("\n")

supported <- (ab_ci[1] > 0 && ab_ci[2] > 0) || (ab_ci[1] < 0 && ab_ci[2] < 0)
cat("  H3 is ", if (supported) "SUPPORTED" else "NOT SUPPORTED",
    " - the bootstrap CI for ab ", if (supported) "excludes" else "includes",
    " zero.\n", sep = "")

inconsistent <- sign(cp_eff) != sign(ab_eff) && y_model$p[X_VAR] < .05
if (inconsistent) {
  cat("  Direct and indirect paths carry opposite signs and the direct effect is\n")
  cat("  significant: this is INCONSISTENT MEDIATION (suppression). Do not use\n")
  cat("  partial/full mediation language, and do not compute a proportion\n")
  cat(sprintf("  mediated (ab / c = %s is uninterpretable under sign reversal).\n",
              num(ab_eff / c_eff, 4)))
}
cat(sprintf("  Oddity moves unexpectedness by %s points, or %s SD of %s.\n",
            num(a_eff, 4), num(a_eff / sd_m, 4), M_VAR))
cat(sprintf("  The covariate %s: b = %s (p %s) on %s, b = %s (p %s) on %s.\n",
            COV_VAR,
            num(y_model$b[COV_VAR], 4), pfmt(y_model$p[COV_VAR]), Y_VAR,
            num(m_model$b[COV_VAR], 4), pfmt(m_model$p[COV_VAR]), M_VAR))
cat("\n")

# --- Machine-readable version (handy for pasting into the write-up) --------

paths_df <- data.frame(
  path   = c("a", "b", "c", "c_prime", "ab", "ab_partially_std"),
  label  = c("oddity -> unexpectedness",
             "unexpectedness -> discomfort",
             "total effect of oddity",
             "direct effect of oddity",
             "indirect effect",
             "indirect effect, partially standardised"),
  effect = c(a_eff, b_eff, c_eff, cp_eff, ab_eff, ps_eff),
  se     = c(unname(m_model$se[X_VAR]), unname(y_model$se[M_VAR]),
             unname(t_model$se[X_VAR]), unname(y_model$se[X_VAR]),
             ab_se, ps_se),
  ci_lo  = c(unname(m_model$lo[X_VAR]), unname(y_model$lo[M_VAR]),
             unname(t_model$lo[X_VAR]), unname(y_model$lo[X_VAR]),
             ab_ci[1], ps_ci[1]),
  ci_hi  = c(unname(m_model$hi[X_VAR]), unname(y_model$hi[M_VAR]),
             unname(t_model$hi[X_VAR]), unname(y_model$hi[X_VAR]),
             ab_ci[2], ps_ci[2]),
  stringsAsFactors = FALSE,
  row.names = NULL
)

print(paths_df, digits = 5)
cat("\n")

invisible(list(
  paths   = paths_df,
  m_model = m_model,
  y_model = y_model,
  t_model = t_model,
  boot    = list(ab = boot_ab, ab_ps = boot_ps, seed = SEED, n = length(boot_ab))
))
