#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# h3_direct_effect_probe.R
#
# Why is c' negative?
#
# Across every split in this project the direct effect of oddity on discomfort
# comes out negative while the total effect is positive - inconsistent
# mediation. Holding perceived unexpectedness constant, an odd scene is rated
# MORE comfortable than a normal one.
#
# The working interpretation is that oddity WITHOUT unexpectedness reads as
# interesting rather than threatening - curiosity. This script does not test
# that. No curiosity measure exists in the dataset, and several other stories
# predict the same sign. What it does instead is establish whether the
# phenomenon is real enough to be worth interpreting at all, by attacking it
# from five directions:
#
#   1. COMMON SUPPORT      Is c' estimated from data, or extrapolated?
#                          "Holding unexpectedness constant" is meaningless if
#                          odd and normal scenes never share a rating.
#
#   2. STRATIFIED c'       Within each unexpectedness level separately, are
#                          odd scenes rated more comfortable? This reproduces
#                          the direct-effect claim with no functional form
#                          assumptions at all.
#
#   3. FUNCTIONAL FORM     Is c' an artifact of forcing the b path to be
#                          linear in a 5-point ordinal mediator?
#
#   4. CELL STRUCTURE      The 2x2 quadrant means, and the oddity x activity
#                          interaction on BOTH the mediator and the outcome.
#
#   5. SENSITIVITY         c' conditions on a post-treatment variable, so any
#                          unmeasured cause of both unexpectedness and
#                          discomfort biases it. How strong would such a
#                          confounder have to be to erase the result?
#
# Plus a partial, indirect probe of the curiosity reading using the one
# disposition variable that exists:
#
#   6. PROPENSITY          c_propen (0 = adventurous, 1 = cautious). If the
#                          negative direct effect is curiosity, it should be
#                          STRONGER (more negative) among adventurous
#                          participants. This is a weak test - propensity is
#                          not curiosity - but it is a falsifiable direction,
#                          and a null or reversed result is informative.
#
# Usage:
#   Rscript source/h3_direct_effect_probe.R   # run from the project root
#   source("source/h3_direct_effect_probe.R") # or from within RStudio
#
# Base R only. Seed fixed. The bootstrap section is the slow part; set
# RUN_CLUSTER_BOOT <- FALSE to skip it.
# ---------------------------------------------------------------------------


# --- Configuration ---------------------------------------------------------

SEED    <- 20260727
CONF    <- 95
HC_TYPE <- "HC4"
N_BOOT  <- 10000

Y_VAR   <- "c_discom"  # discomfort     (1 .. 4)
X_VAR   <- "c_oddity"  # oddity         (0 = absent, 1 = present)
M_VAR   <- "c_expect"  # unexpectedness (1 .. 5)
COV_VAR <- "c_noise"   # activity       (0 = low, 1 = high)
P_VAR   <- "c_propen"  # propensity     (0 = adventurous, 1 = cautious)
ID_VAR  <- "participant_index"

P_LABELS <- c("Adventurous", "Cautious")   # for c_propen 0 and 1

# A stratum needs at least this many rows in BOTH oddity conditions before
# its estimate is treated as anything more than an illustration.
MIN_STRATUM <- 10

# Correlated-error sensitivity sweep.
RHO_GRID <- seq(-0.40, 0.40, by = 0.05)

# Cluster bootstrap of c' (resamples participants, not trials).
RUN_CLUSTER_BOOT <- TRUE


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

needed <- c(Y_VAR, X_VAR, M_VAR, COV_VAR, P_VAR)
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0L) {
  stop("Missing column(s) in ", path, ": ", paste(missing_cols, collapse = ", "))
}

have_id <- ID_VAR %in% names(raw)
if (!have_id) {
  warning("Column '", ID_VAR, "' not found - cluster bootstrap disabled.")
  RUN_CLUSTER_BOOT <- FALSE
} else {
  needed <- c(needed, ID_VAR)
}

dat <- raw[needed]
dat <- dat[stats::complete.cases(dat), , drop = FALSE]

Y  <- as.numeric(dat[[Y_VAR]])
X  <- as.numeric(dat[[X_VAR]])
M  <- as.numeric(dat[[M_VAR]])
CV <- as.numeric(dat[[COV_VAR]])
PR <- as.numeric(dat[[P_VAR]])
N  <- nrow(dat)

m_levels <- sort(unique(M))


# --- OLS with HC4 standard errors ------------------------------------------
#
# Takes a ready-made design matrix (this script needs product terms and
# dummy expansions, so the preds-list interface used by the split scripts
# would only get in the way).

hc4_fit <- function(y, Xm) {
  n <- nrow(Xm); p <- ncol(Xm)
  XtXi <- solve(crossprod(Xm))
  b    <- as.vector(XtXi %*% crossprod(Xm, y))
  e    <- as.vector(y - Xm %*% b)
  h     <- rowSums((Xm %*% XtXi) * Xm)
  delta <- pmin(4, h / (p / n))
  w     <- e^2 / (1 - h)^delta
  V  <- XtXi %*% crossprod(Xm * sqrt(w)) %*% XtXi
  se <- sqrt(diag(V))
  df_res <- n - p
  tval  <- b / se
  pval  <- 2 * stats::pt(-abs(tval), df_res)
  tcrit <- stats::qt(1 - (1 - CONF / 100) / 2, df_res)
  r2 <- 1 - sum(e^2) / sum((y - mean(y))^2)
  names(b) <- names(se) <- names(tval) <- names(pval) <- colnames(Xm)
  list(term = colnames(Xm), b = b, se = se, t = tval, p = pval,
       lo = b - tcrit * se, hi = b + tcrit * se,
       r2 = r2, n = n, df_res = df_res,
       sigma = sqrt(sum(e^2) / df_res), resid = e)
}

dm <- function(...) {
  cols <- list(...)
  Xm <- cbind(1, do.call(cbind, unname(cols)))
  colnames(Xm) <- c("Constant", names(cols))
  Xm
}

ols_coef <- function(Xm, y) as.vector(solve(crossprod(Xm), crossprod(Xm, y)))


# --- Formatting helpers ----------------------------------------------------

no_zero <- function(x, digits = 3) {
  sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
}
num  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
ci   <- function(lo, hi, d = 3) sprintf("[%s, %s]", num(lo, d), num(hi, d))
pfmt <- function(p) {
  if (is.na(p)) return("-")
  if (p < .001) "< .001" else no_zero(p, 3)
}
rule <- function(width = 84, ch = "-") cat(strrep(ch, width), "\n", sep = "")
head_block <- function(n, title) {
  cat("\n"); rule(84, "="); cat(n, ". ", title, "\n", sep = ""); rule(84, "="); cat("\n")
}
star <- function(p) if (is.na(p)) "" else if (p < .001) " ***" else
  if (p < .01) " **" else if (p < .05) " *" else ""

coef_block <- function(fit, title, terms = NULL) {
  cat(title, "\n", sep = "")
  rule()
  cat(sprintf("%-16s %9s %8s %22s %9s %9s\n",
              "Term", "b", "SE", sprintf("%d%% CI", CONF), "t", "p"))
  rule()
  keep <- if (is.null(terms)) seq_along(fit$term) else match(terms, fit$term)
  for (k in keep) {
    cat(sprintf("%-16s %9s %8s %22s %9s %9s%s\n",
                fit$term[k], num(fit$b[k], 4), no_zero(fit$se[k], 4),
                ci(fit$lo[k], fit$hi[k], 4), num(fit$t[k], 4),
                pfmt(fit$p[k]), star(fit$p[k])))
  }
  rule()
  cat(sprintf("R-squared = %s   n = %d   [%s]\n\n",
              no_zero(fit$r2, 4), fit$n, HC_TYPE))
}


# --- Baseline models --------------------------------------------------------

m_base <- hc4_fit(M, dm(oddity = X, noise = CV))
y_base <- hc4_fit(Y, dm(oddity = X, expect = M, noise = CV))
t_base <- hc4_fit(Y, dm(oddity = X, noise = CV))

a_hat  <- unname(m_base$b["oddity"])
b_hat  <- unname(y_base$b["expect"])
cp_hat <- unname(y_base$b["oddity"])
c_hat  <- unname(t_base$b["oddity"])

cat("\n")
rule(84, "=")
cat("PROBING THE NEGATIVE DIRECT EFFECT\n")
cat("Is c' real, and if so what is it not?\n")
rule(84, "=")
cat("\n")
cat("Data      ", path, "\n", sep = "")
cat(sprintf("Rows      %d\n", N))
cat(sprintf("Baseline  a = %s   b = %s   c = %s   c' = %s\n",
            num(a_hat, 4), num(b_hat, 4), num(c_hat, 4), num(cp_hat, 4)))
cat("\n")
cat("c' is the effect of oddity on discomfort with unexpectedness held\n")
cat("constant. It is negative and the total effect is positive, so the two\n")
cat("pathways run in opposite directions. Everything below asks whether that\n")
cat("negative sign is a finding or an artifact.\n")


# ===========================================================================
head_block(1, "COMMON SUPPORT - is c' interpolated or extrapolated?")

cat("If odd and normal scenes never receive the same unexpectedness rating,\n")
cat("then 'holding unexpectedness constant' compares nothing to nothing and\n")
cat("c' is pure model extrapolation. Count the overlap.\n\n")

cat(sprintf("%-10s %10s %10s %10s %10s %10s\n",
            M_VAR, "normal n", "odd n", "% normal", "% odd", "support"))
rule()
n0_tot <- sum(X == 0); n1_tot <- sum(X == 1)
support_ok <- 0L
for (v in m_levels) {
  n0 <- sum(M == v & X == 0); n1 <- sum(M == v & X == 1)
  tag <- if (min(n0, n1) >= MIN_STRATUM) "ok" else
         if (min(n0, n1) > 0) "thin" else "NONE"
  if (min(n0, n1) >= MIN_STRATUM) support_ok <- support_ok + min(n0, n1)
  cat(sprintf("%-10g %10d %10d %10s %10s %10s\n", v, n0, n1,
              num(100 * n0 / n0_tot, 1), num(100 * n1 / n1_tot, 1), tag))
}
rule()
cat(sprintf("Levels with usable overlap (both arms >= %d): %d of %d.\n",
            MIN_STRATUM, sum(vapply(m_levels, function(v)
              min(sum(M == v & X == 0), sum(M == v & X == 1)) >= MIN_STRATUM,
              logical(1))), length(m_levels)))
cat("A level marked NONE contributes nothing but leverage; 'thin' contributes\n")
cat("an estimate with almost no data behind it. Read section 2 accordingly.\n")


# ===========================================================================
head_block(2, "STRATIFIED DIRECT EFFECT - c' without a mediation model")

cat("Within each unexpectedness level, regress discomfort on oddity (plus\n")
cat("activity). No mediator, no linearity assumption, no product terms - just\n")
cat("odd vs normal scenes that received the SAME unexpectedness rating.\n")
cat("If the curiosity reading is right, every row should be negative.\n\n")

strat_row <- function(sub, tag) {
  n0 <- sum(sub$x == 0); n1 <- sum(sub$x == 1)
  if (min(n0, n1) < 3) {
    cat(sprintf("%-10s %6d %6d %54s\n", tag, n0, n1, "skipped - no usable contrast"))
    return(invisible(NULL))
  }
  # A thin stratum can leave the covariate constant, which makes the design
  # matrix singular. Drop it rather than abort the whole run.
  preds <- if (stats::sd(sub$cv) > 0) dm(oddity = sub$x, noise = sub$cv)
           else dm(oddity = sub$x)
  f <- try(hc4_fit(sub$y, preds), silent = TRUE)
  if (inherits(f, "try-error")) {
    cat(sprintf("%-10s %6d %6d %54s\n", tag, n0, n1, "skipped - singular fit"))
    return(invisible(NULL))
  }
  cat(sprintf("%-10s %6d %6d %9s %9s %10s %22s%s\n",
              tag, n0, n1,
              num(mean(sub$y[sub$x == 0]), 3), num(mean(sub$y[sub$x == 1]), 3),
              num(f$b["oddity"], 4),
              ci(f$lo["oddity"], f$hi["oddity"], 4),
              star(f$p["oddity"])))
  invisible(NULL)
}

cat(sprintf("%-10s %6s %6s %9s %9s %10s %22s\n",
            M_VAR, "n norm", "n odd", "M(norm)", "M(odd)", "diff",
            sprintf("%d%% CI", CONF)))
rule()
for (v in m_levels) {
  keep <- M == v
  strat_row(list(y = Y[keep], x = X[keep], cv = CV[keep]), sprintf("%g", v))
}
rule()
cat("diff < 0 means odd scenes were rated MORE comfortable at that level of\n")
cat("unexpectedness. * p<.05  ** p<.01  *** p<.001\n")

cat("\nSame, split by ", P_VAR, "\n", sep = "")
rule()
for (pv in c(0, 1)) {
  cat(P_LABELS[pv + 1L], "\n", sep = "")
  cat(sprintf("%-10s %6s %6s %9s %9s %10s %22s\n",
              M_VAR, "n norm", "n odd", "M(norm)", "M(odd)", "diff",
              sprintf("%d%% CI", CONF)))
  for (v in m_levels) {
    keep <- M == v & PR == pv
    strat_row(list(y = Y[keep], x = X[keep], cv = CV[keep]), sprintf("%g", v))
  }
  cat("\n")
}


# ===========================================================================
head_block(3, "FUNCTIONAL FORM - is c' an artifact of a linear b path?")

cat("c_expect is a 5-point ordinal item treated as continuous. If the true\n")
cat("relationship between unexpectedness and discomfort is curved, forcing a\n")
cat("straight line could manufacture a negative c'. Refit with the mediator\n")
cat("as a set of dummies, which imposes no shape at all.\n\n")

M_dummies <- do.call(cbind, lapply(m_levels[-1], function(v) as.numeric(M == v)))
colnames(M_dummies) <- sprintf("expect_%g", m_levels[-1])
Xm_fac <- cbind(Constant = 1, oddity = X, M_dummies, noise = CV)
y_fac  <- hc4_fit(Y, Xm_fac)

coef_block(y_base, "Linear mediator", c("oddity", "expect", "noise"))
coef_block(y_fac,  sprintf("Mediator as factor (reference %s = %g)",
                           M_VAR, m_levels[1]))

cat(sprintf("c' linear = %s     c' factor = %s     shift = %s\n",
            num(cp_hat, 4), num(y_fac$b["oddity"], 4),
            num(y_fac$b["oddity"] - cp_hat, 4)))
cat("The dummy pattern shows whether the b path is straight. If the dummies\n")
cat("rise unevenly the linear b is a summary, not a description - but c'\n")
cat("surviving the switch means the negative sign is not a shape artifact.\n")


# ===========================================================================
head_block(4, "CELL STRUCTURE - the 2x2 quadrants")

cat("Cell means for both the mediator and the outcome, then the oddity x\n")
cat("activity interaction on each. The additive models used elsewhere in this\n")
cat("project cannot see an interaction, so if one exists it has been folded\n")
cat("into a misleadingly null main effect.\n\n")

cell_stats <- function(v) {
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  se <- s / sqrt(n); tc <- stats::qt(1 - (1 - CONF / 100) / 2, n - 1)
  c(n = n, mean = m, sd = s, lo = m - tc * se, hi = m + tc * se)
}

cat(sprintf("%-28s %18s %22s %18s %22s\n", "Condition",
            sprintf("%s M (SD)", M_VAR), sprintf("%d%% CI", CONF),
            sprintf("%s M (SD)", Y_VAR), sprintf("%d%% CI", CONF)))
rule()
for (xo in c(0, 1)) for (nz in c(0, 1)) {
  keep <- X == xo & CV == nz
  sm <- cell_stats(M[keep]); sy <- cell_stats(Y[keep])
  lab <- sprintf("%s / %s activity",
                 if (xo == 0) "No oddity" else "Oddity",
                 if (nz == 0) "low" else "high")
  cat(sprintf("%-28s %18s %22s %18s %22s\n", lab,
              sprintf("%s (%s)", num(sm[["mean"]], 2), num(sm[["sd"]], 2)),
              ci(sm[["lo"]], sm[["hi"]], 2),
              sprintf("%s (%s)", num(sy[["mean"]], 2), num(sy[["sd"]], 2)),
              ci(sy[["lo"]], sy[["hi"]], 2)))
}
rule()
cat("\n")

m_int <- hc4_fit(M, dm(oddity = X, noise = CV, oddity_x_noise = X * CV))
y_int <- hc4_fit(Y, dm(oddity = X, noise = CV, oddity_x_noise = X * CV))

coef_block(m_int, sprintf("Interaction model on the MEDIATOR (%s)", M_VAR))
coef_block(y_int, sprintf("Interaction model on the OUTCOME  (%s)", Y_VAR))

cat("Simple effect of activity, by oddity condition\n")
rule()
cat(sprintf("  on %-10s  no oddity %s   oddity present %s\n", M_VAR,
            num(m_int$b["noise"], 4),
            num(m_int$b["noise"] + m_int$b["oddity_x_noise"], 4)))
cat(sprintf("  on %-10s  no oddity %s   oddity present %s\n", Y_VAR,
            num(y_int$b["noise"], 4),
            num(y_int$b["noise"] + y_int$b["oddity_x_noise"], 4)))
rule()
if (m_int$p["oddity_x_noise"] < .05 &&
    sign(m_int$b["noise"]) != sign(m_int$b["noise"] + m_int$b["oddity_x_noise"])) {
  cat("\n  *** The two simple effects on the mediator have OPPOSITE SIGNS and\n")
  cat("      the interaction is significant. An additive model averages them\n")
  cat("      toward zero and reports activity as inert on the mediator. That\n")
  cat("      conclusion is an artifact of omitting this interaction.\n")
  cat("      recreate3.md currently makes exactly that claim.\n")
}


# ===========================================================================
head_block(5, "SENSITIVITY - how much confounding would erase c'?")

cat("c' conditions on the mediator, which is measured after treatment. If any\n")
cat("unmeasured variable causes both unexpectedness and discomfort, the two\n")
cat("equations have correlated errors and both b and c' are biased.\n\n")
cat("Let rho = Corr(error in the M model, error in the Y model). Under the\n")
cat("linear model the bias-corrected paths are\n\n")
cat("    b(rho)  = b_hat  - k(rho)\n")
cat("    c'(rho) = c'_hat + a * k(rho)      where k(rho) = rho/sqrt(1-rho^2) * s2/s1\n\n")
cat("with s1, s2 the residual SDs of the two models. rho = 0 recovers the\n")
cat("reported estimates. This is the Imai-Keele-Yamamoto correlated-error\n")
cat("idea; medsens() automates a refined version of it.\n\n")

sens <- function(a, bh, cph, s1, s2) {
  k <- function(rho) rho / sqrt(1 - rho^2) * s2 / s1
  list(b = function(rho) bh - k(rho),
       cp = function(rho) cph + a * k(rho),
       ab = function(rho) a * (bh - k(rho)))
}

tip <- function(f, lo, hi) {
  out <- try(stats::uniroot(f, c(lo, hi))$root, silent = TRUE)
  if (inherits(out, "try-error")) NA_real_ else out
}

s_full <- sens(a_hat, b_hat, cp_hat, m_base$sigma, y_base$sigma)

cat(sprintf("s1 = %s   s2 = %s   s2/s1 = %s\n\n",
            num(m_base$sigma, 4), num(y_base$sigma, 4),
            num(y_base$sigma / m_base$sigma, 4)))

cat(sprintf("%8s %12s %12s %12s %10s\n", "rho", "b(rho)", "c'(rho)", "ab(rho)", "c' sign"))
rule()
for (rho in RHO_GRID) {
  cpv <- s_full$cp(rho)
  cat(sprintf("%8s %12s %12s %12s %10s%s\n",
              num(rho, 2), num(s_full$b(rho), 4), num(cpv, 4),
              num(s_full$ab(rho), 4),
              if (cpv < 0) "negative" else "POSITIVE",
              if (abs(rho) < 1e-9) "   <- reported" else ""))
}
rule()

tip_cp <- tip(s_full$cp, 0, 0.98)
tip_ab <- tip(s_full$ab, 0, 0.98)
cat(sprintf("\nTipping points\n"))
cat(sprintf("  c' reaches zero at rho = %s\n",
            if (is.na(tip_cp)) "not within range" else num(tip_cp, 4)))
cat(sprintf("  ab reaches zero at rho = %s\n",
            if (is.na(tip_ab)) "not within range" else num(tip_ab, 4)))
cat("\n")
cat("Read these as robustness, not probability. The larger the tipping point,\n")
cat("the more confounding it would take to explain the result away. Compare\n")
cat("the two: whichever is smaller is the more fragile claim.\n")

cat("\nBy ", P_VAR, "\n", sep = "")
rule()
cat(sprintf("%-14s %9s %9s %10s %14s %14s\n",
            "Group", "a", "b", "c'", "rho(c'=0)", "rho(ab=0)"))
rule()
for (pv in c(0, 1)) {
  keep <- PR == pv
  mg <- hc4_fit(M[keep], dm(oddity = X[keep], noise = CV[keep]))
  yg <- hc4_fit(Y[keep], dm(oddity = X[keep], expect = M[keep], noise = CV[keep]))
  sg <- sens(unname(mg$b["oddity"]), unname(yg$b["expect"]),
             unname(yg$b["oddity"]), mg$sigma, yg$sigma)
  tc <- tip(sg$cp, 0, 0.98); ta <- tip(sg$ab, 0, 0.98)
  cat(sprintf("%-14s %9s %9s %10s %14s %14s\n", P_LABELS[pv + 1L],
              num(mg$b["oddity"], 4), num(yg$b["expect"], 4),
              num(yg$b["oddity"], 4),
              if (is.na(tc)) "-" else num(tc, 4),
              if (is.na(ta)) "-" else num(ta, 4)))
}
rule()


# ===========================================================================
head_block(6, "PROPENSITY - a weak, falsifiable probe of the curiosity story")

cat("The curiosity reading makes a directional prediction. If oddity without\n")
cat("unexpectedness reads as interesting rather than threatening, then people\n")
cat("who describe themselves as adventurous should show a STRONGER (more\n")
cat("negative) direct effect than cautious ones.\n\n")
cat("This is not a test of curiosity. Propensity is a self-reported travel\n")
cat("disposition, not a curiosity measure, and a null result is equally\n")
cat("consistent with the curiosity story being right and propensity being\n")
cat("the wrong proxy. It is included because the prediction is falsifiable\n")
cat("in one direction: a REVERSED result would count against the reading.\n\n")

Xm_sat <- cbind(Constant = 1, oddity = X, expect = M, noise = CV, propen = PR,
                oddity_x_P = X * PR, expect_x_P = M * PR, noise_x_P = CV * PR)
y_sat <- hc4_fit(Y, Xm_sat)

coef_block(y_sat, "Outcome model saturated in propensity")
cat("Every term is interacted with propensity, so the conditional effects\n")
cat("below reproduce h3_Propensity_split.R exactly.\n\n")

cp_adv <- unname(y_sat$b["oddity"])
cp_cau <- unname(y_sat$b["oddity"] + y_sat$b["oddity_x_P"])
b_adv  <- unname(y_sat$b["expect"])
b_cau  <- unname(y_sat$b["expect"] + y_sat$b["expect_x_P"])

cat(sprintf("%-22s %12s %12s %14s %10s\n",
            "Path", P_LABELS[1], P_LABELS[2], "difference", "p"))
rule()
cat(sprintf("%-22s %12s %12s %14s %10s%s\n", "c' direct effect",
            num(cp_adv, 4), num(cp_cau, 4),
            num(y_sat$b["oddity_x_P"], 4), pfmt(y_sat$p["oddity_x_P"]),
            star(y_sat$p["oddity_x_P"])))
cat(sprintf("%-22s %12s %12s %14s %10s%s\n", "b unexpect -> discom",
            num(b_adv, 4), num(b_cau, 4),
            num(y_sat$b["expect_x_P"], 4), pfmt(y_sat$p["expect_x_P"]),
            star(y_sat$p["expect_x_P"])))
cat(sprintf("%-22s %12s %12s %14s %10s%s\n", "activity -> discom",
            num(y_sat$b["noise"], 4),
            num(y_sat$b["noise"] + y_sat$b["noise_x_P"], 4),
            num(y_sat$b["noise_x_P"], 4), pfmt(y_sat$p["noise_x_P"]),
            star(y_sat$p["noise_x_P"])))
rule()
cat("\n")

pred_met <- cp_adv < cp_cau
sig_diff <- y_sat$p["oddity_x_P"] < .05
cat("Verdict on the curiosity prediction\n")
rule()
cat(sprintf("  Predicted:  c'(%s) more negative than c'(%s)\n",
            P_LABELS[1], P_LABELS[2]))
cat(sprintf("  Observed:   %s = %s   vs   %s = %s\n",
            P_LABELS[1], num(cp_adv, 4), P_LABELS[2], num(cp_cau, 4)))
if (!sig_diff) {
  cat("  The difference is not significant, so propensity provides NO support\n")
  cat("  for the curiosity reading. It does not refute it either - propensity\n")
  cat("  is a poor proxy, and this test had no power to speak of.\n")
  if (!pred_met) {
    cat("  Note the point estimates also run the WRONG way, which is mildly\n")
    cat("  discouraging for the reading even though it is not significant.\n")
  }
} else if (pred_met) {
  cat("  Significant AND in the predicted direction - weak positive evidence.\n")
} else {
  cat("  Significant and REVERSED. This counts against the curiosity reading.\n")
}
cat("\n")
cat("  Whatever happens to c', note whether the b path is moderated: that is\n")
cat("  a separate and better-powered finding about who converts unexpectedness\n")
cat("  into discomfort, and it does not depend on the curiosity story at all.\n")


# ===========================================================================
if (RUN_CLUSTER_BOOT) {
  head_block(7, "CLUSTERING - c' with participants resampled, not trials")

  cat("Each participant contributed 32 trials, so the 2,688 rows are not 2,688\n")
  cat("independent observations. HC4 corrects for heteroscedasticity, not for\n")
  cat("nesting. Resample participants instead and see what happens to c'.\n\n")

  ids     <- dat[[ID_VAR]]
  rows_by <- split(seq_len(N), ids)
  K       <- length(rows_by)
  Xm_y    <- cbind(1, X, M, CV)

  boot_cp <- function(mode) {
    set.seed(SEED)
    out <- numeric(N_BOOT); fails <- 0L
    for (i in seq_len(N_BOOT)) {
      idx <- if (mode == "cluster") {
        unlist(rows_by[sample.int(K, K, replace = TRUE)], use.names = FALSE)
      } else sample.int(N, N, replace = TRUE)
      v <- try(ols_coef(Xm_y[idx, , drop = FALSE], Y[idx])[2], silent = TRUE)
      if (inherits(v, "try-error")) { fails <- fails + 1L; out[i] <- NA_real_ }
      else out[i] <- v
    }
    out <- out[!is.na(out)]
    a2 <- (1 - CONF / 100) / 2
    list(se = stats::sd(out), ci = unname(stats::quantile(out, c(a2, 1 - a2))),
         n = length(out), fails = fails)
  }

  br <- boot_cp("row")
  bc <- boot_cp("cluster")

  cat(sprintf("%d participants, %d trials each on average\n\n",
              K, round(N / K)))
  cat(sprintf("%-24s %10s %10s %24s %8s\n",
              "Method", "c'", "SE", sprintf("%d%% CI", CONF), "width"))
  rule()
  cat(sprintf("%-24s %10s %10s %24s %8s\n", paste0(HC_TYPE, " (analytic)"),
              num(cp_hat, 4), no_zero(y_base$se["oddity"], 4),
              ci(y_base$lo["oddity"], y_base$hi["oddity"], 4),
              num(y_base$hi["oddity"] - y_base$lo["oddity"], 4)))
  cat(sprintf("%-24s %10s %10s %24s %8s\n", "row bootstrap",
              num(cp_hat, 4), no_zero(br$se, 4), ci(br$ci[1], br$ci[2], 4),
              num(br$ci[2] - br$ci[1], 4)))
  cat(sprintf("%-24s %10s %10s %24s %8s\n", "CLUSTER bootstrap",
              num(cp_hat, 4), no_zero(bc$se, 4), ci(bc$ci[1], bc$ci[2], 4),
              num(bc$ci[2] - bc$ci[1], 4)))
  rule()
  still <- (bc$ci[1] < 0 && bc$ci[2] < 0)
  cat(sprintf("With participants resampled, c' %s zero.\n",
              if (still) "still excludes" else "NO LONGER excludes"))
  cat(sprintf("Cluster interval is %sx the width of the row interval.\n",
              num((bc$ci[2] - bc$ci[1]) / (br$ci[2] - br$ci[1]), 2)))
}


# ===========================================================================
head_block(8, "WHAT THIS DOES AND DOES NOT ESTABLISH")

cat("Established, if the sections above came back clean:\n")
cat("  - c' is estimated from real overlapping data, not extrapolated.\n")
cat("  - The negative sign reappears within unexpectedness strata, with no\n")
cat("    mediation model and no linearity assumption.\n")
cat("  - It is not an artifact of treating an ordinal mediator as continuous.\n\n")

cat("NOT established:\n")
cat("  - Curiosity. There is no curiosity measure in this dataset. Several\n")
cat("    other mechanisms predict the same negative sign: the odd element\n")
cat("    making an otherwise ambiguous scene legible, a contrast effect\n")
cat("    against the unexpectedness rating just given, or scale anchoring.\n")
cat("  - Causality. c' conditions on a post-treatment variable, so it is only\n")
cat("    causal under an assumption section 5 shows to be fragile.\n\n")

cat("Suggested wording for the thesis:\n")
cat("  Report the negative direct effect as a robust empirical pattern.\n")
cat("  Offer curiosity as an interpretation, explicitly flagged as untested,\n")
cat("  and cite the sensitivity tipping point so the reader can see how much\n")
cat("  unmeasured confounding the claim tolerates. Do not use partial or full\n")
cat("  mediation language, and do not compute a proportion mediated.\n\n")


# --- Machine-readable summary ----------------------------------------------

strat_df <- do.call(rbind, lapply(m_levels, function(v) {
  keep <- M == v
  n0 <- sum(X[keep] == 0); n1 <- sum(X[keep] == 1)
  if (min(n0, n1) < 3) return(NULL)
  f <- try(hc4_fit(Y[keep], dm(oddity = X[keep], noise = CV[keep])), silent = TRUE)
  if (inherits(f, "try-error")) return(NULL)
  data.frame(expect_level = v, n_normal = n0, n_odd = n1,
             diff = unname(f$b["oddity"]), se = unname(f$se["oddity"]),
             ci_lo = unname(f$lo["oddity"]), ci_hi = unname(f$hi["oddity"]),
             p = unname(f$p["oddity"]),
             usable = min(n0, n1) >= MIN_STRATUM,
             stringsAsFactors = FALSE)
}))
rownames(strat_df) <- NULL

cat("Stratified direct effect (machine readable)\n")
print(strat_df, digits = 5)
cat("\n")

invisible(list(
  baseline    = list(a = a_hat, b = b_hat, c = c_hat, cp = cp_hat),
  m_base = m_base, y_base = y_base, t_base = t_base,
  y_factor = y_fac, m_interaction = m_int, y_interaction = y_int,
  y_propensity = y_sat,
  stratified  = strat_df,
  sensitivity = list(tip_cp = tip_cp, tip_ab = tip_ab,
                     s1 = m_base$sigma, s2 = y_base$sigma),
  seed = SEED
))
