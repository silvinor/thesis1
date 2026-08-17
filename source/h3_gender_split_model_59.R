#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# h3_gender_split_model_59.R
#
# Hypothesis 3 - "Unexpectedness mediates" - does GENDER moderate it?
#
# h3_gender_split.R fitted the mediation separately within each gender and
# left the two sets of estimates side by side, which cannot answer whether
# the groups actually differ. This script answers that question properly:
# a single moderated-mediation model, fitted to all 2,688 rows at once, in
# which gender is allowed to move every path, followed by a bootstrap of the
# difference between the two conditional indirect effects.
#
# This is the PROCESS v4.2 (Hayes, 2022) MODEL 59 specification:
#
#   PROCESS
#       y=c_discom /x=c_oddity /m=c_expect /w=W_male /cov=c_noise
#       /model=59 /boot=10000 /conf=95 /hc=4 /seed=20260727.
#
# Model 59 is the fully moderated simple mediation - W moderates all three
# paths (a, b and c'):
#
#                       W                 W
#                       |                 |
#                       v                 v
#          X --------> (a) ---> M ------> (b) ---> Y
#           \                                      ^
#            \____________ (c') __________________/
#                             ^
#                             |
#                             W
#
#   M model:  c_expect ~ a1*X + a2*W + a3*(X*W) + g1*c_noise
#   Y model:  c_discom ~ c1*X + c2*W + c3*(X*W) + b1*M + b2*(M*W) + g2*c_noise
#
# where X = c_oddity, M = c_expect, W = gender (see W_REF / W_FOCAL below).
#
#   conditional indirect effect at W = w :  (a1 + a3*w) * (b1 + b2*w)
#   conditional direct   effect at W = w :   c1 + c3*w
#
# THE TOTAL EFFECT MODEL
#
#   Model 59 decomposes the effect of X into a direct and an indirect part
#   and never fits the total effect, so on its own it cannot say whether the
#   OVERALL effect of oddity differs by gender. A third model is therefore
#   added here (the analogue of PROCESS /total=1, which Model 59 does not
#   support):
#
#     Total model:  c_discom ~ ct1*X + ct2*W + ct3*(X*W) + g3*c_noise
#
#     conditional total effect at W = w :  ct1 + ct3*w
#
#   ct3 is the moderation of the total effect, and the decomposition
#   identity ct3 = c3 + (indirect contrast) is printed as a check. This
#   matters because the total effect can differ by group even when neither
#   of its two components differs detectably on its own - which is exactly
#   what happens in this dataset.
#
# WHY THERE IS NO "INDEX OF MODERATED MEDIATION" HERE
#
#   In Model 7 or Model 14 only one path is moderated, so the indirect effect
#   is a linear function of W and a single index summarises the moderation.
#   In Model 59 both a and b are moderated by the same W, so the indirect
#   effect (a1 + a3*w)(b1 + b2*w) is QUADRATIC in w and no such index exists -
#   PROCESS does not print one for this model. Because W is dichotomous here,
#   the meaningful equivalent is the CONTRAST between the two conditional
#   indirect effects, bootstrapped directly. That contrast is the headline
#   test this script exists to produce.
#
# WHAT IS AND IS NOT TESTED
#
#   Tested:      whether a, b and c' differ by gender (a3, b2, c3), and
#                whether the indirect effect differs by gender (the contrast).
#   Not tested:  causality. Gender is a measured participant characteristic,
#                not a manipulated factor, so a difference would be confounded
#                with anything else that differs between the groups.
#
# Usage:
#   Rscript source/h3_gender_split_model_59.R   # run from the project root
#   source("source/h3_gender_split_model_59.R") # or from within RStudio
#
# Base R only - no external packages required (no `sandwich`, no `boot`,
# no `lavaan`, no `mediation`). Results are reproducible: the RNG seed is
# fixed below.
#
# NOTE: bootstrap CIs will differ in the third decimal place from SPSS output,
# because PROCESS and R draw their resamples from different RNGs. Every OLS
# quantity (coefficients, HC4 SEs, t, p, R-squared, F) is exact.
# ---------------------------------------------------------------------------


# --- Configuration ---------------------------------------------------------

SEED    <- 20260727    # matches /seed= in the SPSS syntax
N_BOOT  <- 10000       # matches /boot=
CONF    <- 95          # matches /conf=
HC_TYPE <- "HC4"       # matches /hc=4

Y_VAR   <- "c_discom"  # Y - discomfort (1 = absolutely yes .. 4 = certainly not)
X_VAR   <- "c_oddity"  # X - oddity      (0 = absent, 1 = present)
M_VAR   <- "c_expect"  # M - unexpectedness (1 .. 5)
COV_VAR <- "c_noise"   # covariate - background activity (0 = low, 1 = high)

# Declared endpoints of the Y response scale, used only to express effect
# sizes as a percentage of the scale. `comfortable` has FOUR options, so
# c_discom runs 1..4 and the span is 3 - not 4. Do not infer this from
# range(Y): if no participant ever used an endpoint the span would silently
# shrink and every percentage would be overstated. The observed range is
# checked against these values below.
Y_SCALE_MIN <- 1
Y_SCALE_MAX <- 4

# The moderator. `gender` is free text, so it is dummy-coded here:
#   W = 0 for W_REF, W = 1 for W_FOCAL.
# Every "conditional at W = 0" figure therefore refers to W_REF, and the
# contrast is reported in the direction FOCAL minus REF.
W_SRC   <- "gender"
W_REF   <- "Female"    # coded 0 - the reference group
W_FOCAL <- "Male"      # coded 1
W_VAR   <- "W_male"    # label used in the output tables

# PROCESS Model 59 puts the covariate in both models as a main effect only -
# its coefficient is constrained equal across groups. Set this to TRUE to
# interact the covariate with W as well, which saturates the model and makes
# the conditional effects reproduce h3_gender_split.R exactly. Useful as a
# cross-check; leave FALSE to match PROCESS.
SATURATE_COV <- FALSE

# PROCESS does not mean-centre by default (/center=0), and both X and W are
# dichotomous 0/1 here, so the products need no centring and every lower-order
# coefficient reads directly as a simple effect at the other variable = 0.


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

needed <- c(Y_VAR, X_VAR, M_VAR, COV_VAR, W_SRC)
missing_cols <- setdiff(needed, names(raw))
if (length(missing_cols) > 0L) {
  stop("Missing column(s) in ", path, ": ", paste(missing_cols, collapse = ", "))
}

dat <- raw[needed]
dat[[W_SRC]] <- trimws(as.character(dat[[W_SRC]]))
dat[[W_SRC]][dat[[W_SRC]] == ""] <- NA_character_
dat <- dat[stats::complete.cases(dat), , drop = FALSE]

n_total <- nrow(raw)

# Keep only the two coded levels; warn about anything else.
seen <- sort(unique(dat[[W_SRC]]))
extra <- setdiff(seen, c(W_REF, W_FOCAL))
if (!all(c(W_REF, W_FOCAL) %in% seen)) {
  stop("Expected ", W_SRC, " levels \"", W_REF, "\" and \"", W_FOCAL,
       "\" but found ", paste(seen, collapse = "/"))
}
if (length(extra) > 0L) {
  warning(W_SRC, " contains unhandled level(s): ", paste(extra, collapse = ", "),
          " - those rows are dropped.")
  dat <- dat[dat[[W_SRC]] %in% c(W_REF, W_FOCAL), , drop = FALSE]
}

n_used    <- nrow(dat)
n_dropped <- n_total - n_used

Y  <- as.numeric(dat[[Y_VAR]])
X  <- as.numeric(dat[[X_VAR]])
M  <- as.numeric(dat[[M_VAR]])
CV <- as.numeric(dat[[COV_VAR]])
W  <- as.numeric(dat[[W_SRC]] == W_FOCAL)

n_ref   <- sum(W == 0)
n_focal <- sum(W == 1)


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
#
# Unlike the h3_*_split.R scripts this takes a ready-made design matrix,
# because Model 59 needs product terms.

hc4_fit <- function(y, Xm) {
  n <- nrow(Xm)
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

  r2 <- 1 - sum(e^2) / sum((y - mean(y))^2)

  # Omnibus Wald F on the slopes, using the HC4 covariance matrix - this is
  # what PROCESS prints when an HC estimator is requested, not the classical
  # R-squared F.
  slopes  <- b[-1]
  V_slope <- V[-1, -1, drop = FALSE]
  df_mod  <- p - 1
  Fstat   <- as.numeric(t(slopes) %*% solve(V_slope) %*% slopes) / df_mod
  Fp      <- stats::pf(Fstat, df_mod, df_res, lower.tail = FALSE)

  names(b) <- names(se) <- names(tval) <- names(pval) <- colnames(Xm)

  list(
    term = colnames(Xm), b = b, se = se, t = tval, p = pval, V = V,
    lo = b - tcrit * se, hi = b + tcrit * se,
    r2 = r2, F = Fstat, df_mod = df_mod, df_res = df_res, Fp = Fp, n = n
  )
}

ols_coef <- function(Xm, y) {
  as.vector(solve(crossprod(Xm), crossprod(Xm, y)))
}


# --- Build the Model 59 design matrices ------------------------------------

XW <- X * W    # moderation of the a path (in M model) and c' path (in Y model)
MW <- M * W    # moderation of the b path (in Y model)

xw_lab <- paste0(X_VAR, ":", W_VAR)
mw_lab <- paste0(M_VAR, ":", W_VAR)
nw_lab <- paste0(COV_VAR, ":", W_VAR)

m_cols <- list(Constant = rep(1, n_used), X = X, W = W, XW = XW, CV = CV)
m_labs <- c("Constant", X_VAR, W_VAR, xw_lab, COV_VAR)

y_cols <- list(Constant = rep(1, n_used), X = X, W = W, XW = XW,
               M = M, MW = MW, CV = CV)
y_labs <- c("Constant", X_VAR, W_VAR, xw_lab, M_VAR, mw_lab, COV_VAR)

# Total effect model - same as the Y model but with the mediator removed.
t_cols <- list(Constant = rep(1, n_used), X = X, W = W, XW = XW, CV = CV)
t_labs <- c("Constant", X_VAR, W_VAR, xw_lab, COV_VAR)

if (SATURATE_COV) {
  m_cols <- c(m_cols, list(CVW = CV * W)); m_labs <- c(m_labs, nw_lab)
  y_cols <- c(y_cols, list(CVW = CV * W)); y_labs <- c(y_labs, nw_lab)
  t_cols <- c(t_cols, list(CVW = CV * W)); t_labs <- c(t_labs, nw_lab)
}

Xm_m <- do.call(cbind, m_cols); colnames(Xm_m) <- m_labs
Xm_y <- do.call(cbind, y_cols); colnames(Xm_y) <- y_labs
Xm_t <- do.call(cbind, t_cols); colnames(Xm_t) <- t_labs

# Column positions used to pull the paths out of the coefficient vectors.
i_a1 <- 2L                       # X in the M model
i_a3 <- 4L                       # X:W in the M model
i_c1 <- 2L                       # X in the Y model
i_c3 <- 4L                       # X:W in the Y model
i_b1 <- 5L                       # M in the Y model
i_b2 <- 6L                       # M:W in the Y model
i_t1 <- 2L                       # X in the total effect model
i_t3 <- 4L                       # X:W in the total effect model

m_model <- hc4_fit(M, Xm_m)
y_model <- hc4_fit(Y, Xm_y)
t_model <- hc4_fit(Y, Xm_t)

a1  <- unname(m_model$b[i_a1]); a3  <- unname(m_model$b[i_a3])
c1  <- unname(y_model$b[i_c1]); c3  <- unname(y_model$b[i_c3])
b1  <- unname(y_model$b[i_b1]); b2  <- unname(y_model$b[i_b2])
ct1 <- unname(t_model$b[i_t1]); ct3 <- unname(t_model$b[i_t3])

# Conditional effects. W is 0/1, so w = 0 is W_REF and w = 1 is W_FOCAL.
cond_a   <- function(w) a1 + a3 * w
cond_b   <- function(w) b1 + b2 * w
cond_ind <- function(w) cond_a(w) * cond_b(w)
cond_dir <- function(w) c1 + c3 * w
cond_tot <- function(w) ct1 + ct3 * w

ind_ref   <- cond_ind(0); ind_focal <- cond_ind(1)
dir_ref   <- cond_dir(0); dir_focal <- cond_dir(1)
tot_ref   <- cond_tot(0); tot_focal <- cond_tot(1)
ind_diff  <- ind_focal - ind_ref
dir_diff  <- c3                        # by construction
tot_diff  <- ct3                       # by construction

# Decomposition check: the moderation of the total effect must equal the
# moderation of the direct effect plus the contrast between the conditional
# indirect effects. Exact in OLS, up to floating point.
decomp_gap <- tot_diff - (dir_diff + ind_diff)

sd_y <- stats::sd(Y)                   # full-sample SD, as PROCESS uses


# --- Bootstrap --------------------------------------------------------------
#
# Percentile bootstrap, case resampling, N_BOOT replicates. Each replicate
# refits both models and recomputes the two conditional indirect effects and
# their contrast. The contrast CI is the test of whether gender moderates the
# mediation; the two conditional CIs only say whether mediation holds within
# each group, which the split script already established.

set.seed(SEED)

boot <- matrix(NA_real_, nrow = N_BOOT, ncol = 6)
colnames(boot) <- c("ind_ref", "ind_focal", "ind_diff", "ind_diff_ps",
                    "dir_diff", "tot_diff")
n_fail <- 0L

for (i in seq_len(N_BOOT)) {
  idx <- sample.int(n_used, n_used, replace = TRUE)
  res <- try({
    cm <- ols_coef(Xm_m[idx, , drop = FALSE], M[idx])
    cy <- ols_coef(Xm_y[idx, , drop = FALSE], Y[idx])
    ct <- ols_coef(Xm_t[idx, , drop = FALSE], Y[idx])
    A1 <- cm[i_a1]; A3 <- cm[i_a3]
    B1 <- cy[i_b1]; B2 <- cy[i_b2]
    i0 <- A1 * B1
    i1 <- (A1 + A3) * (B1 + B2)
    c(i0, i1, i1 - i0, (i1 - i0) / stats::sd(Y[idx]), cy[i_c3], ct[i_t3])
  }, silent = TRUE)

  if (inherits(res, "try-error")) {
    n_fail <- n_fail + 1L
  } else {
    boot[i, ] <- res
  }
}

boot <- boot[stats::complete.cases(boot), , drop = FALSE]
n_boot_ok <- nrow(boot)

alpha <- (1 - CONF / 100) / 2
probs <- c(alpha, 1 - alpha)

bse <- function(col) stats::sd(boot[, col])
bci <- function(col) unname(stats::quantile(boot[, col], probs))


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

rule <- function(width = 82, ch = "-") cat(strrep(ch, width), "\n", sep = "")

excl_zero <- function(lo, hi) (lo > 0 && hi > 0) || (lo < 0 && hi < 0)

coef_table <- function(fit, title) {
  cat(title, "\n", sep = "")
  rule()
  cat(sprintf("%-20s %9s %8s %22s %9s %9s\n",
              "Term", "b", "SE", sprintf("%d%% CI", CONF), "t", "p"))
  rule()
  for (k in seq_along(fit$term)) {
    cat(sprintf("%-20s %9s %8s %22s %9s %9s\n",
                fit$term[k],
                num(fit$b[k], 4),
                no_zero(fit$se[k], 4),
                ci(fit$lo[k], fit$hi[k], 4),
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
rule(82, "=")
cat("H3 - Does gender moderate the mediation?\n")
cat("Moderated mediation (PROCESS Model 59 equivalent), base R\n")
rule(82, "=")
cat("\n")

cat("Data      ", path, "\n", sep = "")
cat(sprintf("Rows      %d read, %d used, %d dropped\n",
            n_total, n_used, n_dropped))
cat(sprintf("Model     Y = %s   X = %s   M = %s   W = %s   covariate = %s\n",
            Y_VAR, X_VAR, M_VAR, W_VAR, COV_VAR))
cat(sprintf("Moderator %s: \"%s\" = 0 (n = %d),  \"%s\" = 1 (n = %d)\n",
            W_SRC, W_REF, n_ref, W_FOCAL, n_focal))
cat(sprintf("Inference %s standard errors; %d bootstrap samples; %d%% CI; seed %d\n",
            HC_TYPE, n_boot_ok, CONF, SEED))
if (SATURATE_COV) {
  cat("          SATURATE_COV = TRUE - covariate interacted with W, so the\n")
  cat("          conditional effects below reproduce h3_gender_split.R exactly.\n")
}
if (n_fail > 0L) {
  cat(sprintf("          %d bootstrap sample(s) failed and were discarded\n", n_fail))
}
cat("\n")

rule(82, "=")
cat("1. DOES GENDER MOVE EACH PATH?  (the three interaction terms)\n")
rule(82, "=")
cat("\n")
cat("Each row asks whether one path differs between \"", W_REF, "\" and \"",
    W_FOCAL, "\".\n", sep = "")
cat("These are the only coefficients in the two models that bear on moderation.\n\n")

cat(sprintf("%-38s %9s %8s %22s %9s\n",
            "Moderated path", "Difference", "SE", sprintf("%d%% CI", CONF), "p"))
rule()
cat(sprintf("%-38s %9s %8s %22s %9s\n",
            sprintf("a3  %s (X -> M)", xw_lab),
            num(a3, 4), no_zero(m_model$se[i_a3], 4),
            ci(m_model$lo[i_a3], m_model$hi[i_a3], 4),
            pfmt(m_model$p[i_a3])))
cat(sprintf("%-38s %9s %8s %22s %9s\n",
            sprintf("b2  %s (M -> Y)", mw_lab),
            num(b2, 4), no_zero(y_model$se[i_b2], 4),
            ci(y_model$lo[i_b2], y_model$hi[i_b2], 4),
            pfmt(y_model$p[i_b2])))
cat(sprintf("%-38s %9s %8s %22s %9s\n",
            sprintf("c3  %s (X -> Y direct)", xw_lab),
            num(c3, 4), no_zero(y_model$se[i_c3], 4),
            ci(y_model$lo[i_c3], y_model$hi[i_c3], 4),
            pfmt(y_model$p[i_c3])))
rule()
cat(sprintf("%-38s %9s %8s %22s %9s\n",
            sprintf("ct3 %s (X -> Y total)", xw_lab),
            num(ct3, 4), no_zero(t_model$se[i_t3], 4),
            ci(t_model$lo[i_t3], t_model$hi[i_t3], 4),
            pfmt(t_model$p[i_t3])))
rule()
cat("Difference is ", W_FOCAL, " minus ", W_REF,
    ". SEs are ", HC_TYPE, ".\n", sep = "")
cat("ct3 comes from the separate total effect model and is NOT part of\n")
cat("Model 59 proper; a3, b2 and c3 are. It is reported because the total\n")
cat("effect can be moderated even when neither component is.\n")
cat("\n")

rule(82, "=")
cat("2. CONDITIONAL EFFECTS AT EACH LEVEL OF ", toupper(W_SRC), "\n", sep = "")
rule(82, "=")
cat("\n")

ci_ref   <- bci("ind_ref")
ci_focal <- bci("ind_focal")

cat(sprintf("%-12s %9s %9s %9s %9s %9s %24s\n",
            W_SRC, "a", "b", "direct c'", "indirect", "total c",
            sprintf("%d%% CI (ind)", CONF)))
rule()
cat(sprintf("%-12s %9s %9s %9s %9s %9s %24s\n",
            W_REF, num(cond_a(0), 4), num(cond_b(0), 4), num(dir_ref, 4),
            num(ind_ref, 4), num(tot_ref, 4), ci(ci_ref[1], ci_ref[2], 4)))
cat(sprintf("%-12s %9s %9s %9s %9s %9s %24s\n",
            W_FOCAL, num(cond_a(1), 4), num(cond_b(1), 4), num(dir_focal, 4),
            num(ind_focal, 4), num(tot_focal, 4),
            ci(ci_focal[1], ci_focal[2], 4)))
rule()
cat("a, b and c' are simple effects recovered from the single Model 59 fit;\n")
cat("total c comes from the separate total effect model. The CI shown is the\n")
cat("percentile bootstrap interval for the indirect effect.\n")
cat("Both CIs excluding zero means mediation holds in BOTH groups - it does\n")
cat("NOT mean the groups differ. That is section 3.\n")
cat("\n")

rule(82, "=")
cat("3. THE TEST:  DOES THE INDIRECT EFFECT DIFFER BY ", toupper(W_SRC), "?\n", sep = "")
rule(82, "=")
cat("\n")

d_ci    <- bci("ind_diff")
dps_ci  <- bci("ind_diff_ps")
dir_ci  <- bci("dir_diff")
tot_ci  <- bci("tot_diff")

cat(sprintf("%-40s %11s %9s %24s\n",
            "Contrast", "Estimate", "BootSE", sprintf("%d%% CI (boot)", CONF)))
rule()
cat(sprintf("%-40s %11s %9s %24s\n",
            "indirect: focal - ref",
            num(ind_diff, 4), no_zero(bse("ind_diff"), 4),
            ci(d_ci[1], d_ci[2], 4)))
cat(sprintf("%-40s %11s %9s %24s\n",
            "indirect: focal - ref, part. std.",
            num(ind_diff / sd_y, 4), no_zero(bse("ind_diff_ps"), 4),
            ci(dps_ci[1], dps_ci[2], 4)))
cat(sprintf("%-40s %11s %9s %24s\n",
            "direct: focal - ref  (= c3)",
            num(dir_diff, 4), no_zero(bse("dir_diff"), 4),
            ci(dir_ci[1], dir_ci[2], 4)))
cat(sprintf("%-40s %11s %9s %24s\n",
            "total: focal - ref  (= ct3)",
            num(tot_diff, 4), no_zero(bse("tot_diff"), 4),
            ci(tot_ci[1], tot_ci[2], 4)))
rule()
cat("Partially standardised uses the full-sample SD of ", Y_VAR,
    " (", num(sd_y, 4), ").\n", sep = "")
cat(sprintf("ct3 also has an %s test in section 1: p %s.\n",
            HC_TYPE, pfmt(t_model$p[i_t3])))
cat("\n")

cat("Decomposition check\n")
rule()
cat(sprintf("  total contrast   %s\n", num(tot_diff, 6)))
cat(sprintf("  direct contrast  %s\n", num(dir_diff, 6)))
cat(sprintf("  indirect contrast %s\n", num(ind_diff, 6)))
cat(sprintf("  direct + indirect %s   (residual %s)\n",
            num(dir_diff + ind_diff, 6), num(decomp_gap, 8)))
if (abs(decomp_gap) > 1e-8) {
  cat("  WARNING: decomposition does not close - check the design matrices.\n")
}
cat("\n")

cat("Model 59 moderates both a and b with the same W, so the indirect effect\n")
cat("is quadratic in W and no single index of moderated mediation exists.\n")
cat("W is dichotomous, so the contrast above is the equivalent test.\n")
cat("\n")

rule(82, "=")
cat("4. VERDICT\n")
rule(82, "=")
cat("\n")

mod_ind <- excl_zero(d_ci[1], d_ci[2])
mod_tot <- excl_zero(tot_ci[1], tot_ci[2])
any_path <- any(c(m_model$p[i_a3], y_model$p[i_b2], y_model$p[i_c3]) < .05)

cat("  Moderated mediation is ", if (mod_ind) "SUPPORTED" else "NOT SUPPORTED",
    " - the bootstrap CI for the\n  difference between conditional indirect effects ",
    if (mod_ind) "excludes" else "includes", " zero.\n", sep = "")
cat("  The TOTAL effect of ", X_VAR, " ",
    if (mod_tot) "DOES" else "does NOT",
    " differ by ", W_SRC, " - ct3 CI ",
    if (mod_tot) "excludes" else "includes", " zero.\n", sep = "")
cat("\n")
if (!mod_ind && !any_path && !mod_tot) {
  cat("  Nothing is moderated: all three interaction terms and both contrasts\n")
  cat("  are non-significant. The apparent gap in the stratified script is\n")
  cat("  sampling noise, not a ", W_SRC, " difference.\n", sep = "")
} else if (!mod_ind && any_path) {
  cat("  At least one individual path is moderated, but the indirect effect as a\n")
  cat("  whole is not. Report the path-level result and do not claim the\n")
  cat("  mediation differs overall.\n")
} else if (!mod_ind && !any_path && mod_tot) {
  cat("  Read this carefully. The TOTAL effect is moderated, but neither of its\n")
  cat("  two components reaches significance on its own. That is not a\n")
  cat("  contradiction: the total contrast is the sum of the direct and\n")
  cat("  indirect contrasts, and a sum can be estimated more precisely than\n")
  cat("  either part. It is also the pattern a marginal result produces when\n")
  cat("  several tests are run without correction.\n")
  cat("  Report the total effect difference as SUGGESTIVE, locate it in the\n")
  cat("  direct pathway descriptively, and do not claim the mediation itself\n")
  cat("  differs by ", W_SRC, ".\n", sep = "")
}
cat("\n")
cat(sprintf("  Pooled reference (h3_full_set.R): indirect = 1.1622, total = 0.7716.\n"))
cat(sprintf("  Here, indirect: %s = %s, %s = %s.\n",
            W_REF, num(ind_ref, 4), W_FOCAL, num(ind_focal, 4)))
cat(sprintf("  Here, total:    %s = %s, %s = %s.\n",
            W_REF, num(tot_ref, 4), W_FOCAL, num(tot_focal, 4)))
cat("\n")

# Scale context for the total effect gap - the headline number is easy to
# misread as a percentage, so it is spelled out in three units.
y_span <- Y_SCALE_MAX - Y_SCALE_MIN
if (min(Y) < Y_SCALE_MIN || max(Y) > Y_SCALE_MAX) {
  warning(Y_VAR, " observed range [", min(Y), ", ", max(Y),
          "] falls outside the declared scale [", Y_SCALE_MIN, ", ",
          Y_SCALE_MAX, "] - check Y_SCALE_MIN / Y_SCALE_MAX.")
}
cat("  Scale context for the total effect gap\n")
cat(sprintf("    %s scale %g to %g (span %g), observed %g to %g; SD = %s\n",
            Y_VAR, Y_SCALE_MIN, Y_SCALE_MAX, y_span,
            min(Y), max(Y), num(sd_y, 4)))
cat(sprintf("    %-8s total effect %s  = %s%% of span = %s SD\n",
            W_REF, num(tot_ref, 4), num(100 * tot_ref / y_span, 2),
            num(tot_ref / sd_y, 3)))
cat(sprintf("    %-8s total effect %s  = %s%% of span = %s SD\n",
            W_FOCAL, num(tot_focal, 4), num(100 * tot_focal / y_span, 2),
            num(tot_focal / sd_y, 3)))
cat(sprintf("    %-8s gap          %s  = %s%% of span = %s SD\n",
            "", num(abs(tot_diff), 4), num(100 * abs(tot_diff) / y_span, 2),
            num(abs(tot_diff) / sd_y, 3)))
cat("\n")
cat("  Caveat: ", W_SRC, " is measured, not manipulated. Even a significant\n",
    "  contrast would be confounded with anything else differing between groups.\n",
    sep = "")
cat("\n")

rule(82, "=")
cat("5. COMPONENT MODELS\n")
rule(82, "=")
cat("\n")

coef_table(m_model, sprintf("Mediator model:      %s ~ %s",
                            M_VAR, paste(m_model$term[-1], collapse = " + ")))
coef_table(y_model, sprintf("Outcome model:       %s ~ %s",
                            Y_VAR, paste(y_model$term[-1], collapse = " + ")))
coef_table(t_model, sprintf("Total effect model:  %s ~ %s",
                            Y_VAR, paste(t_model$term[-1], collapse = " + ")))


# --- Machine-readable version (handy for pasting into the write-up) --------

effects_df <- data.frame(
  quantity = c("a_ref", "a_focal", "b_ref", "b_focal",
               "direct_ref", "direct_focal",
               "indirect_ref", "indirect_focal",
               "total_ref", "total_focal",
               "a3_moderation", "b2_moderation", "c3_moderation",
               "ct3_moderation_total",
               "indirect_contrast", "indirect_contrast_ps"),
  level    = c(W_REF, W_FOCAL, W_REF, W_FOCAL, W_REF, W_FOCAL, W_REF, W_FOCAL,
               W_REF, W_FOCAL,
               "focal - ref", "focal - ref", "focal - ref", "focal - ref",
               "focal - ref", "focal - ref"),
  estimate = c(cond_a(0), cond_a(1), cond_b(0), cond_b(1),
               dir_ref, dir_focal, ind_ref, ind_focal,
               tot_ref, tot_focal,
               a3, b2, c3, ct3, ind_diff, ind_diff / sd_y),
  se       = c(NA, NA, NA, NA,
               unname(y_model$se[i_c1]), NA,
               bse("ind_ref"), bse("ind_focal"),
               unname(t_model$se[i_t1]), NA,
               unname(m_model$se[i_a3]), unname(y_model$se[i_b2]),
               unname(y_model$se[i_c3]), unname(t_model$se[i_t3]),
               bse("ind_diff"), bse("ind_diff_ps")),
  ci_lo    = c(NA, NA, NA, NA,
               unname(y_model$lo[i_c1]), NA,
               ci_ref[1], ci_focal[1],
               unname(t_model$lo[i_t1]), NA,
               unname(m_model$lo[i_a3]), unname(y_model$lo[i_b2]),
               unname(y_model$lo[i_c3]), unname(t_model$lo[i_t3]),
               d_ci[1], dps_ci[1]),
  ci_hi    = c(NA, NA, NA, NA,
               unname(y_model$hi[i_c1]), NA,
               ci_ref[2], ci_focal[2],
               unname(t_model$hi[i_t1]), NA,
               unname(m_model$hi[i_a3]), unname(y_model$hi[i_b2]),
               unname(y_model$hi[i_c3]), unname(t_model$hi[i_t3]),
               d_ci[2], dps_ci[2]),
  p        = c(NA, NA, NA, NA,
               unname(y_model$p[i_c1]), NA, NA, NA,
               unname(t_model$p[i_t1]), NA,
               unname(m_model$p[i_a3]), unname(y_model$p[i_b2]),
               unname(y_model$p[i_c3]), unname(t_model$p[i_t3]),
               NA, NA),
  stringsAsFactors = FALSE,
  row.names = NULL
)

print(effects_df, digits = 5)
cat("\n")

invisible(list(
  effects = effects_df,
  m_model = m_model, y_model = y_model, t_model = t_model,
  boot    = boot, seed = SEED
))
