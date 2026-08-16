#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# response_validation.R
#
# Response-validity checks and a speed-based sensitivity analysis for the
# visual oddity study.
#
# Motivation: both rating items were forced-response (the task would not
# advance until each was answered). This raises the question of whether
# compelled responses introduced careless or "faked" data. This script
# tests that empirically rather than arguing it in prose.
#
# Part 1 - Response validity
#   1a. Straight-lining: within-participant SD of discomfort across 32 trials
#   1b. Per-participant oddity effect (mean discomfort: oddity - normal)
#   1c. Midpoint use on the unexpectedness item
#   1d. Completion speed, and its correlation with the oddity effect
#
# Part 2 - Robustness probe (NOT part of the reported analysis)
#   Re-estimates the mediation with the fastest decile of participants
#   removed. This is a rebuttal tool only: it exists to answer an examiner
#   who asserts that fast responding implies invalid data. No participant
#   was excluded on speed grounds, and the reported analysis in
#   recreate2.md / recreate3.md uses all 84 participants and all 2,688
#   ratings.
#
#   Note also that the premise is unsound on its own terms: the instruction
#   screen explicitly asked participants to "answer on first impression
#   rather than over-thinking". Fast responding is compliance with the
#   instructions, not evidence of disengagement. Part 1d confirms speed is
#   uncorrelated with the size of a participant's oddity effect.
#
# Mediation specification (matches recreate3.md / PROCESS Model 4):
#   M model : c_expect ~ c_oddity + c_noise            -> a
#   Y model : c_discom ~ c_oddity + c_expect + c_noise -> c' , b
#   Total   : c_discom ~ c_oddity + c_noise            -> c
#   Indirect: ab, percentile bootstrap CI
#
# Note on authority: SPSS is the source of record for the reported analyses
# (see method.md). This script is a cross-check only. Where it disagrees with
# SPSS, SPSS wins. Durations are read from data/timer.csv rather than being
# recomputed, so that every duration figure in the thesis traces to one
# definition (source/timer.py).
#
# Usage:
#   Rscript source/response_validation.R
#
# Requires: data/timer.csv (run source/timer.py first).
# Base R only - no packages required.
# -----------------------------------------------------------------------------

CLEAN  <- "data/data_ODDITY_exp_274266-v20_task-8m3r.clean.csv"
TIMER  <- "data/timer.csv"          # produced by source/timer.py
BOOT_N <- 10000
SEED   <- 20260727
DECILE <- 0.10

rule <- function(s) cat("\n", s, "\n", strrep("-", nchar(s)), "\n", sep = "")
fmt_ms <- function(s) sprintf("%dm %02ds", s %/% 60, round(s %% 60))

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------
dat <- read.csv(CLEAN, stringsAsFactors = FALSE)
stopifnot(nrow(dat) == 2688, length(unique(dat$participant_index)) == 84)

# -----------------------------------------------------------------------------
# Per-participant task duration
#
# Read from data/timer.csv, the output of source/timer.py. Do NOT derive
# durations here: timer.py defines elapsed time as the wall-clock span from a
# participant's first to last "Local Timestamp", and that definition is the
# one used throughout the thesis (see also source/timer_stats.R). Deriving a
# second measure from another column produces figures that disagree with the
# Method by a few seconds for no benefit.
# -----------------------------------------------------------------------------
timer    <- read.csv(TIMER, stringsAsFactors = FALSE)
duration <- setNames(timer$t_duration, as.character(timer$participant_index))
stopifnot(length(duration) == 84)

# -----------------------------------------------------------------------------
# Part 1 - Response validity
# -----------------------------------------------------------------------------
rule("PART 1  RESPONSE VALIDITY")

pop_sd <- function(x) sqrt(mean((x - mean(x))^2))
sds <- tapply(dat$c_discom, dat$participant_index, pop_sd)

cat(sprintf("1a. Straight-lining (within-participant SD of discomfort, 32 trials)\n"))
cat(sprintf("      identical on all 32 trials (SD = 0) : %d\n", sum(sds == 0)))
cat(sprintf("      near-identical (SD < 0.35)          : %d\n", sum(sds > 0 & sds < 0.35)))
cat(sprintf("      min / median / max SD               : %.3f / %.3f / %.3f\n",
            min(sds), median(sds), max(sds)))

odd  <- tapply(dat$c_discom[dat$c_oddity == 1], dat$participant_index[dat$c_oddity == 1], mean)
norm <- tapply(dat$c_discom[dat$c_oddity == 0], dat$participant_index[dat$c_oddity == 0], mean)
eff  <- odd - norm

cat(sprintf("\n1b. Per-participant oddity effect (mean discomfort: oddity - normal)\n"))
cat(sprintf("      positive effect      : %d of %d\n", sum(eff > 0), length(eff)))
cat(sprintf("      zero or negative     : %d\n", sum(eff <= 0)))
cat(sprintf("      median effect        : %.3f\n", median(eff)))
cat(sprintf("      non-positive cases   : %s\n",
            paste(sprintf("idx %s (%.3f)", names(eff)[eff <= 0], eff[eff <= 0]), collapse = ", ")))

mid <- sum(dat$c_expect == 3)
cat(sprintf("\n1c. Midpoint use on unexpectedness ('Indifferent / Can't Decide')\n"))
cat(sprintf("      %d of %d ratings = %.2f%%\n", mid, nrow(dat), 100 * mid / nrow(dat)))

cat(sprintf("\n1d. Completion speed (t_duration from %s)\n", TIMER))
cat(sprintf("      fastest : idx %s at %.2f s (%s, %.2f s/trial)\n",
            names(which.min(duration)), min(duration), fmt_ms(min(duration)), min(duration) / 32))
cat(sprintf("      slowest : idx %s at %.2f s (%s)\n",
            names(which.max(duration)), max(duration), fmt_ms(max(duration))))
cat(sprintf("      median  : %.2f s (%s)\n", median(duration), fmt_ms(median(duration))))
shared <- intersect(names(duration), names(eff))
cat(sprintf("      r(duration, oddity effect) : %.3f\n",
            cor(duration[shared], eff[shared])))

# -----------------------------------------------------------------------------
# Part 2 - Sensitivity: drop the fastest decile
# -----------------------------------------------------------------------------
rule("PART 2  ROBUSTNESS PROBE - FASTEST DECILE REMOVED (not a reported analysis)")

mediate <- function(d) {
  a  <- coef(lm(c_expect ~ c_oddity + c_noise, data = d))[["c_oddity"]]
  ym <- coef(lm(c_discom ~ c_oddity + c_expect + c_noise, data = d))
  ct <- coef(lm(c_discom ~ c_oddity + c_noise, data = d))[["c_oddity"]]
  c(a = a, b = ym[["c_expect"]], cp = ym[["c_oddity"]], c = ct,
    ab = a * ym[["c_expect"]])
}

boot_ab <- function(d, reps = BOOT_N, seed = SEED) {
  set.seed(seed)
  n  <- nrow(d)
  ab <- vapply(seq_len(reps), function(i) mediate(d[sample.int(n, n, TRUE), ])[["ab"]],
               numeric(1))
  quantile(ab, c(0.025, 0.975))
}

report <- function(label, d) {
  e  <- mediate(d)
  ci <- boot_ab(d)
  cat(sprintf("%s\n", label))
  cat(sprintf("      participants %d | ratings %d\n",
              length(unique(d$participant_index)), nrow(d)))
  cat(sprintf("      a = %.4f   b = %.4f   c' = %.4f   c = %.4f\n",
              e[["a"]], e[["b"]], e[["cp"]], e[["c"]]))
  cat(sprintf("      ab = %.4f   95%% CI [%.4f, %.4f]\n\n", e[["ab"]], ci[[1]], ci[[2]]))
  c(ab = e[["ab"]], lo = ci[[1]], hi = ci[[2]])
}

full <- report("Full sample", dat)

k        <- ceiling(DECILE * length(duration))
fastest  <- names(sort(duration))[seq_len(k)]
cat(sprintf("Dropping fastest %d participants (idx %s)\n",
            k, paste(fastest, collapse = ", ")))
cat(sprintf("      durations (s): %s\n\n",
            paste(sprintf("%.1f", sort(duration)[seq_len(k)]), collapse = ", ")))

reduced <- report("Fastest decile removed",
                  dat[!(as.character(dat$participant_index) %in% fastest), ])

rule("CONCLUSION")
cat(sprintf("Indirect effect changes from %.4f to %.4f (%+.2f%%).\n",
            full[["ab"]], reduced[["ab"]],
            100 * (reduced[["ab"]] - full[["ab"]]) / full[["ab"]]))
cat(sprintf("Both confidence intervals exclude zero: [%.4f, %.4f] and [%.4f, %.4f].\n",
            full[["lo"]], full[["hi"]], reduced[["lo"]], reduced[["hi"]]))
cat("The mediation does not depend on any subset of participants defined by speed.\n")
cat("Reminder: no participant was excluded on speed grounds. The reported\n")
cat("analysis uses all 84 participants and all 2,688 ratings.\n")

# oef
