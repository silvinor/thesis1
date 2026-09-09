#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# timer_stats.R
#
# Descriptive statistics for the participant timing data (data/timer.csv).
#
# Reports:
#   - Counts of males and females
#   - Mean, SD and IQR of age
#   - Mean and SD of experiment duration
#   - Minimum and maximum experiment duration
#
# Usage:
#   Rscript source/timer_stats.R            # run from the project root
#   source("source/timer_stats.R")          # or from within RStudio
#
# Base R only - no external packages required.
# ---------------------------------------------------------------------------

# --- Locate the data file --------------------------------------------------
# Works whether the script is run from the project root or from source/.
find_data <- function() {
  candidates <- c("data/timer.csv", "../data/timer.csv")
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("Could not find data/timer.csv - run this from the project root.")
  }
  hit[1L]
}

path <- find_data()
timer <- read.csv(path, stringsAsFactors = FALSE)

# --- Helper: format seconds as "Xm YYs" ------------------------------------
as_mmss <- function(secs) {
  sprintf("%dm %02ds", secs %/% 60, round(secs %% 60))
}

# --- Helper: mean / SD / range as a one-row summary ------------------------
describe <- function(x) {
  x <- x[!is.na(x)]
  c(
    n      = length(x),
    mean   = mean(x),
    sd     = sd(x),                          # sample SD (n - 1 denominator)
    min    = min(x),
    max    = max(x),
    median = median(x),
    q1     = unname(quantile(x, 0.25)),      # type 7 (R default)
    q3     = unname(quantile(x, 0.75)),
    iqr    = IQR(x)                          # == q3 - q1
  )
}

age <- describe(timer$age)
dur <- describe(timer$t_duration)   # seconds
sex <- table(timer$gender)

# --- Report ----------------------------------------------------------------
cat("\n")
cat("Participant timing summary -", path, "\n")
cat(strrep("-", 60), "\n\n")

cat("N total:", nrow(timer), "\n\n")

cat("Gender\n")
for (lab in names(sex)) {
  cat(sprintf("  %-8s %3d  (%.1f%%)\n", lab, sex[[lab]], 100 * sex[[lab]] / sum(sex)))
}
cat("\n")

cat("Age (years)\n")
cat(sprintf("  Mean    %6.2f\n", age[["mean"]]))
cat(sprintf("  SD      %6.2f\n", age[["sd"]]))
cat(sprintf("  Median  %6.2f\n", age[["median"]]))
cat(sprintf("  Q1      %6.2f\n", age[["q1"]]))
cat(sprintf("  Q3      %6.2f\n", age[["q3"]]))
cat(sprintf("  IQR     %6.2f   (%.0f - %.0f)\n", age[["iqr"]], age[["q1"]], age[["q3"]]))
cat(sprintf("  Range   %6.0f - %.0f\n", age[["min"]], age[["max"]]))
cat("\n")

cat("Time to complete the experiment (seconds)\n")
cat(sprintf("  Mean    %7.2f   (%s)\n", dur[["mean"]],   as_mmss(dur[["mean"]])))
cat(sprintf("  SD      %7.2f   (%s)\n", dur[["sd"]],     as_mmss(dur[["sd"]])))
cat(sprintf("  Median  %7.2f   (%s)\n", dur[["median"]], as_mmss(dur[["median"]])))
cat(sprintf("  Q1      %7.2f   (%s)\n", dur[["q1"]],     as_mmss(dur[["q1"]])))
cat(sprintf("  Q3      %7.2f   (%s)\n", dur[["q3"]],     as_mmss(dur[["q3"]])))
cat(sprintf("  IQR     %7.2f   (%s)\n", dur[["iqr"]],    as_mmss(dur[["iqr"]])))
cat(sprintf("  Low     %7.2f   (%s)\n", dur[["min"]],    as_mmss(dur[["min"]])))
cat(sprintf("  High    %7.2f   (%s)\n", dur[["max"]],    as_mmss(dur[["max"]])))
cat("\n")

# --- Machine-readable version (handy for pasting into the write-up) --------
summary_df <- data.frame(
  measure = c("age", "duration_sec"),
  n       = c(age[["n"]],    dur[["n"]]),
  mean    = c(age[["mean"]], dur[["mean"]]),
  sd      = c(age[["sd"]],   dur[["sd"]]),
  min     = c(age[["min"]],  dur[["min"]]),
  max     = c(age[["max"]],  dur[["max"]]),
  q1      = c(age[["q1"]],   dur[["q1"]]),
  q3      = c(age[["q3"]],   dur[["q3"]]),
  iqr     = c(age[["iqr"]],  dur[["iqr"]]),
  row.names = NULL
)
print(summary_df, digits = 5)
cat("\n")

invisible(list(summary = summary_df, gender = sex))
