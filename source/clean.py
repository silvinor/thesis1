#!/usr/bin/env python3
"""Clean Gorilla task exports: consolidate per-trial rows into one line per
respondent per trial.

Usage:
    python3 clean.py <input.csv> <demographics.csv> <output.csv>

<demographics.csv> is the output of demographics.py; its age and gender
values are joined onto each trial row by participant_index (assumes both
exports hold the same participants in the same order, one session each).
Writes the cleaned data to <output.csv>.

Runs in two streaming passes so memory use stays small no matter how many
participants the file holds:
  pass 1 collects each participant's propensity response (one value each);
  pass 2 consolidates trial rows, holding only one participant's trials in
  memory at a time (assumes each participant's rows are contiguous, as in
  Gorilla exports).
"""

import csv
import sys
from pathlib import Path

# Input column -> output column
SPREADSHEET_COLUMNS = [
    ("Spreadsheet: image_set", "image_set"),
    ("Spreadsheet: noisiness", "noisiness"),
    ("Spreadsheet: oddity", "oddity"),
    ("Spreadsheet: quadrant", "quadrant"),
]
# Tags whose Response value becomes its own output column
RESPONSE_TAGS = ["expectedness", "comfortable"]

# Participants to exclude from output, by "Participant External Session ID"
# in the original data. Their trials are still counted toward `index` and
# `participant_index` so the surviving rows keep the numbering they would
# have had if these participants were merely removed after the fact (i.e.
# the sequence skips the excluded values rather than closing the gap).
EXCLUDED_PARTICIPANTS = [
    "6a55f1d8045ad75a5b407efe",
]

# Columns written as quoted strings; every other column is written as a
# number (unquoted) via QUOTE_NONNUMERIC where its value allows.
STRING_COLUMNS = {
    "gender",
    "expectedness",
    "comfortable",
    "propensity",
    "noisiness",
    "oddity",
    "c_tmzone",
}

# Source column -> codified column, derived from the value's leading digit
# (kept to 8 chars for SPSS/PROCESS field-name limits)
CODIFIED_COLUMNS = [
    ("expectedness", "c_expect", lambda n: n),
    ("comfortable", "c_discom", lambda n: n),
    ("noisiness", "c_noise", lambda n: n),
    ("oddity", "c_oddity", lambda n: n),
    ("propensity", "c_propen", lambda n: n),
]

OUTPUT_COLUMNS = (
    ["index", "participant_index", "age", "gender", "timezone", "trial_number"]
    + RESPONSE_TAGS
    + ["propensity"]
    + [out for _, out in SPREADSHEET_COLUMNS]
)
OUTPUT_COLUMNS.insert(OUTPUT_COLUMNS.index("image_set") + 1, "image_set_repeat")
OUTPUT_COLUMNS += [out for _, out, _ in CODIFIED_COLUMNS] + ["c_oddnse", "c_tmzone"]


def leading_digit(value: str):
    value = (value or "").strip()
    return int(value[0]) if value[:1].isdigit() else None


def classify_timezone(value: str) -> str:
    value = (value or "").strip()
    try:
        n = float(value)
    except ValueError:
        return ""
    if n < -1:
        return "US"
    if n <= 3:
        return "UK"
    return "AU"


def codify(record: dict) -> None:
    for src, out, rule in CODIFIED_COLUMNS:
        n = leading_digit(record.get(src, ""))
        record[out] = rule(n) if n is not None else ""
    record["c_oddnse"] = 1 if record["c_oddity"] == 1 and record["c_noise"] == 1 else 0
    record["c_tmzone"] = classify_timezone(record.get("timezone", ""))


def numify(record: dict) -> None:
    """Convert non-STRING_COLUMNS values to int so QUOTE_NONNUMERIC leaves
    them unquoted."""
    for col in OUTPUT_COLUMNS:
        if col in STRING_COLUMNS:
            continue
        value = record.get(col)
        if isinstance(value, str):
            try:
                record[col] = int(value)
            except ValueError:
                pass


def session_key(row: dict) -> tuple:
    return (
        row.get("Participant Private ID"),
        row.get("Participant External Session ID"),
    )


def is_excluded(key: tuple) -> bool:
    return key is not None and key[1] in EXCLUDED_PARTICIPANTS


def read_rows(input_path: Path):
    with input_path.open(newline="", encoding="utf-8-sig") as f:
        yield from csv.DictReader(f)


def collect_propensities(input_path: Path) -> dict:
    """Pass 1: propensity response per (participant_id, session_id)."""
    propensities = {}
    for row in read_rows(input_path):
        if (
            (row.get("Display") or "").strip() == "propensity"
            and (row.get("Response Type") or "").strip() == "response"
        ):
            propensities[session_key(row)] = row.get("Response", "")
    return propensities


def load_demographics(demographics_path: Path) -> dict:
    """participant_index -> {"age": ..., "gender": ...} from demographics.py output."""
    demographics = {}
    with demographics_path.open(newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            demographics[row["participant_index"]] = {
                "age": row.get("age", ""),
                "gender": row.get("gender", ""),
                "timezone": row.get("timezone", ""),
            }
    return demographics


def clean_file(input_path: Path, demographics_path: Path, output_path: Path) -> Path:
    demographics = load_demographics(demographics_path)
    propensities = collect_propensities(input_path)

    with output_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f, fieldnames=OUTPUT_COLUMNS, restval="", quoting=csv.QUOTE_NONNUMERIC
        )
        # Plain writer for the header so column names stay unquoted
        csv.writer(f).writerow(OUTPUT_COLUMNS)

        total = 0
        current_key = None
        participant_index = 0
        trials: dict[str, dict] = {}  # trial number -> consolidated record
        set_counts: dict[str, int] = {}  # image_set -> occurrences so far this participant

        def flush():
            nonlocal total
            excluded = is_excluded(current_key)
            propensity = propensities.get(current_key, "")
            for record in trials.values():
                total += 1
                if excluded:
                    continue
                record["index"] = total
                record["propensity"] = propensity
                record.update(demographics.get(str(record["participant_index"]), {}))
                codify(record)
                numify(record)
                writer.writerow(record)
            trials.clear()

        for row in read_rows(input_path):
            tag = (row.get("Tag") or "").strip()
            if tag not in RESPONSE_TAGS:
                continue

            key = session_key(row)
            if key != current_key:
                flush()
                current_key = key
                participant_index += 1
                set_counts.clear()

            trial = row.get("Trial Number", "")
            record = trials.get(trial)
            if record is None:
                record = {"participant_index": participant_index, "trial_number": trial}
                for src, out in SPREADSHEET_COLUMNS:
                    record[out] = row.get(src, "")
                set_counts[record["image_set"]] = set_counts.get(record["image_set"], 0) + 1
                record["image_set_repeat"] = set_counts[record["image_set"]]
                trials[trial] = record
            record[tag] = row.get("Response", "")
        flush()

    print(f"{input_path.name}: {total} trials -> {output_path.name}")
    return output_path


def main(argv: list[str]) -> int:
    if len(argv) != 4:
        print(__doc__.strip(), file=sys.stderr)
        return 1
    input_path, demographics_path = Path(argv[1]), Path(argv[2])
    for path in (input_path, demographics_path):
        if not path.is_file():
            print(f"error: file not found: {path}", file=sys.stderr)
            return 1
    clean_file(input_path, demographics_path, Path(argv[3]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
