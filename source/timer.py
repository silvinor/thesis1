#!/usr/bin/env python3
"""Extract how long each respondent took on the Gorilla task: one line per
respondent with their first and last timestamp and the elapsed time.

Usage:
    python3 timer.py <input.csv> <demographics.csv> <output.csv>

<demographics.csv> is the output of demographics.py; its age, gender and
timezone values are joined onto each row by participant_index (assumes both
exports hold the same participants in the same order, one session each).
Writes the timings to <output.csv>.

Runs in a single streaming pass, holding only one participant's first/last
timestamp at a time (assumes each participant's rows are contiguous, as in
Gorilla exports).

`Local Timestamp` is a Unix epoch value in milliseconds encoded in UTC (it
matches `UTC Timestamp` in these exports), so the human-readable columns are
rendered in UTC. Add the respondent's `timezone` (hours) to convert to their
own wall-clock time.
"""

import csv
import sys
from datetime import datetime, timezone
from pathlib import Path

# Column holding the epoch (milliseconds) used for the timings
TIMESTAMP_COLUMN = "Local Timestamp"

# Participants to exclude from output, by "Participant External Session ID"
# in the original data. They are still counted toward `index` and
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
    "h_start",
    "h_end",
    "h_duration",
}

OUTPUT_COLUMNS = [
    "index",
    "participant_index",
    "age",
    "gender",
    "timezone",
    "t_start",
    "t_end",
    "t_duration",
    "h_start",
    "h_end",
    "h_duration",
]


def human_time(epoch_ms: int) -> str:
    """Epoch milliseconds -> 'YYYY-MM-DD HH:MM:SS' in UTC."""
    return datetime.fromtimestamp(epoch_ms / 1000, timezone.utc).strftime(
        "%Y-%m-%d %H:%M:%S"
    )


def human_duration(seconds: float) -> str:
    """Elapsed seconds -> 'Hh Mm Ss', dropping empty leading units."""
    total = int(round(seconds))
    hours, remainder = divmod(total, 3600)
    minutes, secs = divmod(remainder, 60)
    if hours:
        return f"{hours}h {minutes}m {secs}s"
    if minutes:
        return f"{minutes}m {secs}s"
    return f"{secs}s"


def numify(record: dict) -> None:
    """Convert non-STRING_COLUMNS values to numbers so QUOTE_NONNUMERIC
    leaves them unquoted."""
    for col in OUTPUT_COLUMNS:
        if col in STRING_COLUMNS:
            continue
        value = record.get(col)
        if isinstance(value, str):
            try:
                record[col] = int(value)
            except ValueError:
                try:
                    record[col] = float(value)
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


def timestamp_of(row: dict):
    """Epoch milliseconds for a row, or None if the row carries no usable
    timestamp (e.g. the trailing 'END OF FILE' marker row)."""
    value = (row.get(TIMESTAMP_COLUMN) or "").strip()
    try:
        return int(value)
    except ValueError:
        return None


def load_demographics(demographics_path: Path) -> dict:
    """participant_index -> {"age": ..., "gender": ..., "timezone": ...}."""
    demographics = {}
    with demographics_path.open(newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            demographics[row["participant_index"]] = {
                "age": row.get("age", ""),
                "gender": row.get("gender", ""),
                "timezone": row.get("timezone", ""),
            }
    return demographics


def time_file(input_path: Path, demographics_path: Path, output_path: Path) -> Path:
    demographics = load_demographics(demographics_path)

    with output_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f, fieldnames=OUTPUT_COLUMNS, restval="", quoting=csv.QUOTE_NONNUMERIC
        )
        # Plain writer for the header so column names stay unquoted
        csv.writer(f).writerow(OUTPUT_COLUMNS)

        total = 0
        written = 0
        current_key = None
        participant_index = 0
        t_start = t_end = None

        def flush():
            nonlocal total, written
            if t_start is None:
                return
            total += 1
            if is_excluded(current_key):
                return
            duration = (t_end - t_start) / 1000
            record = {
                "index": total,
                "participant_index": participant_index,
                "t_start": t_start,
                "t_end": t_end,
                "t_duration": duration,
                "h_start": human_time(t_start),
                "h_end": human_time(t_end),
                "h_duration": human_duration(duration),
            }
            record.update(demographics.get(str(participant_index), {}))
            numify(record)
            writer.writerow(record)
            written += 1

        for row in read_rows(input_path):
            stamp = timestamp_of(row)
            if stamp is None:
                continue

            key = session_key(row)
            if key != current_key:
                flush()
                current_key = key
                participant_index += 1
                t_start = t_end = stamp

            t_start = min(t_start, stamp)
            t_end = max(t_end, stamp)
        flush()

    print(f"{input_path.name}: {written} participants -> {output_path.name}")
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
    time_file(input_path, demographics_path, Path(argv[3]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
