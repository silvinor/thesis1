#!/usr/bin/env python3
"""Clean Gorilla demographics exports: one line per respondent with their
age and gender responses.

Usage:
    python3 demographics.py <input.csv> <output.csv>

Writes the cleaned data to <output.csv>.

Participants are identified by participant_index, assigned the same way as
in clean.py: it starts at 1 and increments whenever the
(Participant Private ID, Participant External Session ID) pair changes
(assumes each participant's rows are contiguous, as in Gorilla exports).
"""

import csv
import sys
from pathlib import Path

# Screen name -> output column; value taken from Response on the
# "Response Type" == "response" row for that screen
SCREEN_COLUMNS = [
    ("age", "age"),
    ("gender", "gender"),
]

OUTPUT_COLUMNS = ["participant_index"] + [out for _, out in SCREEN_COLUMNS]


def session_key(row: dict) -> tuple:
    return (
        row.get("Participant Private ID"),
        row.get("Participant External Session ID"),
    )


def clean_file(input_path: Path, output_path: Path) -> Path:
    with input_path.open(newline="", encoding="utf-8-sig") as fin, \
            output_path.open("w", newline="", encoding="utf-8") as fout:
        writer = csv.DictWriter(fout, fieldnames=OUTPUT_COLUMNS, restval="")
        writer.writeheader()

        total = 0
        current_key = None
        participant_index = 0
        record: dict = {}

        def flush():
            nonlocal total
            # Drop participants who answered neither screen
            if record and any(record.get(out) for _, out in SCREEN_COLUMNS):
                writer.writerow(record)
                total += 1

        for row in csv.DictReader(fin):
            key = session_key(row)
            if key != current_key:
                flush()
                current_key = key
                participant_index += 1
                record = {"participant_index": participant_index}

            if (row.get("Response Type") or "").strip() != "response":
                continue
            screen = (row.get("Screen") or "").strip()
            for src, out in SCREEN_COLUMNS:
                if screen == src:
                    record[out] = row.get("Response", "")
        flush()

    print(f"{input_path.name}: {total} participants -> {output_path.name}")
    return output_path


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 1
    input_path = Path(argv[1])
    if not input_path.is_file():
        print(f"error: file not found: {input_path}", file=sys.stderr)
        return 1
    clean_file(input_path, Path(argv[2]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
