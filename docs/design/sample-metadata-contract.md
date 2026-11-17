# Sample metadata contract (v0)

Status: **draft** — for discussion; not an API commitment yet.

Author: Thanadol Sutantiwani  
Last updated: 2026-11-17

## Goal

One sentence: what should a user be able to do with this in v0?

> _e.g. Read a sample metadata file, validate required fields, and return a tidy tibble ready to join to NPX on SampleID._

## Steps (sugested)

1. Write the contract first (½–1 day, no code)
Decide: input format (TSV/CSV?), required columns, how it joins NPX (SampleID?), and what the first user-facing function should do (read? validate? annotate NPX?). One short design note beats jumping into R/.

2. Learn the local patterns by reading 2–3 files
Mirror how this package already does I/O + checks:

- R/read_npx.R (+ a read_npx_*.R) — file → parse → return tibble
- R/check_*.R — validation + cli::cli_abort
- Matching tests/testthat/test-*.R

3. Build the smallest vertical slice
Suggested first PR shape:
- check_*() for required columns / types
- read_*() that reads a file and runs those checks
- 1 fixture under inst/extdata/ or tests/testthat/
- tests for happy path + 1–2 clear failures

4. Only then join to NPX / export / richer metadata features.


## Background / naming

In OlinkAnalyze today, `manifest` / `olink_plate_randomizer()` means a **plate-layout sample map** (`SampleID`, `SubjectID`, `Visit`, `Site`). That stays as-is.

This work is about **sample / study metadata** (possibly SDRF-oriented). Prefer new names such as `read_sdrf` / `check_sdrf` (final names TBD) so we do not overload `manifest`.

## Input

| Item | Decision (fill in) |
|------|--------------------|
| File format | tsv / csv / other? |
| Encoding / separator | |
| Source | user-provided / Olink export / SDRF template? |
| Example fixture | path once we add one under `inst/extdata` or `tests/testthat` |

## Required columns

| Column | Required? | Type | Notes / allowed values |
|--------|-----------|------|------------------------|
| SampleID | yes | character | must be joinable to NPX `SampleID` |
| | | | |
| | | | |

## Optional columns

| Column | Type | Notes |
|--------|------|-------|
| | | |

## Relationship to NPX

- Join key:
- One row per: sample / sample×assay / other?
- Behavior when metadata has SampleIDs missing from NPX:
- Behavior when NPX has SampleIDs missing from metadata:
- Duplicate SampleIDs in metadata: error / warn / allow?

## Out of scope for v0

- Changing existing `manifest` / plate randomization APIs
- Writing / exporting SDRF (unless we decide otherwise)
- Full ontology / controlled-vocabulary enforcement (unless required)

## First public functions (proposed)

1. `check_*(df)` — validate columns / types; fail clearly with `cli`
2. `read_*(file)` — read file → run checks → return tibble
3. Later: join / annotate NPX (not required for first PR)

## Minimal example (paste real or synthetic rows)

```text
SampleID	...
S1	...
S2	...
```

## Open questions for Olink reviewers

1.
2.
3.

## References

- SDRF / related specs (links TBD)
- Related internal docs or tickets (links TBD)
