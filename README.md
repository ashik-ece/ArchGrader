# ArchGrader

ArchGrader grades student Quarto (`.qmd`) assignments written in R against a
YAML rubric, and writes a plain-language feedback report for each student. It
is deterministic: no LLM, no randomness, so the same rubric and inputs always
produce the same score.

> Work in progress. This README is a stub and will be finished in Phase 5.

## Grade one submission

```r
devtools::load_all()   # or library(ArchGrader) once installed
grade_submission("inst/mock_submissions/a1/a1_partial_table.qmd", assignment = "a1")
#> ArchGrader: a1_partial_table | Assignment 1: Summary Statistics of Lithic Artifacts
#>  criterion         status  credit points
#>  renders           pass    1.00   10.00/10
#>  ...
#>  summary_table     partial 0.67   16.67/25
#>
#> Score: 91.67 / 100 (91.67%)
```

The result also holds the per-criterion notes (observed versus expected
values and the chunk to revisit). See [`docs/rubric-format.md`](docs/rubric-format.md)
for how a rubric is written.

## Grade a folder

```r
grade_folder("path/to/submissions", assignment = "a1", output_dir = "grading-output")
#> ArchGrader batch summary
#>   Graded:         8 submissions
#>   Average score:  52.71 / 100 (52.7%)
#>   With errors:    1
#>   Total time:     8 s (median 0.93 s per submission)
#>   Scores:         grading-output/scores.csv
#>   Reports:        grading-output/reports/
```

This writes `scores.csv` (student ID, total, one column per criterion, error
flag, failing chunk, seconds) and one self-contained HTML feedback report per
student in `reports/`. A crashing, looping or unreadable submission becomes a
normal row and report; it never stops the batch. Re-running the same folder
gives an identical CSV apart from the `seconds` column.

Feedback reports are rendered with Quarto, which must be installed and on the
`PATH` (use `reports = FALSE` to skip them). Feedback is assembled from the
rubric's comments, not generated, so identical mistakes get identical feedback.

## Folder layout

| Folder | Contents |
|---|---|
| `R/` | Package code (runner, rubric reader, checks, scorer, batch runner) |
| `inst/assignments/` | Assignment `.qmd` files, instructor solutions, and the dataset |
| `inst/rubrics/` | One YAML rubric per assignment |
| `inst/templates/` | Quarto template for the feedback report |
| `inst/mock_submissions/` | Author-written mock submissions and `expected_scores.csv` |
| `data-raw/` | Script that generates the synthetic dataset |
| `docs/` | Naming conventions and instructor notes |
| `tests/testthat/` | Unit tests |

## Requirements

R (4.6 used in development), Quarto, and the R packages in `DESCRIPTION`
(`callr`, `knitr`, `yaml`, `testthat`, `dplyr`, `ggplot2` as they are needed).

See [`docs/conventions.md`](docs/conventions.md) for the object-naming
convention every assignment follows.

## License

MIT
