# ArchGrader

ArchGrader grades student Quarto (`.qmd`) assignments written in R against a
YAML rubric, and writes a plain-language feedback report for each student. It
is deterministic: no LLM, no randomness, so the same rubric and inputs always
produce the same score.

> Work in progress. This README is a stub and will be finished in Phase 5.

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
