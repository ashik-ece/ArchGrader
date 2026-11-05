# Rubric format

One YAML file per assignment in `inst/rubrics/`. The rubric holds everything
the grader needs to know: what to check, how many points it is worth, the
tolerance, and what to tell the student. Changing a criterion, its points, or
its wording never means touching R code.

The rubric is validated on load. An unknown field, a typo in a check type, a
missing comment, a non-positive point value, or points that do not add up to
`total_points` stops grading with a readable message that lists every problem
found.

## Top level

| Field | Required | Meaning |
|---|---|---|
| `assignment` | yes | Short name, such as `a1` |
| `title` | yes | Shown in the report |
| `solution` | yes | File name of the instructor solution in `inst/assignments/` |
| `total_points` | yes | Must equal the sum of the criteria's points |
| `timeout` | no | Seconds allowed per submission (default 30) |
| `criteria` | yes | List of criteria |

## Criterion

| Field | Required | Meaning |
|---|---|---|
| `id` | yes | Unique; letters, digits, underscores. Becomes a column in `scores.csv` |
| `description` | yes | One line, shown in the report table |
| `points` | yes | Positive number |
| `check` | yes | One of the check types below |
| `params` | per check | Parameters for the check |
| `comments` | yes (not `manual`) | `pass`, `partial` and `fail` text |
| `hint` | yes (not `manual`) | Shown when points were lost |

Comments and hints may use these placeholders, which the grader fills in:
`{object}` (the result object's name), `{observed}` (what the student got),
`{expected}` (the solution's value), and `{chunk}` (the chunk to revisit).
Any other `{word}` is rejected, so a typo cannot silently change feedback.

## Check types

| Check | Verifies | Parameters | Status |
|---|---|---|---|
| `renders` | The code runs top to bottom without error | none | implemented |
| `value` | A number, vector, or small table is close to the solution's | `object`, `tolerance`, optional `key` | implemented |
| `manual` | Needs human judgment; listed for TA review, not scored | none | implemented |
| `plot` | A ggplot has the right geometry and labels | | Phase 4 |
| `uses_function` | A required function appears in the code | | Phase 4 |
| `text_mentions` | The written interpretation covers required ideas | | Phase 4 |

### `value`

- `object`: the name the student must save the answer under.
- `tolerance`: required, absolute. Numbers within it of the solution's count
  as correct. Use `0` for counts.
- `key`: for tables, the column(s) that identify a row (for example `site`).
  Rows are matched on the key, so a different row order still earns credit.
  Without a key, rows are compared by position.

Credit for a table is the share of non-key cells that match, so one wrong
column of four out of twelve cells earns 8/12 of the points. A missing object
or an object left as `NULL` earns zero, and the hint names the object.

## Scoring

Score = sum of `points x credit`, rounded to two decimals. `manual` criteria
carry no automatic points; they appear as "needs review" and their points are
reported separately. The percentage is earned points over automatically
scorable points.

## Example

```yaml
assignment: a1
title: "Assignment 1: Summary Statistics of Lithic Artifacts"
solution: a1_solution.qmd
total_points: 25

criteria:
  - id: median_by_site
    description: "Median length for each site"
    points: 25
    check: value
    params:
      object: median_by_site
      tolerance: 0.01
      key: site
    comments:
      pass: "Correct: `{object}` has the right median for every site."
      partial: "`{object}` is partly right: some sites are wrong or missing."
      fail: "`{object}` does not match the expected table."
    hint: "Group by `site` and take the median of `length_mm`. Check chunk `{chunk}`."
```
