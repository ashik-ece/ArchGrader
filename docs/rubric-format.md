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

| Check | Verifies | Parameters |
|---|---|---|
| `renders` | The code runs top to bottom without error | none |
| `value` | A number, vector, or small table is close to the solution's | `object`, `tolerance`; optional `key`, `ignore_sign` |
| `plot` | A saved ggplot has the right geometry, mappings and labels | `object`; at least one of `geom`, `mapping`, `labels` |
| `uses_function` | A required function is called in the student's code | `functions`; optional `mode` (`any` or `all`) |
| `text_mentions` | The written answer covers required ideas | `heading`, `groups` |
| `manual` | Needs human judgment; listed for TA review, not scored | none |

### `value`

- `object`: the name the student must save the answer under.
- `tolerance`: required, absolute. Numbers within it of the solution's count
  as correct. Use `0` for counts.
- `key`: for tables, the column(s) that identify a row (for example `site`).
  Rows are matched on the key, so a different row order still earns credit.
  Without a key, rows are compared by position.

- `ignore_sign`: optional (default `false`). A number is accepted if it matches
  the solution's or its negative. An interval is accepted as given or negated
  **and reversed**, so `3.5 to 9.6` also matches `-9.6 to -3.5`, but
  `-3.5 to -9.6` does not. Use it where the order of subtraction is the
  student's choice.

Credit for a table is the share of non-key cells that match, so one wrong
column of four out of twelve cells earns 8/12 of the points. A missing object
or an object left as `NULL` earns zero, and the hint names the object.

### `plot`

Inspects the saved ggplot object; it never looks at the picture.

- `object`: the plot's name.
- `geom`: a geom such as `point` or `line` (or a list of acceptable ones). Credit if a layer of that type exists.
- `mapping`: aesthetic to column, for example `{x: length_mm, y: width_mm}`.
  The aesthetic may be set on the plot or on a layer.
- `labels`: any of `x`, `y`, `title`, `subtitle`, `caption`, `colour`, `fill`.
  A label counts only if it differs from the default (the variable name), so a
  plot that still shows `length_mm` on its axis does not earn the label credit.

Credit is the average over the parts asked about, so a plot with the right
geometry but a wrong axis earns part of the points.

### `uses_function`

`functions` lists names; `mode: any` (default) needs one of them, `mode: all`
needs every one. The code is parsed and the function names are read from it, so
a name in a comment or a string does not count. Both `stats::t.test(...)` and
`t.test(...)` are matched as `t.test`. Use this sparingly; it is the one check
that looks at *how* the student worked rather than what they produced.

### `text_mentions`

`heading` is text to find in a markdown heading (case-insensitive, any heading
level, the first heading that contains it); the section runs until the next
heading of the same or higher level. `groups` maps a group name to a list of keywords. A group is met when
any of its keywords appears (case-insensitive, plain text, not a regular
expression); credit is the share of groups met. This is keyword matching, not
understanding, which is why reasoning quality is a separate `manual`
criterion. A missing heading or an empty section scores zero, and the note says which.

### YAML booleans

YAML 1.1 reads a bare `y`, `n`, `yes`, `no`, `on` or `off` as a boolean. The
loader turns that off: only `true` and `false` are booleans, so an aesthetic
key `y:` or a keyword `[yes, no]` stays text.

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
