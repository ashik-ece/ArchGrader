# ArchGrader for instructors

This guide is for the person who will actually run the grader: setting it up,
grading a batch, reading the output, and changing or adding assignments. It
assumes you can run an R command but not that you know the package's insides.

## What it does, and what it does not

For each student's Quarto (`.qmd`) file, ArchGrader runs the code, compares the
results the student saved against your own solution, applies your rubric, and
writes a short feedback report. The same file and rubric always give the same
score. There is no AI judging; every comment in a report is text you wrote in
the rubric.

It grades **results**, not style or method. If the task says "save the mean as
`mean_length`", any way of computing the mean earns the points. It does not
grade reasoning quality: a written explanation is checked for the ideas you
name, and a person should still read it (use a `manual` criterion for that).

## One-time setup

1. Install R, and [Quarto](https://quarto.org/docs/get-started/). Quarto
   must be on the `PATH` (`quarto --version` works in a terminal); it is used
   to render the feedback reports.
2. Get the code and its dependencies:

   ```sh
   git clone https://github.com/ashik-ece/ArchGrader.git
   cd ArchGrader
   Rscript -e 'install.packages("devtools"); devtools::install_deps(dependencies = TRUE); devtools::install()'
   ```

3. Check that it works on the sample submissions:

   ```sh
   Rscript -e 'ArchGrader::grade_folder("inst/mock_submissions/a1", "a1")'
   ```

   This should print a summary of 10 graded submissions and create
   `grading-output/`.

## Grading a batch

1. Put the submissions in one folder: one `.qmd` per student, named so the
   file name identifies the student (`jsmith.qmd`, `12345.qmd`). The file name
   without `.qmd` becomes the student ID in the scores and reports.
2. Run:

   ```r
   library(ArchGrader)
   grade_folder("submissions/a1", assignment = "a1", output_dir = "grading-output")
   ```

3. Open `grading-output/scores.csv` for the marks (one row per student, one
   column per criterion) and give each student their file from
   `grading-output/reports/`.

A submission that crashes, loops forever, or cannot be read still gets a row and
a report; it never stops the batch. Submissions are stopped after the
rubric's `timeout` (30 seconds by default).

### Reading `scores.csv`

| Column | Meaning |
|---|---|
| `student_id` | File name without `.qmd` |
| `total` | Points earned on the automatically graded criteria |
| one per criterion | Points earned on that criterion |
| `error` | `TRUE` if the code stopped with an error or timed out |
| `error_chunk` | The chunk that failed, so you can look there first |
| `seconds` | How long the student's code took to run |

`manual` criteria are left out of `total`; each report says how many points a
person still needs to mark.

### Things to look at yourself

- **Every `error` row**: a student whose code stops partway can still earn
  credit for what ran before the error, which is usually what you want, but
  check that the partial credit seems fair.
- **A handful of reports** before you hand them all out, especially for the
  first run of a new assignment.
- **Totals that look too high or too low** compared with the class. The grader
  is faithful to the rubric, so a surprising pattern usually means the rubric
  or the task wording is ambiguous.

## Running student code safely

ArchGrader runs each submission in a fresh R process in a temporary folder with
a time limit. That keeps one submission from affecting another and stops
infinite loops. **It is not a security sandbox.** Student code can still read
files, use the network, or run system commands as your user. Grade only
submissions from people you trust, or run the grader inside a container or a
throwaway virtual machine.

## Changing a rubric

Everything about a criterion is in the YAML file in `inst/rubrics/`: the
points, the tolerance, and the feedback wording. You never edit R code to
change what a criterion is worth or says. The full format is in
[`rubric-format.md`](rubric-format.md). The rubric is validated when it loads,
and a typo produces a readable message naming the criterion and the problem.

The most common edits:

| I want to... | Change |
|---|---|
| Make a numeric check stricter or looser | `tolerance` (absolute; `0` for counts) |
| Accept the difference taken in either order | add `ignore_sign: true` |
| Give partial credit for a table with some wrong columns | nothing; it is automatic |
| Reword the feedback | `comments` (`pass`, `partial`, `fail`) and `hint` |
| Have a person mark something | `check: manual` (no comments needed) |

Points must add up to `total_points`, or the rubric is rejected.

## Adding an assignment

ArchGrader grades work that is shaped for it, so a new assignment needs five
things. Follow the existing ones in `inst/assignments/` and see
[`conventions.md`](conventions.md) for the rules.

1. **The assignment `.qmd`.** Each task names the object students must create,
   in bold code, and says its type and columns. Chunks have labels in the
   header form (` ```{r load-data} `), not `#| label:`. Keep any written answer
   under its own heading, with the instructions in a different section (see the
   Assignment 3 note in `conventions.md`).
2. **Your solution `.qmd`**, which runs cleanly and creates every object.
3. **The rubric**, one criterion per thing you want to check.
4. **Any data file** the code reads (`read.csv("data.csv")`).
5. **Mock submissions with answers decided in advance**: a correct one, one or
   two valid alternative approaches, and one broken copy per criterion. Write
   down the points each should get *before* running the grader. This is how you
   find out whether the rubric does what you meant. `data-raw/mocks.R` shows
   the approach.

To grade it:

```r
grade_folder("submissions", assignment = "a4",
             rubric = "path/to/a4.yaml",
             solution = "path/to/a4_solution.qmd",
             data_dir = "path/to/data-folder",
             output_dir = "grading-output")
```

With your own rubric you must pass `solution` and `data_dir` too; the defaults
point at the assignments bundled with the package.

## When something looks wrong

| You see | Likely cause | Try |
|---|---|---|
| Error mentioning the instructor solution | Your solution does not run, or its data file is missing | Render your solution yourself; check `data_dir` |
| Error naming a rubric field | A typo or missing field in the YAML | The message lists every problem; fix them all and rerun |
| "Quarto was not found" | `quarto` is not on the `PATH` | Run `quarto --version`; add Quarto to the `PATH`, or set `QUARTO_PATH` in `~/.Renviron`, or use `reports = FALSE` to skip reports |
| A student has 0 for an object they clearly made | They named it differently, or never assigned it with `<-` | The report's hint names the object it expected |
| A student's plot scores 0 | The plot was drawn but not saved to the named object | Same as above |
| A good answer scored low | The answer is correct but outside the tolerance | Widen `tolerance`, or add `ignore_sign: true` |
| A wrong answer scored high | The tolerance is too loose, or the keyword check is too easy | Tighten it, and add a `manual` criterion where judgment matters |

## What has been tested, and what has not

The grader was tested against 30 mock submissions across three assignments,
with the expected score for every criterion written before the grader ran. It
agreed on every one (see [`../results/summary.md`](../results/summary.md) and
[`disagreement-log.md`](disagreement-log.md)). Those mocks were written by the
same person who wrote the rubrics, so this shows the grader does what the
rubric says, not that the rubric matches your judgment. **Run it on a few real
submissions you have already graded by hand and compare** before relying on it
for marks.
