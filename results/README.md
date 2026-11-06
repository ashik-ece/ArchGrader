# Results folder

| File | Made by | Contents |
|---|---|---|
| `summary.md` | `Rscript data-raw/make_summary.R` | Agreement, criteria automated, timing, coverage. Quoted in the README. |
| `mock_scores.csv` | the same script | One row per mock and criterion: expected points, graded points, agree. |
| `hand_grading_times.csv` | you, by hand | Stopwatch minutes for ten mocks. Blank until recorded. |

Never edit `summary.md` or `mock_scores.csv` by hand; rerun the script.

## Hand-grading stopwatch protocol

The point is an honest answer to "how long does this take without the tool?",
so the protocol is fixed in advance.

1. Do it in one sitting, with the ten submissions in `hand_grading_times.csv`
   in a shuffled order. Do not look at ArchGrader's scores or reports for them
   before you finish.
2. Use only what a grader would have: the assignment, the instructor solution,
   and the rubric in `inst/rubrics/`.
3. For each submission, start the stopwatch when you open the `.qmd` and stop it
   when you have written down, for every rubric criterion, a score and, where
   points were lost, one or two sentences of feedback. That matches what
   ArchGrader's report contains.
4. Run the code yourself where you need to; that time counts.
5. Enter the minutes (decimals are fine, for example `6.5`) in the `minutes`
   column. Leave a row blank rather than guess.
6. Run `Rscript data-raw/make_summary.R`. The "Against grading by hand" section
   fills in.

Known bias to state when quoting the result: the author wrote these mocks and
knows where the planted mistakes are, which makes hand grading look faster
than it would be for an unfamiliar batch.
