# Disagreement log

This is the record of every place where ArchGrader's score differed from the
hand-derived expected score, and what was done about it. It is deliberately
kept even when short: a log with entries shows the grader was actually tested.

## Protocol

1. The rubric and assignments were written first.
2. The mock submissions were written in `data-raw/mocks.R`, and the expected
   points for every criterion of every mock were derived **by hand** and
   written to `inst/mock_submissions/expected_scores.csv`.
3. The grader was run on the mocks only after that file was frozen.
4. Every difference between the grader and the key is an entry below. For each
   one, a decision is recorded: the grader was wrong (fix it), the key was
   wrong (fix it and say why), or the rubric was ambiguous (clarify it).

Answer key frozen at SHA-256
`d8330a8ec984afaa5aad755c961b537e4e4570065ae1059ac6111d6f34db6bdf`
(`shasum -a 256 inst/mock_submissions/expected_scores.csv` reproduces it).

## Mocks

30 submissions, 10 per assignment: 1 correct, 6 or 7 with a planted mistake,
and 2 or 3 that solve the task by a different valid method (the
false-negative test). Overall 3 correct, 19 mistakes, 8 alternate-valid. `planted_mistakes.csv` records the kind of each mock and the chunk where
a code error should be reported.

## First run

| Measure | Result |
|---|---|
| Submission totals matching the key | 30 / 30 |
| Criterion-level scores matching the key | 190 / 190 |
| Planted code errors reported at the right chunk | 3 / 3 |
| Valid alternate methods accepted in full | 8 / 8 |
| Planted mistakes penalised | 19 / 19 |

**No disagreements on the first run.** Read that with its limit in mind: the
same person wrote the rubric, the mocks and the key, so agreement shows the
grader implements the rubric faithfully, not that the rubric matches what
another instructor would grade. See the README's Limitations section.

## Issues found while building the mocks (before the first run)

These were found by thinking about the mocks, not by the grader disagreeing,
so they are not disagreements, but they changed the product.

1. **A3 template leaked the answer.** The `## Interpretation` section in the
   assignment contained the instructions, and the instructions contained the
   keywords the `text_mentions` check looks for ("effect size", "larger", ...).
   An empty answer would have earned full text credit. The instructions were
   moved to a separate "Task 3: Explain your result" section, leaving only a
   placeholder under `## Interpretation`. Rule recorded in
   `docs/conventions.md`.
2. **YAML `y:` became `TRUE`.** In a plot rubric, the mapping key `y` was read
   as a boolean and silently became the name `"TRUE"`, so the y-axis check
   could never pass. The loader now reads only `true` and `false` as booleans,
   with a test.
3. **Sign of an interval.** Accepting the negative of an interval as-is would
   also accept a nonsensical interval whose ends are in the wrong order. Negated
   intervals are now reversed before comparing.

## Known blind spots (accepted, written into the key)

- **Close p-values pass.** `a3_wrong_test_wilcox` uses a Wilcoxon test. Its p-value
  differs from the t-test's by 8.7e-5, inside the rubric's tolerance of 1e-4,
  so the `p_value` criterion gives full credit. The statistic and method
  criteria still lose points (55 / 100 in total). A tighter tolerance would fix
  this, but would then reject legitimate rounding. The key records the expected
  full `p_value` credit rather than hiding the case.
- **An empty or garbage file earns the `renders` points.** Code that does not
  run cannot fail, so `renders` passes when there is no code. Open design
  decision; see the README.
- **Keyword matching is not understanding.** `text_mentions` gives credit for
  an answer that mentions the right words, including a wrong conclusion that
  uses them. Reasoning quality is a `manual` criterion for that reason.

## Template for future entries

```
### YYYY-MM-DD <submission> / <criterion>
- Grader: <points>   Key: <points>
- Cause: <grader bug | key error | ambiguous rubric>
- Decision: <what changed>
```
