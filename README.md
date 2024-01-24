# Generate Sheets
Scripts to generate randomized sheets for testing.

## `singlesheets.R`

Generates a single sheet in CSV format.
Sheets should take ~20 seconds to generate.

## `mixconditions.R`

Separate trial sheets are named `Iteration_<number>.csv`. The final sheet with 60 mixed conditions is called `List_60_<number>.csv`.
The number (`<number>`) is the iteration it took to find a sheet that met the criteria.
Sheets will be generated in the folder location you are executing from. You can check where this is using `getwd()`
You can change your working directory with `setwd()`, for example, `setwd("~/Desktop")` would save the files to your desktop (on a Mac).

**v0.1**: Initial code that runs the same code as before but quicker. Run time is approx 30 seconds to generate 3 sheets plus the mixed conditions.

**v0.2**: Added more notes to the code and reordered columns. Run time is approx 30 seconds to generate 3 sheets plus the mixed conditions.

**0.3**: Now tests for congruent and non-congruent trials. The script will make sure even there is an even distribution (5 LL, 5 RR, 5 LR, 5, RL) for each sheet. Run time is approx 90 seconds to generate 3 sheets plus the mixed conditions.
