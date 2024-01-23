# Generate Sheets
Scripts to generate randomised sheets for testing.

## `singlesheets.R`

Generates a single sheets in csv format.
Sheets should take ~20 seconds to generate.

## `mixconditions.R`

**v0.2**: Generates separate sheets for conditions 1, 2, and 3, and then mixes conditions into a single list of 60.
Should takes ~40 seconds to generate. Separate sheets are named `Iteration_<number>.csv`. Final sheet with 60 mixed conditions is called `List_60_<number>.csv`.
The number (`<number>`) is the iteration it took to find a sheet that met the criteria.
Sheets will be generated in the folder location you are executing from. You can check where this is using `getwd()`
You can change your working directory with `setwd()`, for example, `setwd("~/Desktop")` would save the files to your desktop (on a mac).
