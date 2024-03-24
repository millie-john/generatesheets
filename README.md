# Generate Sheets
Scripts to generate randomized sheets for testing. Packages should auto-install if not already available. The easiest thing to do is select the entire script and run it all in R studio. Sheets will be saved in CSV format in the working directory. For individual sheets, the scripts use a blunt approach of generating a random sheet, testing it to see if it fits the rules, and throwing it out and starting again if it doesn't. For mixing the 3 sets of conditions, the scripts will try to randomly sample and add the line to the final sheet, if it fits the rules it will continue and add another line, if it doesn't it will fail after x iterations and start again as sometimes the script will create an unsolvable puzzle requiring the reset. 

## `singlesheets_20.R`

Generates a single sheet in CSV format.
Sheets should take ~20 seconds to generate.

**v0.1**: Initial version.

**v0.2**: Converted to same code as `mixconditions.R` without the mixing of the three sheets. Will generate 3 sheets and then stop. Added in more robust testing for congruent and non-congruent trials (no more than 2 in a row). Also added `cat` statement to show where files are saved.

## `mixconditions_20.R`

Separate trial sheets are named `Iteration_<number>.csv`. The final sheet with 60 mixed conditions is called `List_60_<number>.csv`.
The number (`<number>`) is the iteration it took to find a sheet that met the criteria.
Sheets will be generated in the folder location you are executing from. You can check where this is using `getwd()`
You can change your working directory with `setwd()`, for example, `setwd("~/Desktop")` would save the files to your desktop (on a Mac).

**v0.1**: Initial code that runs the same code as before but quicker. Run time is approx 30 seconds to generate 3 sheets plus the mixed conditions.

**v0.2**: Added more notes to the code and reordered columns. Run time is approx 30 seconds to generate 3 sheets plus the mixed conditions.

**v0.3**: Now tests for congruent and non-congruent trials. The script will make sure even there is an even distribution (5 LL, 5 RR, 5 LR, 5, RL) for each sheet. Run time is approx 90 seconds to generate 3 sheets plus the mixed conditions.

**v0.4**: Reordered columns on saved 60 trial sheet.

**v0.5**: Added better testing for congruent and non-congruent trials. No more than 2 in a row. Also added `cat` statement to show where files are saved.

## `singlesheets_12.R`

Generates 5 single sheet in CSV format.
Sheets should take ~5 seconds to generate.
