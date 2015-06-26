# PNOIT
Scripts for PNOIT and related projects

PNOIT1_BAT_Paths.R lists the Buhlmann test files for PNOIT1 (using the 965_zipped folder on the remote drive) and prints a RedCap-ready .csv and a .csv for oddly-named files that need hand-editing.

Future directions for BAT_Paths:
-We can basically use this script to enter any file paths into RedCap.

PhadiaReader takes all Phadia IgE .csv files in a directory and prints a .csv ready to upload to RedCap.
6/26: Currently having issues with two files that for some reason don't read into R the same way as the rest... and as far as I can tell, it's the majority that are behaving unexpectedly, which is making it very very frustrating to fix.
The Phadia100's .csv files show up in Excel with the column names shifted one to the left, so the concentration is listed under "quot" instead of "conc", etc.
When I read most of them into R, it automatically labels the first column as "row numbers" and shifts all the column names to the right.
Under ANY OTHER CIRCUMSTANCES this would ruin your column names and you would want it to stop doing that, but it works out for me here.
For two of the files (so far), it inserts an entire new column for row numbers and the columns remained matched with their original names, which conflicts with the rest of my script.
The files are IgE-20150129.csv and IgE_20150424.csv and I can't figure out what makes them so special.

Future directions for the Phadia Reader:
-Once the RedCap databases for other projects are finalized, I can add to this script to pull out their IgE data as well.
-IgG, IgG4, IgA, etc, will require seperate scripts. It should be a matter of changing some names in this script.
