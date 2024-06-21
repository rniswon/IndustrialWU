Steps for adding a new state:

1)  Source `_targets.R`

2)  To incorporate a new state, start with `DataCrosswalks/DataDirectories.csv`. Files are listed in the first column. For each file, designate whether the file contains any of the following: `SiteDescriptions`, `LocationInfo`, `MonthlyData`, `AnnualData`, `Metadata`. If the file does, or may, contain such info, replace NA in that column with 1. If it does not, replace NA with 0. If the file is redundant to other files in the directory, or if the file is not directly relevant, you can place a 1 in the `Duplicate` or `NotRelevant` column, and 0 in everything else. 1 can be placed in more than one of the columns with the 5 types of useful data. All NAs in the row should be replaced by a 0 or 1: leaving an NA is an indication that the file has not fully been dealt with yet, and duplicate lines could be added to `DataDirectories.csv` in the future if a line is saved with only a few NAs replaced.

3)  Run tar_make(). This will update the `DataCrosswalks/HeaderCrosswalk.csv` file to include lines for the files you have just handled in the `DataDirectories.csv` file. All excel files need to be closed when tar_make() is run.

4)  Update the `DataCrosswalks/HeaderCrosswalk.csv` lines for the files that are being handled. Data crosswalks do not include any data munging that may be applied to the columns during formatting - they just identify which column the data is coming from. In cases where data must be pivoted, enter `~PIVOT~`, and that tells the script that more information is needed that you will have to enter later. A few tips for filling out the HeaderCrosswalk.csv file:

    1)  Enter the name of the column as it appears in the raw data file. The name does not have to be in quotations

    2)  Not all entries in `HeaderCrosswalk.csv` will correspond to a column for a given file. If there is no relevant column, still delete 'NA'. As with `DataDirectories.csv`, having NAs indicates that the file has not been handled yet, and it is skipped by the formatting script.

    3)  For information that comes from .docx files (such as ReadMes), the header can be "text".

    4)  Currently, the code is very sensitive to case and spelling. Be sure that the headers are entered correctly.

5)  Run tar_make() this will use the input data crosswalks to format the new state's data. There are several checks built into the script that should prompt the developer to make further updates to the csv control files if necessary, but the developer should also carefully look at the output for the new state as well, as there may be new cases of data formatting that need to be dealt with in the code. The commented out code at the bottom of the `_targets.R` file are there to help with the debugging. All excel files must be closed for tar_make() to run.

6)  If necessary, update `DataCrosswalks/DataPivots.csv`. The entries to this spreadsheet will be used to generate dplyr::pivot() lines of code in the `readandrename_columns` function.

7)  If necessary, update `DataCrosswalks/DataCodesCrosswalk.csv` with new data assignments. Error messages are included in the code to help the developer identify the new codes. For this file, the first column is the header of the formatted data column that has the new codes. The second column is the old code, and the third code is the value that should be used instead.

8)  If prompted, update `DataCrosswalks/HardcodedManualAttributes.csv`. This will usually happen if a value cannot be reliably extracted through the code already written. You should get a pop-up window to enter the value that should be used for the attribute in the case of this file. If necessary, this file can also be edited manually in excel or notepad, or the like.

9)  Run tar_make() again.

When new data files are added to the state_data folder, new lines will be added to `DataCrosswalks/DataDirectories.csv`, with NA entered for columns `SiteDescriptions`, `LocationInfo`, `MonthlyData`, `AnnualData`, `Metadata`, `Duplicate`, and `NotRelevant`. This process can be repeated for the new entries.
