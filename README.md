Steps for adding a new state:

1)  Source `_targets.R`

2)  Update `DataCrosswalks/DataDirectories.csv`

    1)  To incorporate a new state, start with `DataCrosswalks/DataDirectories.csv`. Files are listed in the first column. For each file, designate whether the file contains any of the following: `SiteDescriptions`, `LocationInfo`, `MonthlyData`, `AnnualData`, `Metadata`. If the file does, or may, contain such info, replace NA in that column with 1. If it does not, replace NA with 0. If the file is redundant to other files in the directory, or if the file is not directly relevant, you can place a 1 in the `Duplicate` or `NotRelevant` column, and 0 in everything else. 1 can be placed in more than one of the columns with the 5 types of useful data. All NAs in the row should be replaced by a 0 or 1: leaving an NA is an indication that the file has not fully been dealt with yet, and duplicate lines could be added to `DataDirectories.csv` in the future if a line is saved with only a few NAs replaced.

3)  Run tar_make().

    1)  This will update the `DataCrosswalks/HeaderCrosswalk.csv` file to include lines for the files you have just handled in the `DataDirectories.csv` file. All excel files need to be closed when tar_make() is run.

4)  Update `DataCrosswalks/HeaderCrosswalk.csv`

    1)  Not all entries in `HeaderCrosswalk.csv` will correspond to a column for a given file. If there is no relevant column, still delete 'NA'. As with `DataDirectories.csv`, having NAs indicates that the file has not been handled yet, and it is skipped by the formatting script.

    2)  For shapefiles, I've just entered the headers into the line for the .shp file and have left the rest blank (e.g. .prj, .dbf files)

    3)  For information that comes from .docx files (such as ReadMes), the header can be "text".

    4)  When data files are "tidy", meaning that headers are on the top line, and data are below in tabular format, enter the name of the column as it appears in the raw data file without quotations around it. Currently the code is very sensitive to case and spelling. Be sure that the headers are entered correctly.

        1)  In some cases there are multiple columns that can be combined (such as `Description`, `NAICS`, or `SIC`. Headers from the raw data can be separated with a comma.

    5)  When data files are not "tidy", there are multiple instruction entries you can include in `HeaderCrosswalk.csv`. These are `~FORM~`, `~PIVOT~`, `~FILL~`, and `~BLANK~`. These instructions can be used in conjunction with each other if necessary.

        1)  `~FORM~` is used when the raw data is not formatted with one line of headers and data below, for example if there are multiple lines of metadata above the data, or if the data appear to the right of the headers. There still likely will be headers for the data, but they may be located in various locations relative to the data. In `HeaderCrosswalk.csv`, enter the header that indicates the data as `~FORM~Header`.

            1)  For example, if data for facility numbers were provided left to right as such:

                |            |           |
                |------------|-----------|
                | Permit ID: | 123456789 |

                the entry in `HeaderCrosswalk.csv` for the `FacilityNumber` column would be `~FORM~Permit ID:`.

        2)  `~PIVOT~` is used when the raw data is tabular, but `HeaderCrosswalk.csv` expects the data to be either long or wide, and the raw data is the opposite.

            1)  For example, `HeaderCrosswalk.csv` expects monthly data to have values in long format by year (i.e. each year is on a separate row), but wide format by month (i.e. each month has a separate column), and the raw data may not be in this format.

            2)  To instruct the code to pivot the data, simply enter `~PIVOT~` into `HeaderCrosswalk.csv` in the column that represents the data that comes out of a pivot. The `~PIVOT~` instructs the code to read additional instructions from `DataPivots.csv`. Instructions for updating `DataPivots.csv` are given later in this ReadMe.

            3)  `~FORM~~PIVOT~` is a valid entry if the raw data requires it.

        3)  `~FILL~` instructs the code to fill the data down a column, such as the Fill Down action in excel would do. This is entered as `~FILL~Header`.

        4)  `~BLANK~` may be used in conjunction with `~FORM~` to indicate that there is not a header for a type of data, but the data is in a specific place. Because this code is still being developed, the use of `~BLANK~` is likely unstable and may not work if it is needed for more than one column.

5)  Save and close all crosswalk files. Run tar_make() again.

    1)  This will generate any forms that need to be filled out, and will hopefully provide useful error messages in accomplishing any of the next steps that may be necessary.

    2)  It is likely that tar_make() will need to be run many times to work through a list of "errors" that are designed to walk through all of the necessary additional steps.

6)  The following errors are designed to help set up the additional crosswalk files. Errors other than these may indicate bugs in R functions that need to be addressed. For debugging, running tar_make(callr_function = NULL) is needed to make tools such as browser() calls work.

    1)  

        ```         
        Please use XX_YYxZZ_formtemplate to indicate '~HEADER~', '~DATA~', and '~IGNORE~' cells.
        ```

        This error indicates that a new form template has been created in `DataCrosswalks/StateForms`. The user should open the form in whatever application is easiest. The csv file will be a print out of the data. The user should overwrite all cells with either `~HEADER~` if the cell contains a header, `~DATA~` if the cell contains data, or `~IGNORE~` if the cell contains information that is not necessary to ingest.

    2)  

        ```         
        could not check target datafp. file not found or no read permission at 'XXXX'
        ```

        This error indicates that there is a data file open that needs to be closed.

    3)  

        ```         
        Format of XXFILENAMEXX still needs to be handled in code. Please leave all headers as NA for now until code can accomodate.
        ```

        This error indicates that the code is not ready to handle this file yet. This appears if a form is entered. The code to handle forms is still in development. The easiest way to get past this error is to delete the row that describes this file from the HeaderCrosswalk.csv. It will be added back with "NAs" next time tar_make() is run.

    4)  

        ```         
        Pivot instructions need to be entered into DataPivots.csv for file XXXX
        ```

        This error indicates that the file `DataPivots.csv` needs to be filled out for this input file. There are 7 columns in this data crosswalk file that need to be editted:

        1)  `file` is the file name, which can be copy and pasted from the error message

        2)  `long_wide` indicates if the data needs to be pivoted into a longer or wider format. Values can be "wide" or "long" and indicate the format that the data needs to be remade into. If there are aspects of the data that need both, enter two lines for the file, one for "long" and one for "wide"

        3)  `cols` indicates which columns need to be pivoted. For pivoting longer, this should be R code that selects the headers that need to be pivoted. For wide pivots, this should include R code that generates an ID column that is not already in the data (e.g. if a date column needs to be split into month and year columns, the year column will go in the `cols` column.

        4)  `names_tofrom` indicates what column the headers will be sent to (for long pivots) or the column the headers will be taken from (for wide pivots). This may be entered as R code to generate these names, such as if a date column needs to be split into month and year columns. If multiple entries are needed, they can be separated with a comma.

        5)  `names_pattern` only needs to be filled out if there is more than one entry in `names_tofrom`. It indicates the text that separates the different parts of the headers, and is preceeded by "sep: ". For example, if I have a header `123456 in ACFT` that needs to be split into SourceNumber and Units_monthly, I would enter `sep: "in"`.

        6)  `values_tofrom` indicate what column the data will be sent to (for long pivots) or the column the data will be taken from (for wide pivots). If both long and wide pivots will be made on the same data, be sure that headers match when needed.

        7)  `values_transform` indicates if any additional processing is needed on the data once it has been pivoted. This may be left blank. An example entry could be `Annual_reported = as.numeric` to indicate that the data in the Annual_reported column will need to be transformed to numeric data.

        Pivot instructions can be tricky to get correct. For debugging, enter the `applyPIVOTrules` function, and look at how the entries are being transformed into R code. This process is still in development.

    5)  

        ```         
        Enter suspected HEADER value based on FILE.ext Suggested options are ...
        ```

        This error appears if data needs to be manually entered into `HardcodedManualAttributes.csv`. A line will have been added to the bottom of the csv file, and the user will need to manually update the `Value` column based on their best judgement from the data files. If any suggestions are made from the code, they will be displayed in the error message.

    6)  

        ```         
        New codes XXXX detected for HEADER in FILEPATH
        ```

        This error appears if `DataCodesCrosswalk.csv` needs to be updated. For this file, the first column is the header of the formatted data column that has the new codes. The second column is the old code (these will be listed in the error message), and the third code is the value that should be used instead. Each old code needs its own line

7)  When tar_make() runs successfully to the end, it is highly recommended to inspect the output data to ensure it looks as expected.

8)  When new data files are added to the state_data folder, new lines will be added to `DataCrosswalks/DataDirectories.csv`, with NA entered for columns `SiteDescriptions`, `LocationInfo`, `MonthlyData`, `AnnualData`, `Metadata`, `Duplicate`, and `NotRelevant`. This process can be repeated for the new entries.
