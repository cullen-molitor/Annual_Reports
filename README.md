# Annual_Reports
Repository of Rmarkdown scripts used to build KFM Annual Report appendix graphs.

## Usage

In order to use this script, it is necessary to download or have the raw text files from the Kelp Forest Monitoring access database. View the Database Management SOP in the Kelp Forest Monitoring Handbook SOP folder for instructions on how to create an export database with the correct tables. With the export databsae setup, the tables should be exported as text files. Only use raw data.

The files needed are the following:

1. "KFM_1mQuadrat_RawData_1982-CurrentYear.txt"
2. "KFM_5mQuadrat_RawData_1982-CurrentYear.txt"
3. "KFM_BandTransect_RawData_1982-CurrentYear.txt"
4. "KFM_RandomPointContact_RawData_1982-CurrentYear.txt"
5. "KFM_VisualFishTransect_RawData_1985-CurrentYear.txt"
6. "KFM_NaturalHabitatSizeFrequency_RawData_1985-CurrentYear.txt"
7. "Temperature_RawData_1994-CurrentYear.txt"

These files should all be placed in the folder titled "Raw_DB_Files_SAVE_HERE" in the directory with this project.

With the raw data files in place, open the "Raw_to_Tidy.R" script in RStudio. Hit ALT + o to collapse the code chunks. Select all the code and hit CTRL + Enter to run. 