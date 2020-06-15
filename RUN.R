# 
#
#
#
#
#
#
# RUN the following to produce the annual report documents
# 
#
#
#
#
#
#
# ONLY RUN THIS TO CLEAN RAW DATA INTO TIDY DATA
# RAW DATA FILES NEED TO BE IN "Raw_DB_Files_SAVE_HERE" FOLDER
# RAW DATA FILES SHOULD BE THE DEFAULT FILE NAME GIVEN WHEN EXPORTED FROM DB
# MAKE SURE TO SET THE YEAR FOR THE DATA FILES AND FOR THE REPORT YEAR IN "Raw_to_Tidy.R"
source("Raw_to_Tidy.R")
# ^^^ THIS WILL TAKE SEVERAL MINUTES 


# RUN THIS TO PRODUCE ANNUAL REPORT APPENDIX GRAPH DOCUMENTS
rmarkdown::render("Rmarkdown_Files/1_m_Quadrats.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/5_m_Quadrats.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/Band_Transects.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/RPCs.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/Visual_Fish_Transect.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/Temperature.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Rmarkdown_Files/NHSF.Rmd", output_dir = "Output_Documents")
