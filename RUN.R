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
rmarkdown::render("1_m_Quadrats.Rmd", output_dir = "Output_Documents")
rmarkdown::render("5_m_Quadrats.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Band_Transects.Rmd", output_dir = "Output_Documents")
rmarkdown::render("RPCs.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Visual_Fish_Transect.Rmd", output_dir = "Output_Documents")
rmarkdown::render("Temperature.Rmd", output_dir = "Output_Documents")
rmarkdown::render("NHSF.Rmd", output_dir = "Output_Documents")

rmarkdown::render("Biomass.Rmd", output_dir = "Output_Documents")
