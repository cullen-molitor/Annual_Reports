#
#
#
#
#     Use for Manipulating data frames 
#
#
#
#
#

source("global_markdown.R")

{ # 1 m DF     ----
  
  { # RAW   ----
    oneM_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tibble::add_row(SiteNumber = 5, IslandCode = "SR", IslandName = "Santa Rosa Island", SiteCode = "RR", SiteName = "Rodes Reef", Species = 9008, 
                      ScientificName = "Lithopoma gibberosa", CommonName = "red turban snail", SurveyYear = 1985, Date = as.Date("1985-08-27"), 
                      QuadratNumber= 1:19, CountA = 0, CountB = NA,  ReserveStatus = "Outside", MeanDepth = 44.77, Reference = FALSE) %>% 
      tibble::add_row(SiteNumber = 5, IslandCode = "SR", IslandName = "Santa Rosa Island", SiteCode = "RR", SiteName = "Rodes Reef", Species = 11001, 
                      ScientificName = "Patiria miniata", CommonName = "bat star", SurveyYear = 1985, Date = as.Date("1985-08-27"), 
                      QuadratNumber = 20, CountA = 0, CountB = NA,  ReserveStatus = "Outside", MeanDepth = 44.77, Reference = FALSE) %>% 
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/oneM_Raw_Tidy.csv")
  }
  
  { # Summary   ---- 
          
    oneM_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/oneM_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n(),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = n(),
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
             Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
             Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
               Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/oneM_Summary.csv")
    
  }
  
  { # MPA Summary    ----
    oneM_MPA <-  readr::read_csv("Tidy_Data_Dont_Touch/oneM_Raw_Tidy.csv") %>% 
      dplyr::filter(IslandCode != "CL", IslandCode != "SM", SiteCode != "KH", Reference == TRUE) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
               CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n(),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      ungroup() %>%
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(MPA_Area_Surveyed = n(),
                    MPA_Total_Count = sum(Count_To_Reuse),
                    MPA_Mean_Density = round(MPA_Total_Count / MPA_Area_Surveyed, 4),
                    MPA_SD = round(sd(Count_To_Reuse), 4),
                    MPA_SE = round(MPA_SD / sqrt(MPA_Area_Surveyed), 4),
                    MPA_Date = mean.Date(Date),
                    MPA_Name = 
                      ifelse(IslandName == "Santa Rosa Island", "South Point SMR at Santa Rosa Island ", 
                             ifelse(IslandName == "Santa Cruz Island", "Scorpion SMR at Santa Cruz Island",
                                    ifelse(IslandName == "Anacapa Island", "Anacapa Island SMR", 
                                           "Santa Barbara Island SMR")))) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, MPA_Date, MPA_Mean_Density, MPA_SD, 
                    MPA_SE, MPA_Area_Surveyed, MPA_Total_Count, Survey_Type, MPA_Name) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/oneM_MPA.csv")
  }
  
} 

{ # 5 m DF    ----
  
  { # RAW    ----
    fiveM_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1982-{Export_END_Year}.txt")) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) 
      
    Macro_Combo <- fiveM_Raw %>%
      dplyr::filter(Species == 2002.25 | Species == 2002.75) %>%
      dplyr::mutate(CommonName = "giant kelp, all > 1 m tall",
                    Species = 2002) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::distinct(.keep_all = TRUE) %>% 
      dplyr::ungroup()
    
    Sargassum_Combo <- fiveM_Raw %>%   
      dplyr::filter(Species == 2016.00 | Species == 2016.50) %>%
      dplyr::mutate(CommonName = "Sargassum horneri, all",
                    Species = 2017) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::distinct(.keep_all = TRUE) %>%
      dplyr::ungroup()
    
    Undaria_Combo <- fiveM_Raw %>%  
      dplyr::filter(Species == 2009.00 | Species == 2009.50) %>%
      dplyr::mutate(CommonName = "wakame, all",
                    Species = 2009.75) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::distinct(.keep_all = TRUE) %>%
      dplyr::ungroup()
    
    
    base::rbind(fiveM_Raw, Macro_Combo, Sargassum_Combo, Undaria_Combo)  %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/fiveM_Raw_Tidy.csv")
  }
  
  { # Summary    ----
    fiveM_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/fiveM_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n() * 5,
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = n() * 5,
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
                    Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/fiveM_Summary.csv")
  }
  
  { # MPA Summary   ----
    fiveM_MPA <-  readr::read_csv("Tidy_Data_Dont_Touch/fiveM_Raw_Tidy.csv") %>% 
      dplyr::filter(IslandCode != "CL", IslandCode != "SM", SiteCode != "KH", Reference == TRUE) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n() * 5,
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      ungroup() %>%
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(MPA_Area_Surveyed = n() * 5,
                    MPA_Total_Count = sum(Count_To_Reuse),
                    MPA_Mean_Density = round(MPA_Total_Count / MPA_Area_Surveyed, 4),
                    MPA_SD = round(sd(Count_To_Reuse), 4),
                    MPA_SE = round(MPA_SD / sqrt(MPA_Area_Surveyed), 4),
                    MPA_Date = mean.Date(Date),
                    MPA_Name = ifelse(IslandName == "Santa Rosa Island", "South Point SMR at Santa Rosa Island ", 
                                      ifelse(IslandName == "Santa Cruz Island", "Scorpion SMR at Santa Cruz Island",
                                             ifelse(IslandName == "Anacapa Island", "Anacapa Island SMR", "Santa Barbara Island SMR"))) ) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, MPA_Date, MPA_Mean_Density, MPA_SD, 
                    MPA_SE, MPA_Area_Surveyed, MPA_Total_Count, Survey_Type, MPA_Name) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/fiveM_MPA.csv")
  }
  
}

{ # Band DF    ----
  
  { # RAW   -----
    bands_Raw <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, TransectNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/bands_Raw_Tidy.csv")
  }
  
  { # Summary   -----
    bands_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/bands_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n() * 30,
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD/sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = n() * 30,
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD/sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
                    Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/bands_Summary.csv")
  }
    
  { # MPA Summary   -----
    bands_MPA <- readr::read_csv("Tidy_Data_Dont_Touch/bands_Raw_Tidy.csv") %>% 
      dplyr::filter(IslandCode != "CL", IslandCode != "SM", SiteCode != "KH", Reference == TRUE) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n() * 30,
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD/sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>%
      ungroup() %>%
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(MPA_Area_Surveyed =  n() * 30,
                    MPA_Total_Count = sum(Count_To_Reuse),
                    MPA_Mean_Density = round(MPA_Total_Count / MPA_Area_Surveyed, 4),
                    MPA_SD = round(sd(Count_To_Reuse), 4),
                    MPA_SE = round(MPA_SD/sqrt(MPA_Area_Surveyed), 4),
                    MPA_Date = mean.Date(Date),
                    MPA_Name = ifelse(IslandName == "Santa Rosa Island", "South Point SMR at Santa Rosa Island ", 
                                      ifelse(IslandName == "Santa Cruz Island", "Scorpion SMR at Santa Cruz Island",
                                             ifelse(IslandName == "Anacapa Island", "Anacapa Island SMR", "Santa Barbara Island SMR"))) ) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, MPA_Date, MPA_Mean_Density, MPA_SD, 
                    MPA_SE, MPA_Area_Surveyed, MPA_Total_Count, Survey_Type, MPA_Name) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/bands_MPA.csv")
  }
  
}

{ # RPC DF   ----
  
  { # RAW    ----
    rpcs_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
      col_types = cols(CountA = col_number(), CountB = col_number(), CountC = col_number(), CountD = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y'), Survey_Type = "RPC") %>%
      tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, Quadrat_Number, Count, ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/rpcs_Raw_Tidy.csv")
    
  }
  
  { # Summary   ---- 
    rpcs_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/rpcs_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear == 1982, 5, 
                                              ifelse(SurveyYear == 1983, 4,
                                                     ifelse(SurveyYear == 1984, 5,
                                                            ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = ifelse(SurveyYear == 1982, 5 * length(unique(SiteName)), 
                                                  ifelse(SurveyYear == 1983, 4 * length(unique(SiteName)),
                                                         ifelse(SurveyYear == 1984, 5 * length(unique(SiteName)),
                                                                ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10 * length(unique(SiteName)), 
                                                                       6 * length(unique(SiteName)))))),
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
                    Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/rpcs_Summary.csv")
  }
    
  { # MPA Summary ----
    rpcs_MPA <- readr::read_csv("Tidy_Data_Dont_Touch/rpcs_Raw_Tidy.csv") %>% 
      dplyr::filter(IslandCode != "CL", IslandCode != "SM", SiteCode != "KH", Reference == TRUE) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear == 1982, 5, 
                                              ifelse(SurveyYear == 1983, 4,
                                                     ifelse(SurveyYear == 1984, 5,
                                                            ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      ungroup() %>%
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(MPA_Area_Surveyed = ifelse(SurveyYear == 1982, 5 * length(unique(SiteName)) / 2, 
                                               ifelse(SurveyYear == 1983, 4 * length(unique(SiteName)) / 2,
                                                      ifelse(SurveyYear == 1984, 5 * length(unique(SiteName)) / 2,
                                                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10 * length(unique(SiteName)) / 2, 
                                                                    6 * length(unique(SiteName)) / 2)))),
                    MPA_Total_Count = sum(Count_To_Reuse),
                    MPA_Mean_Density = round(MPA_Total_Count / MPA_Area_Surveyed, 4),
                    MPA_SD = round(sd(Count_To_Reuse), 4),
                    MPA_SE = round(MPA_SD / sqrt(MPA_Area_Surveyed), 4),
                    MPA_Date = mean.Date(Date),
                    MPA_Name = 
                      ifelse(IslandName == "Santa Rosa Island", "South Point SMR at Santa Rosa Island ", 
                             ifelse(IslandName == "Santa Cruz Island", "Scorpion SMR at Santa Cruz Island",
                                    ifelse(IslandName == "Anacapa Island", "Anacapa Island SMR", 
                                           "Santa Barbara Island SMR")))) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, MPA_Date, MPA_Mean_Density, MPA_SD, 
                    MPA_SE, MPA_Area_Surveyed, MPA_Total_Count, Survey_Type, MPA_Name) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/rpcs_MPA.csv")
  }
  
}

{ # VFT DF  ----
  
  { # RAW   ----
    VFT_Raw <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_VisualFishTransect_RawData_1985-{Export_END_Year}.txt")) %>% 
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName)) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
                    Survey_Type = "VFT") %>%
      dplyr::filter(IslandCode != "CL") %>% 
      dplyr::rename(Count = CountA) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, Species, ScientificName,
                    CommonName, Transect_Number, Count, ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
      dplyr::group_by(SiteNumber, SurveyYear) %>%
      dplyr::filter(Date == max(Date)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(SiteNumber, SurveyYear, Species) %>% 
      tidyr::pivot_wider(names_from = "Transect_Number", names_prefix = "T", values_from = "Count") %>%
      dplyr::mutate(T1 = ifelse(SurveyYear < 1997, T1, T1 + T2),
                    T2 = ifelse(SurveyYear < 1997, T2, T3 + T4)) %>% 
      dplyr::select(-T3, -T4) %>%
      tidyr::pivot_longer(cols = c(T1, T2), values_to = "Count", names_to = "Transect_Number") %>%
      dplyr::ungroup() %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/VFT_Raw_Tidy.csv")
    
    
  }
  
  { # Summary  ----
    VFT_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/VFT_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = n(),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = n(),
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
                    Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/VFT_Summary.csv")
  }
  
  { # MPA Summary  ----
    
  }
  
}

{ # Temperature DF   ----
  
  { # RAW  ----
    temp_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/Temperature_RawData_1994-{Export_END_Year}.txt")) %>%
      dplyr::filter(IslandCode != "CL", Site_Number < 38, !base::is.na(Temp_C)) %>% 
      dplyr::select(-Date, -Time) %>%
      tidyr::separate(DateTime, c('Date','Time'),' ') %>%
      dplyr::group_by(Date, Site_Number) %>% 
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                    Month = lubridate::month(Date, label = TRUE),
                    Temp_Daily_Min = base::min(Temp_C),
                    Temp_Daily_Max = base::max(Temp_C),
                    Temp_Daily_Mean = base::mean(Temp_C)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(Site_Number, Year, Month) %>% 
      dplyr::mutate(Temp_Monthly_Mean = base::mean(Temp_C)) %>%
      dplyr::ungroup() %>% 
      dplyr::distinct(Site_Number, Date, .keep_all = TRUE) %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    Year, Month, Date, Temp_Daily_Mean, Temp_Daily_Min, Temp_Daily_Max, Temp_Monthly_Mean, MeanDepth) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/Temp_Raw_Tidy.csv")
  }
  
}

{ # RDFC DF   ----
  
  { # RAW  ----
    rdfc_Raw <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", "NA", Abundance),
                    Abundance = gsub("-", "NA", Abundance)) %>%
      dplyr::filter(IslandCode != "CL") %>% 
      # dplyr::mutate_at(14, ~replace(., is.na(.), 0)) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                    Count = as.double(Count)) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date),
                    SurveyYear > 1996,
                    ExperienceLevel == "E") %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(SiteNumber, CommonName, Abundance) %>% 
      dplyr::mutate(
        Count = case_when(
          is.na(Count) & is.na(Score) ~ 0,
          SurveyYear < 2004 & Abundance == "S" ~ 1,
          is.na(Count) & !is.na(Score) ~ mean(Count, na.rm = T),
          !is.na(Count) & !is.na(Score) ~ Count
      )) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, 
                    SurveyYear, Date, Score, Abundance, Count, ExperienceLevel, PermanentObserverNumber,
                    ReserveStatus, MeanDepth) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv")
    # mutate(ExperienceLevel = ifelse(RDFC_DF$PermanentObserverNumber == 1 |
    #                                   RDFC_DF$PermanentObserverNumber == 20,
    #                                 "E", RDFC_DF$ExperienceLevel),
    #        Count = ifelse(is.na(RDFC_DF$Count) & is.na(RDFC_DF$Score), 0, 
    #                       ifelse(SurveyYear < 2004 & RDFC_DF$Abundance == "S", 1, 
    #                              ifelse(SurveyYear < 2004 & RDFC_DF$Abundance == "F", 2,
    #                                     ifelse(SurveyYear < 2004 & RDFC_DF$Abundance == "C", 3, 
    #                                            ifelse(SurveyYear < 2004 & RDFC_DF$Abundance == "M", 4, RDFC_DF$Count))))))
  }
  # rdfc_Raw$Abundance <- factor(rdfc_Raw$Abundance, levels = c("NA", "S", "F", "C", "M"), ordered=TRUE)
  # unique(rdfc_Raw$CommonName)
  # filtered <- rdfc_Raw %>% 
  #   filter(CommonName == "garibaldi, adult",
  #          SiteCode == "GI")
  # ggplot(data = filtered, aes(x = SurveyYear, y = Abundance, group = CommonName, color = CommonName)) +
  #   geom_line(size = 1)
  
  { # Summary   ----
    # oneM_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv") %>% 
    #   dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
    #                   CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
    #   dplyr::summarise(Count_To_Reuse = Count,
    #                    Area_Surveyed = n(),
    #                    Total_Count = sum(Count),
    #                    Mean_Density = round(Total_Count / Area_Surveyed, 4), 
    #                    SD = round(sd(Count), 4),
    #                    SE = round(SD / sqrt(Area_Surveyed), 4),
    #                    Survey_Type = "RDFC") %>% 
    #   dplyr::ungroup() %>%  
    #   dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
    #   dplyr::mutate(Island_Area_Surveyed = n(),
    #                 Island_Total_Count = sum(Count_To_Reuse),
    #                 Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
    #                 Island_SD = round(sd(Count_To_Reuse), 4),
    #                 Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
    #                 Island_Date = mean.Date(Date)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
    #                 Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed,MeanDepth, Island_Date, Island_Mean_Density, Island_SD, 
    #                 Island_SE, Island_Area_Surveyed, Island_Total_Count, Survey_Type) %>%
    #   dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
    #                   Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
    #   readr::write_csv("Tidy_Data_Dont_Touch/RDFC_Summary.csv")
  }
  
}

{ # NHSF DF    ----
  
  { # RAW   -----
    nhsf_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_NaturalHabitatSizeFrequency_RawData_1985-{Export_END_Year}.txt")) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::group_by(SurveyYear, SiteNumber) %>%
      dplyr::mutate(Date = mean(as.Date(Date, format='%m/%d/%Y'))) %>%
      tidyr::uncount(weights = NoOfInd, .remove = TRUE) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SiteNumber, SurveyYear, Date, IslandName, SiteName, ScientificName, CommonName, 
             Size_mm, IslandCode, SiteCode, Species, ReserveStatus, MeanDepth, Reference) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/NHSF_Raw_Tidy.csv")
  }
  
  { # Summary   -----
    nhsf_Summary <- readr::read_csv("Tidy_Data_Dont_Touch/NHSF_Raw_Tidy.csv") %>%
      dplyr::group_by(SiteCode, Species, SurveyYear) %>%
      dplyr::mutate(Total_Count = n(),
                    Mean_Size = round(mean(Size_mm), 4),
                    SD = round(sd(Size_mm), 4),
                    SE = round(SD / sqrt(Total_Count), 4)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(IslandCode, Species, SurveyYear) %>%
      dplyr::mutate(Island_Total_Count = n(),
                    Island_Mean = round(mean(Size_mm), 4),
                    Island_SD = round(sd(Size_mm), 4),
                    Island_SE = round(SD / sqrt(n()), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, Date,
                    Total_Count, Mean_Size, SD, SE, Island_Date, Island_Total_Count, Island_Mean, Island_SD, Island_SE,
                    MeanDepth) %>%
      dplyr::distinct(SiteNumber, Species, SurveyYear, Mean_Size, SD, SE, Total_Count, .keep_all = TRUE) %>%
      dplyr::mutate_at(c(13:14, 18:19), ~replace(., is.na(.), 0)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/NHSF_Summary.csv")
  }
  
  { # MPA Summary   -----
    nhsf_MPA <- readr::read_csv("Tidy_Data_Dont_Touch/NHSF_Raw_Tidy.csv") %>%
      dplyr::group_by(SiteCode, Species, SurveyYear) %>%
      dplyr::filter(IslandCode != "CL", IslandCode != "SM", SiteCode != "KH", Reference == TRUE) %>%
      dplyr::group_by(SiteCode, Species, SurveyYear) %>%
      dplyr::mutate(Total_Count = n(),
                    Mean_Size = round(mean(Size_mm), 4),
                    SD = round(sd(Size_mm), 4),
                    SE = round(SD / sqrt(Total_Count), 4),
                    Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(ReserveStatus, IslandCode, IslandName, Species, ScientificName, CommonName, SurveyYear) %>%
      dplyr::mutate(MPA_TotalCount =  n(),
                    MPA_Mean = round(mean(Size_mm), 4),
                    MPA_SD = round(sd(Size_mm), 4),
                    MPA_SE = round(SD / sqrt(MPA_TotalCount), 4),
                    MPA_Date = mean(Date),
                    MPA_Name = ifelse(IslandName == "Santa Rosa Island", "South Point SMR at Santa Rosa Island ", 
                                      ifelse(IslandName == "Santa Cruz Island", "Scorpion SMR at Santa Cruz Island",
                                             ifelse(IslandName == "Anacapa Island", "Anacapa Island SMR", "Santa Barbara Island SMR")))) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, Date,
                    Mean_Size, Total_Count, SD, SE, MPA_Date, MPA_TotalCount, MPA_Mean, MPA_SD, MPA_SE,
                    MeanDepth, ReserveStatus, MPA_Name) %>%
      dplyr::distinct(SiteNumber, Species, SurveyYear, Mean_Size, SD, SE, Total_Count, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/NHSF_MPA.csv")
  }
  
} 

{ # FSF DF    ----
  
  { # Raw ----
    FSF_Raw <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_FishSizeFrequency_RawData_2007-{Export_END_Year}.txt")) %>%   
      left_join(siteInfo1) %>%
      separate(SurveyDate, c('Date','Time'),' ') %>%
      filter(IslandCode != "CL") %>%
      dplyr::rename(Size_cm = TotalLength_cm) %>%
      mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::uncount(weights = Count, .remove = TRUE) %>%
      select(SiteNumber, SurveyYear, Date, IslandName, SiteName, ScientificName, CommonName,
             Size_cm, IslandCode, SiteCode, ReserveStatus, MeanDepth) %>%
      write_csv("Tidy_Data_Dont_Touch/FSF_Raw_Tidy.csv")
  }
  
  { # Mean ----
    FSF_Summary <- FSF_Raw %>%
      group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear) %>%
      mutate(TotalCount = length(Size_cm),
             MeanSize = sum(Size_cm)/TotalCount,
             StandardDeviation = sd(Size_cm, na.rm = FALSE),
             StandardError = std.error(Size_cm, na.rm),
             Date = mean(as.Date(Date))) %>%
      ungroup() %>%
      group_by(IslandCode, IslandName, ScientificName, CommonName, SurveyYear) %>%
      mutate(IslandTotalCount = length(Size_cm),
             Island_Mean = sum(Size_cm)/TotalCount,
             IslandSD = sd(Size_cm, na.rm = FALSE),
             IslandSE = std.error(Size_cm, na.rm),
             IslandDate = mean(Date)) %>%
      select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear, Date,
             MeanSize, TotalCount, StandardError, StandardDeviation, IslandDate, IslandTotalCount, Island_Mean, IslandSD, IslandSE,
             MeanDepth) %>%
      distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear, MeanSize, 
               StandardError, StandardDeviation, TotalCount, .keep_all = TRUE) 
    
    FSF_Summary <- FSF_Summary %>%
      mutate_at(c(12:13, 17:18), ~replace(., is.na(.), 0)) %>% 
      write_csv("Tidy_Data_Dont_Touch/FSF_Summary.csv")
  }
  
}

{ # Biomass  ----
  
  { # Invert  ----
    
  }
  
  { # Fish  ----
    
  }
  
}

{ # Visibility DF   ----
  Secchi_Raw <- readr::read_csv(
    glue("Raw_DB_Files_SAVE_HERE/KFM_VisualFishTransect_Secchi_1985-{Export_END_Year}.txt")) %>% 
    tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
    dplyr::filter(IslandCode != "CL") %>%
    dplyr::left_join(siteInfo1) %>%
    dplyr::group_by(SurveyYear, SiteNumber) %>%
    dplyr::mutate(Date = mean(as.Date(Date, format='%m/%d/%Y'))) %>% 
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, 
                  Horizontal_Secchi_m, Surge_degrees, ReserveStatus, MeanDepth, Reference)
}




