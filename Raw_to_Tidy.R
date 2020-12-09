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

{ # SST Anomaly Index (ONI and PDO)   ----
  
  { # Oceanic Nino Index  ----
    oni <- read.table( # Read in  ONI to be added to all data
      "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt",
      header = T) %>%
      dplyr::mutate(Date = as.Date(ISOdate(YR, MON, 1)),
                    DateStart = as.Date(ISOdate(YR, MON, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::filter(YR > 2004, Date < glue("{Year_to_Filter_Data_by}-2-1")) %>%
      dplyr::rename(ONI_ANOM = ANOM,
                    Month = MON,
                    SurveyYear = YR) %>% 
      dplyr::group_by(SurveyYear) %>%
      dplyr::mutate(Mean_ONI_ANOM = mean(ONI_ANOM)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, ONI_ANOM, Mean_ONI_ANOM) 
    
    annual_mean_oni <- oni %>% 
      dplyr::select(SurveyYear, Mean_ONI_ANOM) %>% 
      dplyr::distinct(SurveyYear, .keep_all = TRUE)
  }
  
  { # PDO  ----
    pdo <- read.table(
      "https://www.cpc.ncep.noaa.gov/products/GODAS/PDO/pdo_h300_pac_current.txt",
      header = T)  %>%
      dplyr::mutate(Date = as.Date(ISOdate(Year, Month, 1)),
                    DateStart = as.Date(ISOdate(Year, Month, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::filter(Year > 2004, Date < glue("{Year_to_Filter_Data_by}-2-1")) %>%
      dplyr::rename(PDO_ANOM = PDO,
                    SurveyYear = Year) %>% 
      dplyr::group_by(SurveyYear) %>%
      dplyr::mutate(Mean_PDO_ANOM = mean(PDO_ANOM)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, PDO_ANOM, Mean_PDO_ANOM)
    
    annual_mean_pdo <- pdo %>% 
      dplyr::select(SurveyYear, Mean_PDO_ANOM) %>% 
      dplyr::distinct(SurveyYear, .keep_all = TRUE)
  }
  
  { # Full Index  ----
    SST_Anomaly_Index <- dplyr::left_join(pdo, oni) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/SST_Anomaly_Index.csv")
  }
  
}

{ # 1 m DF     ----
  
  { # RAW   ----
    oneM_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>% 
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    CommonName != "giant kelp stipes > 1m") %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/oneM_Raw_Tidy.csv")
  }
  
  { # Summary   ---- 
          
    oneM_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/oneM_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      Species, CommonName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD / sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    Species, CommonName,
                    ScientificName,SurveyYear, 
             Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, 
             Island_Date, Island_Mean_Density, Island_SD,
             Island_SE, Island_Area_Surveyed, Island_Total_Count,
             Survey_Type, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      Species, CommonName,
                      ScientificName, SurveyYear, 
               Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/oneM_Summary.csv")
    
  }
  
} 

{ # 5 m DF    ----
  
  { # RAW    ----
    fiveM_Raw <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
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
    
    
    base::rbind(fiveM_Raw, 
                Macro_Combo, Sargassum_Combo,
                Undaria_Combo)  %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/fiveM_Raw_Tidy.csv")
  }
  
  { # Summary    ----
    fiveM_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/fiveM_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      Species, CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
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
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    CommonName, Species,
                    SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Island_Date, Island_Mean_Density, Island_SD,
                    Island_SE, Island_Area_Surveyed, Island_Total_Count,
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      Species, CommonName,
                      ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/fiveM_Summary.csv")
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
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, TransectNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber) %>%
      dplyr::mutate(Count = sum(Count, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, ScientificName, SurveyYear, TransectNumber, Count, .keep_all = TRUE) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/bands_Raw_Tidy.csv")
    
    bands_Raw <- bands_Raw %>% 
      filter(ScientificName == "Undaria pinnatifida")
  }
  
  { # Summary   -----
    bands_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/bands_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      Species, CommonName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60), 
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD/sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                    Island_Total_Count = sum(Count_To_Reuse),
                    Island_Mean_Density = round(Island_Total_Count / Island_Area_Surveyed, 4),
                    Island_SD = round(sd(Count_To_Reuse), 4),
                    Island_SE = round(Island_SD/sqrt(Island_Area_Surveyed), 4),
                    Island_Date = mean.Date(Date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                    Species, CommonName,
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Island_Date, Island_Mean_Density, Island_SD,
                    Island_SE, Island_Area_Surveyed, Island_Total_Count,
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      Species, CommonName,
                      ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/bands_Summary.csv")
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
  
  { # Summary  ----
    
    rpcs_Percent_Cover <-  readr::read_csv("Tidy_Data_Dont_Touch/rpcs_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear == 1982, 5, 
                                              ifelse(SurveyYear == 1983, 4,
                                                     ifelse(SurveyYear == 1984, 5,
                                                            ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
                       Total_Count = sum(Count),
                       Percent_Cover = 
                         ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
                                ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                                       ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                                              ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                                     round((Total_Count / Area_Surveyed), 4))))),
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Percent_Cover, SD, SE, Area_Surveyed, MeanDepth, ReserveStatus, Reference, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Percent_Cover, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/rpcs_Percent_Cover.csv")
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
                       Area_Surveyed = 600,
                       Total_Count = sum(Count),
                       Mean_Density = round(Total_Count / Area_Surveyed, 4), 
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::group_by(IslandCode, Species, CommonName, SurveyYear) %>%
      dplyr::mutate(Island_Area_Surveyed = n() * 600,
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
      dplyr::mutate(Include = ifelse(is.even(match(Month, month.abb)) & n() < 24, FALSE, 
                                     ifelse(is.odd(match(Month, month.abb)) & n() < 25, FALSE, TRUE)))%>% 
      dplyr::filter(Include == TRUE) %>%
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
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance)) %>%
      dplyr::filter(IslandCode != "CL") %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                    Count = as.double(Count)) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date),
                    SurveyYear > 1996,
                    ExperienceLevel == "E") %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(CommonName) %>% 
      dplyr::mutate(
        Count = case_when(
          is.na(Count) & is.na(Score) ~ 0,
          Abundance == "S" ~ 1,
          is.na(Count) & !is.na(Score) ~ mean(Count, na.rm = TRUE),
          !is.na(Count) & !is.na(Score) ~ Count
      )) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, 
                    SurveyYear, Date, Score, Abundance, Count, ExperienceLevel, PermanentObserverNumber,
                    ReserveStatus, MeanDepth, Reference) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv")
  }
  
  { # Summary   ----
    rdfc_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv") %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = 2000,
                       Mean_Count = mean(Count),
                       Mean_Density = round(Mean_Count / Area_Surveyed, 4),
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4),
                       Survey_Type = "RDFC") %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear,
                    Date, Mean_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear,
                      Mean_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/RDFC_Summary.csv")
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
    # FSF_Summary <- FSF_Raw %>%
    #   group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear) %>%
    #   mutate(TotalCount = length(Size_cm),
    #          MeanSize = sum(Size_cm)/TotalCount,
    #          StandardDeviation = sd(Size_cm, na.rm = FALSE),
    #          StandardError = std.error(Size_cm, na.rm),
    #          Date = mean(as.Date(Date))) %>%
    #   ungroup() %>%
    #   group_by(IslandCode, IslandName, ScientificName, CommonName, SurveyYear) %>%
    #   mutate(IslandTotalCount = length(Size_cm),
    #          Island_Mean = sum(Size_cm)/TotalCount,
    #          IslandSD = sd(Size_cm, na.rm = FALSE),
    #          IslandSE = std.error(Size_cm, na.rm),
    #          IslandDate = mean(Date)) %>%
    #   select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear, Date,
    #          MeanSize, TotalCount, StandardError, StandardDeviation, IslandDate, IslandTotalCount, Island_Mean, IslandSD, IslandSE,
    #          MeanDepth) %>%
    #   distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear, MeanSize, 
    #            StandardError, StandardDeviation, TotalCount, .keep_all = TRUE) 
    # 
    # FSF_Summary <- FSF_Summary %>%
    #   mutate_at(c(12:13, 17:18), ~replace(., is.na(.), 0)) %>% 
    #   write_csv("Tidy_Data_Dont_Touch/FSF_Summary.csv")
  }
  
}

{ # Benthic and Fish Count Data Formatted for Diversity   ----
  
  { # 1 m DF     ----
    oneM_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    CommonName != "giant kelp stipes > 1m") %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, Date, Total_Count, 
                    Mean_Density, SD, SE, Area_Surveyed, Survey_Type, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 1996,
                    ScientificName != "Macrocystis pyrifera" | SurveyYear < 1996,
                    ScientificName != "Undaria pinnatifida",
                    ScientificName != "Dictyoneuropsis reticulata/Agarum fimbriatum",
                    ScientificName != "Haliotis rufescens",
                    ScientificName != "Crassedoma giganteum",
                    ScientificName != "Kelletia kelletii",
                    ScientificName != "Parastichopus parvimensis",
                    ScientificName != "Oxylebius pictus",
                    ScientificName != "Pycnopodia helianthoides",
                    ScientificName != "Lytechinus anamesus",
                    ScientificName != "Sargassum horneri")
  } 
  
  { # 5 m DF    ----
    fiveM_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 2014,
                    ScientificName != "Pisaster ochraceus" | SurveyYear < 2014,
                    ScientificName != "Undaria pinnatifida") 
    
  }
  
  { # Bands DF    ----
    bands_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, TransectNumber, Count, ReserveStatus, MeanDepth, Reference)  %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD/base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, Date,
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>% 
      dplyr::filter(ScientificName != "Sargassum horneri")
    
  }
  
  { # RPC DF  ----
    rpcs_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/rpcs_Raw_Tidy.csv") %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear == 1982, 500, 
                                              ifelse(SurveyYear == 1983, 400,
                                                     ifelse(SurveyYear == 1984, 500,
                                                            ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 1000, 600)))),
                       Total_Count = sum(Count),
                       Mean_Density = 
                         ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed) / .05, 4), 
                                ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed) / .04, 4),
                                       ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed) / .05, 4),
                                              ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed) / .1, 4), 
                                                     round((Total_Count / Area_Surveyed) / .06, 4))))),
                       SD = round(sd(Count), 4),
                       SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, ReserveStatus, Reference, Survey_Type) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/rpcs_Summary.csv")
    
    rpcs_Data <-  readr::read_csv("Tidy_Data_Dont_Touch/rpcs_Summary.csv") %>% 
      dplyr::filter(ScientificName != "Macrocystis, Pterygophora, and Eisenia combined",
                    # ScientificName != "Macrocystis pyrifera",
                    # ScientificName != "Eisenia arborea",
                    # ScientificName != "Pterygophora californica",
                    # ScientificName != "Laminaria farlowii",
                    # ScientificName != "Sargassum horneri",
                    ScientificName != "Leucetta losangelensis",
                    ScientificName != "Hydrozoa",
                    ScientificName != "Bare Substrate",
                    ScientificName != "Rock",
                    ScientificName != "Cobble",
                    ScientificName != "Sand",
                    ScientificName != "Balanus",
                    ScientificName != "Sargassum muticum",
                    ScientificName != "Polymastia pachymastia",
                    ScientificName != "Spirobranchus spinosus") %>%
      dplyr::mutate(ScientificName = dplyr::case_when(
                      CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
                      CommonName == "articulated coralline algae" ~ "encrusting coralline algae",
                      CommonName != "articulated coralline algae" |
                        CommonName != "articulated coralline algae" ~ ScientificName))%>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, Date,
                    Total_Count,Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference)
    
    # unique(rpcs_Data$ScientificName)
    
  }
  
  { # Benthic Density and Count Tables   ----
    Benthic_Counts_Wide <- base::rbind(oneM_Data, fiveM_Data,
                                # rpcs_Data,
                                bands_Data) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      dplyr::mutate(Est_Total_Count = Mean_Density * 2000) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Est_Total_Count = base::sum(Est_Total_Count)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Est_Total_Count, values_fill = 0) %>%
      # dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/Benthic_Counts.csv") 

    # perhaps we call this Benthic_Counts_Table when we go through and do a cleanup? A bit more accurate name since 
    # we created site counts from densities in this bit, and the diversity() part is in the Rmd. Also it'll make a bit 
    # more sense when I'm done making the analogous benthic_biomass_table. 
  }
  
  { # Fish Density and Count Tables   ----
    
    Fish_Counts_Wide <- read_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv") %>%
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                    SurveyYear, Count, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      dplyr::mutate(Count= replace_na(Count, 0)) %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Count = mean(Count)) %>% 
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Count, values_fill = 0) %>%
      # dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/Fish_Counts.csv") 
    
    # `%nin%` = Negate(`%in%`)
    # 
    # fishyyy <- read_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv") %>%
    #   distinct(CommonName, ScientificName) %>% 
    #   filter(CommonName %nin% a) %>% 
    #   arrange(ScientificName) %>% 
    #   write_csv("FishyFishy.csv")
    # 
    # fish_species <- readr::read_csv("Meta_Data/SpeciesComplete2.csv") %>% 
    #   filter(Classification == "Fish")
    # a <- unique(fish_species$CommonName)
    # b <- unique(Fish_Data$CommonName)
    # base::setdiff(
    #   b,
    #   a)
  }
  
}

{ # Biomass Data   ----
  
  { # Benthic Density (1 m, 5 m, Bands)     ----
    oneM_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    CommonName != "giant kelp, juvenile",
                    CommonName != "giant kelp stipes > 1m") %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName,SurveyYear, 
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, 
                    Survey_Type, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) 
    
    oneM_Biomass <- oneM_Data %>% 
      dplyr::filter(ScientificName %in% oneM_Biomass_Species, 
                    ScientificName != "Pisaster giganteus" | SurveyYear < 1996)
  
    fiveM_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  
    fiveM_Biomass <- fiveM_Data %>% 
      dplyr::filter(ScientificName == "Pisaster giganteus",
                    ScientificName != "Pisaster giganteus" | SurveyYear < 2014) 
  
    bands_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, TransectNumber, Count, ReserveStatus, MeanDepth, Reference)  %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD/base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear,
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>% 
      dplyr::filter(ScientificName %in% bands_Biomass_Species)
    
  }
  
  { # Macro Biomass  ----
    oneM_Macro <- oneM_Data %>% 
      filter(ScientificName == "Macrocystis pyrifera",
             SurveyYear %in% 1984:1995)
    
    fiveM_Macro <- fiveM_Data %>% 
      dplyr::filter(ScientificName == "Macrocystis pyrifera")
    
    Macro <- rbind(oneM_Macro, fiveM_Macro)
    
    stipedensity_FSC <- read_csv("Meta_Data/stipedensity_FSC_regressioncoefs.csv") %>%
      dplyr::filter(Month %in% season_months) %>%
      dplyr::summarise(slope=mean(slope))
    
    Macro_Data <- read_csv(
      glue::glue("Raw_DB_Files_SAVE_HERE/KFM_Macrocystis_RawData_1984-{Export_END_Year}.txt"),
      col_types = cols(PermanentObserverNumber = col_double())) %>% 
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::select(-SurveyDate, -Species, -PermanentObserverNumber, -Marker, -Diameter_cm) %>% 
      dplyr::full_join(Macro,
                       by = c('SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 
                              'SiteName', 'ScientificName', 'SurveyYear')) %>% 
      dplyr::mutate(Stipe_Count = dplyr::case_when(
        is.na(Stipe_Count) & Mean_Density == 0 ~ 0,
        !is.na(Stipe_Count) ~ Stipe_Count)) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, Mean_Density,
                    ScientificName, SurveyYear, Stipe_Count, ReserveStatus,
                    MeanDepth, Reference, Survey_Type) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Stipe_Density = sum(Stipe_Count / n() * Mean_Density)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Biomass = (Stipe_Density * stipedensity_FSC$slope)*1000,
                    FSC = Biomass) %>% # converts from kg to g here
      dplyr::arrange(SiteName, SurveyYear) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/Macro_Biomass_Long.csv")
  }
  
  { # Invertebrate biomass raw  ----
    Invert_Biomass <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_InvertebrateBiomass_1985-{Export_END_Year}.txt")) %>% 
      dplyr::filter(IslandCode != "CL", !is.na(Invertebrate_Biomass),
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      tidyr::uncount(weights = NoOfInd) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, CommonName) %>% 
      dplyr::mutate(Bio_Proportion = 1 / n()) %>% 
      dplyr::ungroup() %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,  
                    SurveyYear, Size_mm, Invertebrate_Biomass, Bio_Proportion, ReserveStatus, MeanDepth, Reference)
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass_Long <- base::rbind(oneM_Biomass, fiveM_Biomass, bands_Data) %>% 
      dplyr::full_join(
        Invert_Biomass,
        by = c('SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 'SiteName','ScientificName',
               'SurveyYear', 'ReserveStatus', 'MeanDepth', 'Reference')) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(SiteNumber, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Mean_Density = dplyr::case_when(
          is.na(Mean_Density) & !is.na(Invertebrate_Biomass) ~ n()/2000,
          !is.na(Mean_Density) ~ Mean_Density),
        Invertebrate_Biomass = dplyr::case_when(
          is.na(Invertebrate_Biomass) & Mean_Density == 0 ~ 0,
          !is.na(Invertebrate_Biomass) ~ Invertebrate_Biomass),
        Bio_Proportion = dplyr::case_when(
          is.na(Bio_Proportion) & Mean_Density == 0 ~ 0,
          !is.na(Bio_Proportion) ~ Bio_Proportion),
        Biomass = base::sum(Bio_Proportion * Invertebrate_Biomass * Mean_Density)) %>% 
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, SurveyYear, ScientificName, .keep_all = TRUE)  %>% 
      dplyr::group_by(ScientificName, IslandName, SurveyYear) %>% 
      dplyr::mutate(Biomass = dplyr::case_when(
        is.na(Biomass) & Mean_Density > 0 ~ mean(Biomass, na.rm = TRUE),
        !is.na(Biomass) ~ Biomass),
        Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1))) %>% 
      dplyr::ungroup()%>% 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, MeanDepth, ReserveStatus, Reference),
                      ScientificName, SurveyYear,
                      fill = list(Mean_Density = 0, Survey_Type = "Bands")) %>%
      dplyr::filter(ScientificName != "Tegula regina" | SurveyYear > 2005) %>% 
      dplyr::group_by(SiteName, IslandName, ScientificName, SurveyYear, ReserveStatus) %>% 
      base::rbind(dplyr::select(Kelp_Biomass, -Stipe_Density))%>%
      dplyr::mutate(Mean_Biomass = round(mean(Biomass), 4)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, Mean_Density, ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
      dplyr::distinct(SiteName, IslandCode, ScientificName, SurveyYear, ReserveStatus, .keep_all = TRUE) %>% 
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/Benthic_Biomass_Long.csv")
  }
  
  { # Benthic Biomass Wide DF  ----
    Benthic_Biomass_Wide <- Benthic_Biomass_Long %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0)  %>%
      dplyr::arrange(SiteName, SurveyYear) %>% 
      base::cbind(dplyr::select(Kelp_Biomass, -IslandCode, -IslandName, -SiteCode, -SiteName,
                      -ScientificName, -SurveyYear, -ReserveStatus, -Stipe_Density)) %>%
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear"))
    names(Benthic_Biomass_Wide) <- str_replace_all(names(Benthic_Biomass_Wide), c(" " = "_" , "," = "" ))
    readr::write_csv(Benthic_Biomass_Wide, "Tidy_Data_Dont_Touch/Benthic_Biomass_Wide.csv") 
  }
  
  { # Fish Biomass   ----
    
    { # Raw NASTY  ----
      RDFC_Biomass_Data <- data.table::fread(
        glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
        dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                      Abundance = gsub("c", "C", Abundance),
                      Abundance = gsub("f", "F", Abundance),
                      Abundance = gsub("s", "S", Abundance),
                      Abundance = gsub("m", "M", Abundance),
                      Abundance = gsub("^$", NA, Abundance),
                      Abundance = gsub("-", NA, Abundance)) %>%
        dplyr::filter(IslandCode != "CL", SurveyYear > 2006, ExperienceLevel == "E") %>% 
        tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
        dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y')) %>% 
        dplyr::group_by(SurveyYear, SiteNumber) %>% 
        dplyr::filter(Date == base::max(Date)) %>% 
        dplyr::ungroup() %>% 
        dplyr::left_join(siteInfo1) %>%
        dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, 
                      SurveyYear, Date, Score, Abundance, Count, ExperienceLevel, PermanentObserverNumber,
                      ReserveStatus, MeanDepth, Reference)
      
      RDFC_Biomass_Data2 <- RDFC_Biomass_Data  %>% 
        dplyr::filter(ScientificName != "Semicossyphus pulcher",
                      ScientificName != "Halichoeres semicinctus") %>% 
        dplyr::group_by(SiteNumber, SurveyYear, ScientificName, PermanentObserverNumber) %>% 
        dplyr::mutate(Count = sum(Count)) %>% 
        dplyr::ungroup() %>% 
        dplyr::distinct(SiteNumber, SurveyYear, ScientificName, PermanentObserverNumber, .keep_all = TRUE) %>% 
        dplyr::mutate(CommonName = gsub("juvenile", "all", CommonName),
                      CommonName = gsub("subadult", "all", CommonName),
                      CommonName = gsub("adult", "all", CommonName, fixed = TRUE))
      
      RDFC_Biomass_Data3 <- RDFC_Biomass_Data  %>% 
        dplyr::filter(ScientificName == "Semicossyphus pulcher" |
                        ScientificName == "Halichoeres semicinctus")
      
      RDFC_Biomass_Data4 <- base::rbind(RDFC_Biomass_Data2, RDFC_Biomass_Data3) %>%
        dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                        CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
        dplyr::summarise(Count_To_Reuse = Count,
                         Area_Surveyed = 2000,
                         Mean_Count = mean(Count),
                         Mean_Density = round(Mean_Count / Area_Surveyed, 4),
                         SD = round(sd(Count), 4),
                         SE = round(SD / sqrt(Area_Surveyed), 4)) %>%
        dplyr::ungroup() %>%
        dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, CommonName, ScientificName, SurveyYear,
                      Mean_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Reference) %>%
        dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  CommonName, SurveyYear,
                        Mean_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>% 
        dplyr::mutate(CommonName = gsub(", all", "", CommonName))
      
      Fish_Biomass_Raw <- data.table::fread(
        glue("Raw_DB_Files_SAVE_HERE/KFM_FishBiomass_2007-{Export_END_Year}.txt")) %>%
        dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                      CommonName = gsub(", all", "", CommonName),
                      ScientificName = gsub(", all", "", ScientificName),
                      ScientificName = gsub(", male", "", ScientificName),
                      ScientificName = gsub(", female", "", ScientificName),
                      ScientificName = gsub(", juvenile", "", ScientificName),
                      Species_Count = as.double(Species_Count)) %>% 
        dplyr::filter(!is.na(Fish_Biomass)) %>% 
        dplyr::mutate(
          Fish_Biomass = dplyr::case_when(
            ScientificName == "Semicossyphus pulcher" ~ Fish_Biomass * 1000,
            ScientificName == "Scorpaenichthys marmoratus" ~ Fish_Biomass * 1000,
            ScientificName == "Sebastes mystinus" ~ Fish_Biomass * 1000,
            ScientificName != "Scorpaenichthys marmoratus" &
              ScientificName != "Scorpaenichthys marmoratus" &
              ScientificName != "Sebastes mystinus"~ Fish_Biomass))
      
      RDFC_Biomass_Data4 <- RDFC_Biomass_Data4 %>% 
        dplyr::filter(ScientificName %in% unique(Fish_Biomass_Raw$ScientificName))
      
      Fish_Trophic_Levels <- readr::read_csv("Meta_Data/KFM_Fish_Trophic_Levels.csv") %>% 
        dplyr::mutate(Count= replace_na(Legal_Size_cm, 0))
    }
    
    { # Fish Biomass Long   -----
      Fish_Biomass_Long <- Fish_Biomass_Raw %>%
        dplyr::full_join(RDFC_Biomass_Data4, by = c(
          'SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 'SiteName', 'CommonName', 'ScientificName', 'SurveyYear')) %>% 
        dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, ScientificName, CommonName, 
                      Species_Count, Total_Length_cm, Fish_Biomass, Mean_Count, Mean_Density) %>% 
        dplyr::mutate(
          Mean_Count = dplyr::case_when(
            is.na(Mean_Count) & !is.na(Species_Count) ~ Species_Count,
            Mean_Count == 0 & Species_Count > 0 ~ Species_Count,
            is.na(Mean_Count) & is.na(Species_Count) ~ 0,
            !is.na(Mean_Count) ~ Mean_Count),
          Species_Count = dplyr::case_when(
            !is.na(Mean_Count) & is.na(Species_Count) ~ round(Mean_Count, 0),
            is.na(Mean_Count) & is.na(Species_Count) ~ 0,
            !is.na(Species_Count) ~ Species_Count)) %>% 
        dplyr::group_by(CommonName, IslandName) %>% 
        dplyr::mutate(Mean_Isl_Bio = base::mean(Fish_Biomass, na.rm = TRUE)) %>% 
        dplyr::ungroup()%>% 
        dplyr::mutate(
          Fish_Biomass  = dplyr::case_when(
            Mean_Count == 0 & is.na(Fish_Biomass) ~ 0,
            Mean_Count > 0 & is.na(Fish_Biomass) ~ Mean_Isl_Bio,
            !is.na(Fish_Biomass) ~ Fish_Biomass)) %>%
        dplyr::filter(!is.na(Fish_Biomass))  %>%
        tidyr::uncount(weights = Species_Count) %>% 
        dplyr::group_by(SiteNumber, SurveyYear, CommonName) %>% 
        dplyr::mutate(Biomass = base::sum(1/n() * Fish_Biomass * Mean_Count)) %>% 
        dplyr::ungroup() %>% 
        dplyr::distinct(SiteNumber, SurveyYear, CommonName, Biomass, .keep_all = TRUE) %>% 
        dplyr::left_join(siteInfo1) %>% 
        dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, ScientificName, 
                      CommonName, Biomass, Mean_Count, Mean_Density, MeanDepth, Reference, ReserveStatus) %>% 
        dplyr::left_join(Fish_Trophic_Levels)  %>% 
        dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
        tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                                IslandCode, MeanDepth, ReserveStatus, Reference),
                        nesting(ScientificName, CommonName, Broad_Trophic_Level, Targeted), SurveyYear, 
                        fill = list(Biomass = 0, Mean_Density = 0)) %>%
        dplyr::group_by(SiteCode, SiteName, IslandName, ScientificName, CommonName, SurveyYear) %>%
        dplyr::mutate(Mean_Biomass = round(mean(Biomass), 4)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(SiteCode, SiteName, IslandCode, ScientificName, CommonName, 
                        SurveyYear, ReserveStatus, .keep_all = TRUE) %>% 
        dplyr::select(SiteCode, SiteName, IslandCode, IslandName, CommonName, ScientificName, SurveyYear, 
                      Mean_Biomass, ReserveStatus,Reference, Broad_Trophic_Level, Targeted) %>%
        dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                      IslandName = gsub(" Island", "", IslandName)) %>%
        readr::write_csv("Tidy_Data_Dont_Touch/Fish_Biomass_Long.csv")
    }
    
    { # Fish Biomass Wide   -----
      Fish_Biomass_Wide <- Fish_Biomass_Long %>% 
        dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                      CommonName, SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
        dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
        tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0) %>%
        dplyr::left_join(annual_mean_oni, by = c("SurveyYear"))
      names(Fish_Biomass_Wide) <- str_replace_all(names(Fish_Biomass_Wide), c(" " = "_" , "," = "" )) 
      readr::write_csv(Fish_Biomass_Wide, "Tidy_Data_Dont_Touch/Fish_Biomass_Wide.csv") 
    }
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




