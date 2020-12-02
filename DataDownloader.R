library(lubridate)
library(data.table)
library(dplyr)
library(beepr)

#grab file 
loc <- "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv"
df <- fread(input = loc, encoding = 'UTF-8',data.table = F)

#data dict URL 
#https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario
#end_time <- Sys.time()
#end_time - start_time

CaseStatus <- df %>% 
  dplyr::group_by(Reporting_PHU, Reporting_PHU_Latitude, Reporting_PHU_Longitude) %>% 
  dplyr::count(name = "Total_Cases") %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Reporting_PHU) %>% 
  dplyr::mutate(Reporting_PHU = trimws(Reporting_PHU, which = "both")) %>% 
  # dplyr::mutate(Reporting_PHU = case_when(
  #   Reporting_PHU == "Huron Perth District Health Unit" ~ "Huron Perth Health Unit", 
  #   Reporting_PHU == "Kingston, Frontenac and Lennox & Addington Public Health" ~ "Kingston, Frontenac and Lennox and Addington Health Unit",
  #   Reporting_PHU == "Sudbury & District Health Unit" ~ "Sudbury and District Health Unit", 
  #   Reporting_PHU == "Wellington-Dufferin-Guelph Public Health" ~ "Wellington-Dufferin-Guelph Health Unit", 
  #   Reporting_PHU == "York Region Public Health Services" ~ "York Region Public Health",
  #   TRUE ~ NA_real_)) %>% 
  data.frame()

CaseStatus$Total_Cases_Pct <- round(CaseStatus$Total_Cases/sum(CaseStatus$Total_Cases)*100,1)

#fixing 5 rows that the PHU name is not verbatim as in the shp boundary file 
PHU_csums$Reporting_PHU <- ifelse(PHU_csums$Reporting_PHU == "Huron Perth District Health Unit", "Huron Perth Health Unit", 
                                  ifelse(PHU_csums$Reporting_PHU == "Kingston, Frontenac and Lennox & Addington Public Health", "Kingston, Frontenac and Lennox and Addington Health Unit",
                                         ifelse(PHU_csums$Reporting_PHU == "Sudbury & District Health Unit", "Sudbury and District Health Unit", 
                                                ifelse(PHU_csums$Reporting_PHU == "Wellington-Dufferin-Guelph Public Health", "Wellington-Dufferin-Guelph Health Unit", 
                                                       ifelse(PHU_csums$Reporting_PHU == "York Region Public Health Services", "York Region Public Health", PHU_csums$Reporting_PHU)
                                                )
                                         )
                                  )
)

#add population and areal data 
Population <- c(113084,134943,102042,645862,202762,
                161977,109652,179083,548430,536917,
                161180,136093,193363,126638,169244,
                455526,447888,123820,76455,934243,
                1381744,138236,84201,535154,103593,
                540249,953261,196448,151884,33049,
                2731571,284461,398953,1109909)
LandSizeSqKM <- c(41266.67, 1128.88,2470.52,2523.80,5314.34,
                  8603.70,2859.09,9065.60,964.04,1117.29,
                  7154.41,5617.79,6627.44,3002.25,6418.52,
                  3317.27,1854.23,16937.78,173828.16,2790.30,
                  1246.96,3848.20,271922.40,1368.92,15032.23,
                  8800.12,20917.77,46551.02,230610.23,14146.28,
                  630.20,4147.01,1850.90,1762.13)

PHU_csums <- data.frame(PHU_csums, Population, LandSizeSqKM)



#create .csv for each date of dataset
dirPath <- paste(getwd(), "/dataset/", sep = "")

#Delete files if exists 
do.call(file.remove, list(list.files(dirPath, full.names = TRUE)))
fileName = paste(dirPath, 'PHU_Summary.csv',sep = '')
#Write Case Status File 
write.csv(PHU_csums, 
          file = fileName, 
          fileEncoding = 'UTF-8', 
          row.names = F)

beep(sound = 1, expr = NULL)

print(paste("Download Completed"))
remove(list = ls())
