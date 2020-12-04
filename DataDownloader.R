library(lubridate)
library(data.table)
library(dplyr)
library(beepr)

#grab file 
loc <- "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv"
df <- fread(input = loc, encoding = 'UTF-8',data.table = F)


# Clean up PHU Names 
df <- df %>% 
  dplyr::mutate(Reporting_PHU = case_when(
    .$Reporting_PHU == "Huron Perth District Health Unit" ~ "Huron Perth Health Unit", 
    .$Reporting_PHU == "Kingston, Frontenac and Lennox & Addington Public Health" ~ "Kingston, Frontenac and Lennox and Addington Health Unit", 
    .$Reporting_PHU == "Sudbury & District Health Unit" ~ "Sudbury and District Health Unit", 
    .$Reporting_PHU == "Wellington-Dufferin-Guelph Public Health" ~ "Wellington-Dufferin-Guelph Health Unit", 
    .$Reporting_PHU == "York Region Public Health Services" ~ "York Region Public Health", 
    TRUE ~ .$Reporting_PHU)) %>% 
  dplyr::mutate(Reporting_PHU = trimws(Reporting_PHU, which = "both"))



# old method using a nested if-else loop
# CaseStatus$Reporting_PHU <- ifelse(CaseStatus$Reporting_PHU == "Huron Perth District Health Unit", "Huron Perth Health Unit", 
#                                    ifelse(CaseStatus$Reporting_PHU == "Kingston, Frontenac and Lennox & Addington Public Health", "Kingston, Frontenac and Lennox and Addington Health Unit",
#                                           ifelse(CaseStatus$Reporting_PHU == "Sudbury & District Health Unit", "Sudbury and District Health Unit", 
#                                                  ifelse(CaseStatus$Reporting_PHU == "Wellington-Dufferin-Guelph Public Health", "Wellington-Dufferin-Guelph Health Unit", 
#                                                         ifelse(CaseStatus$Reporting_PHU == "York Region Public Health Services", "York Region Public Health", CaseStatus$Reporting_PHU)
#                                                  )
#                                           )
#                                    )
# )


# Aggregating the data to gather the total covid cases per PHU 
CaseStatus <- df %>% 
  dplyr::group_by(Reporting_PHU, 
                  Reporting_PHU_Latitude, Reporting_PHU_Longitude, 
                  Reporting_PHU_Address, 
                  Reporting_PHU_City, Reporting_PHU_Postal_Code, Reporting_PHU_Website) %>% 
  dplyr::count(name = "Total_Cases") %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Reporting_PHU) %>% 
  #reordering fields 
  dplyr::select(Reporting_PHU, Reporting_PHU_Website, Reporting_PHU_Address, Reporting_PHU_City, Reporting_PHU_Postal_Code, Reporting_PHU_Latitude, Reporting_PHU_Longitude, Total_Cases) %>% 
  dplyr::rename(Website = Reporting_PHU_Website,
                Address = Reporting_PHU_Address, 
                City = Reporting_PHU_City, 
                Postal_Code = Reporting_PHU_Postal_Code, 
                Lat = Reporting_PHU_Latitude, 
                Lon = Reporting_PHU_Longitude) %>% 
  # Adding a Total_Cases percentage field 
  # this will denote the total cases relative to the sum of all cases in our dataset 
  dplyr::mutate(TC_Percentage = as.numeric(round(Total_Cases / sum(Total_Cases) * 100, 1))) %>% 
  data.frame()

# Now we need to Add the Outcomes 
Outcomes<- df %>% 
  dplyr::group_by(Reporting_PHU, Outcome1) %>% 
  dplyr::summarise(count = n(), .groups = 'keep') %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(Outcome1, count, fill = 0) %>% 
  data.frame()  %>% 
  dplyr::arrange(Reporting_PHU) %>% 
  dplyr::rename(Total_Deaths = Fatal, 
                Total_Recovered = Resolved, 
                Total_Actives = Not.Resolved) 


# Appending Results to Dataset 
CaseStatus <- CaseStatus %>% 
  dplyr::left_join(Outcomes, by = "Reporting_PHU") %>% 
  #adding a Percentage fields 
  dplyr::mutate(TD_Pct = as.numeric(round(Total_Deaths / Total_Cases*100,1)), 
                TR_Pct = as.numeric(round(Total_Recovered / Total_Cases*100,1)),
                TA_Pct = as.numeric(round(Total_Actives / Total_Cases*100,1)),)


# Aggregate Case Acquisition Info 
AcquisitionType<- df %>% 
  dplyr::group_by(Reporting_PHU, Case_AcquisitionInfo) %>% 
  dplyr::summarise(count = n(), .groups = 'keep') %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(Case_AcquisitionInfo, count, fill = 0) %>% 
  data.frame()  %>% 
  dplyr::arrange(Reporting_PHU) %>% 
  dplyr::rename(CloseContact = CC, 
                Outbreak = OB, 
                Missing_Information = Missing.Information, 
                No_Known_EpiLink= No.known.epi.link, 
                Unspecified_EpiLink = Unspecified.epi.link) %>% 
  #creating an Unknown field by almgamating Missing_Information + No_Known_EpiLink + Unspecified_EpiLink
  dplyr::mutate(Unknown = Missing_Information + No_Known_EpiLink + Unspecified_EpiLink) %>% 
  #dropping fields 
  dplyr::select(-Missing_Information, -No_Known_EpiLink, -Unspecified_EpiLink)

CaseStatus <- CaseStatus %>% 
  dplyr::left_join(AcquisitionType, by = "Reporting_PHU") 

CaseStatus <- CaseStatus %>% 
  dplyr::mutate(CloseContact_Pct = round(CloseContact/Total_Cases*100,1), 
                Outbreak_Pct = round(Outbreak/Total_Cases*100,1), 
                Travel_Pct = round(Travel/Total_Cases*100,1), 
                Unknown_Pct = round(Unknown/Total_Cases*100, 1))


# Adding per Capita fields 

# adding population and areal data 
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

CaseStatus <- data.frame(CaseStatus, Population, LandSizeSqKM)

# Adding per Capita fields 
# We'll use 100,000 as the scale to which we tweak our values to since that is closest to population median of our dataset 

CaseStatus <- CaseStatus %>% 
  dplyr::mutate(
    #outcomes per capita 
    Cases_pC = round((Total_Cases / Population)*100000,0), 
    Deaths_pC = round((Total_Deaths / Population)*100000,0),
    Actives_pC = round((Total_Actives / Population)*100000,0),
    Recovered_pC = round((Total_Recovered / Population)*100000,0),
    #acquistions per capita 
    CC_pC = round((CloseContact / Population)*100000,0), 
    OB_pC = round((Outbreak / Population)*100000,0), 
    Tr_pC = round((Travel / Population)*100000,0), 
    unknwn_pC = round((Unknown / Population)*100000,0))











PHU_csums <- CaseStatus





#write data to drive 
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

