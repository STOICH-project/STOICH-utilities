rm(list=ls())

# Load required packages.
library(tidyverse)
library(stoichUtilities)
library(lubridate)

myPath <- file.path(path.expand("~"), "data", "stoichdb", "currentDB", "STOICH_Beta_Release_2023-07-21")

stoichData <- stoichUtilities::loadSTOICH(dataPath=myPath)

stoichTable <- stoichData |>
#   stoichUtilities::filterSTOICH(var="SampleDate", val="20080402", condition="greater than") |>
#   stoichUtilities::filterSTOICH(var="SampleDate", val="20090402", condition="less than") |>
#   stoichUtilities::filterSTOICH(var="State", val="Florida", condition="not equal") |>
    stoichUtilities::filterSTOICH(var="EcosystemType", val="Salt Marsh", condition="equal")
#   # Join all the tables into one large wide table
#   stoichUtilities::joinSTOICH()
#
# # Match organism stoichiometry data with water chemsitry data for samples that weren't taken at the exact same time
# stoichPaired <- stoichUtilities::locateDataPairsSTOICH(stoichData, timeDiff=2, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE)

nrow(stoichTable$tbl_OrganismStoichiometry)
nrow(stoichTable$tbl_WaterChemistry)



stoichData$tbl_InputFile = tibble(Id = c(1))

stoichData$tbl_Contact = tibble(Id = c(1))

stoichData$tbl_Source = tibble(Id=c(1), ContactId=c(1))

stoichData$tbl_Site = tibble(Latitude=c(40.9008, 40.9008, 40.9008, 40.9808),
                             Longitude=c(-96.719894, -96.739117, -96.758,-96.758)) |>
  tibble::rowid_to_column("Id") |>
  mutate(Id = Id + 10) |>
  mutate(Notes=as.character(NA))

# Site 14 is about 5.6 miles or ~9km from the closest other point
stoichData$tbl_SampleEvent = tibble(SiteId=c(11, 12, 13, 11, 14, 11, 14),
                                    SampleDate=c("20220510", "20220525", "20220606", "20220608", "20220525", "20220609", as.character(NA))) |>
  mutate(SampleYear = year(ymd(SampleDate))) |>
  mutate(SampleMonth = month(ymd(SampleDate))) |>
  mutate(SampleDay = day(ymd(SampleDate))) |>
  mutate(SourceId = 1) |>
  mutate(InputFileId = 1) |>
  tibble::rowid_to_column("Id") |>
  mutate(Id = Id + 1000) |>
  mutate(SampleDate = ymd(SampleDate)) |>
  mutate(Notes=as.character(NA))

stoichData$tbl_OrganismStoichiometry = tibble(SampleEventId=c(1001,1002,1003,1004,1007)) |>
  tibble::rowid_to_column("Id") |>
  mutate(Id = Id + 200) |>
  mutate(Notes=as.character(NA))

stoichData$tbl_WaterChemistry = tibble(SampleEventId=c(1005, 1006)) |>
  tibble::rowid_to_column("Id") |>
  mutate(Id = Id + 300) |>
  mutate(Notes=as.character(NA))

# dataTables <- stoichData

pairSummary <- function(dataTbl, timeDiff){
  return(dataTbl |>
           mutate(WaterEventId = str_extract(Notes.WaterChemistry, "(?<=SampleEventId = )[0-9]+"), .after="SampleDate.SampleEvent") |>
           mutate(CheckDate = ymd(str_extract(Notes.WaterChemistry, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))  - SampleDate.SampleEvent < timeDiff, .after="SampleDate.SampleEvent") |>
           mutate(CheckLat = as.numeric(str_extract_all(Notes.WaterChemistry, "(?<=Latitude = )-?[0-9]+\\.[0-9]*")) - Latitude.Site, .after="SampleDate.SampleEvent") |>
           mutate(CheckLong = as.numeric(str_extract_all(Notes.WaterChemistry, "(?<=Longitude = )-?[0-9]+\\.[0-9]*")) - Longitude.Site, .after="SampleDate.SampleEvent"))
}

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=2, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH() |>
  pairSummary(weeks(2))

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=3, timeUnits="weeks", distance=15, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH() |>
  pairSummary(weeks(3))

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=4, timeUnits="weeks", distance=15, pairMethod="Min Dist", ignoreExisting=TRUE) %>%
  joinSTOICH() |>
  pairSummary(weeks(4))

joinSTOICH(stoichData) %>% View()

# joinSTOICH(stoichData)
