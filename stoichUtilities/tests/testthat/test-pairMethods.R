rm(list=ls())

# Load required packages.
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(sf)
library(units)
library(jsonlite)
library(stoichUtilities)
library(testthat)

#### Code for full database
# basePath <- file.path(path.expand("~"), "data", "stoichdb", "currentDB", "STOICH_Beta_Release_2025-09-10")

#### Code for NEON only database for smaller test dataset
basePath <- system.file("testdata", "STOICH_NEON_Only_2025-09-10", package="stoichUtilities")

stoichData <- loadSTOICH(basePath) #|>
  # filterSTOICH(var="FirstAuthor", val="Neon", condition="contains") |> # line not needed for NEON only dataset, but left to make it consistent between datasets
  # filterSTOICH(var="SampleYear", val=as.integer(2018),  condition="less than")

# stoichData[["tbl_Source"]] <- stoichData[["tbl_Source"]] |>
#   filter(str_detect(FirstAuthor, "Neon", negate=TRUE))
# stoichData <- filterJoinSTOICH(stoichData, "tbl_Source")


#### Code to write a NEON only database version
# basePath <- file.path(path.expand("~"), "GitHub", "STOICH-utilities", "stoichUtilities",
#                       "inst", "testdata", "STOICH_NEON_Only_2025-09-10")
# library(stoichCommon)
# if (!dir.exists(basePath)){dirc <- dir.create(basePath, recursive=TRUE)}
# stoichCommon::getStoichMeta() |>
#   dplyr::select(c("SQL_Variable", "table", "xlsx_description", "values", "dataType")) |>
#   tidyr::drop_na(SQL_Variable) |>
#   dplyr::mutate(Units = if_else(stringr::str_to_lower(stringr::str_trim(values)) %in% c("meters", "degrees", "celsius", "percent", "us/cm"),
#                                 stringr::str_trim(values),
#                                 as.character(NA)), .keep="all") |>
#   dplyr::mutate(values = if_else(is.na(Units) | Units == "", values, as.character(NA)), .keep="all") |>
#   dplyr::rename(variable = SQL_Variable) |>
#   dplyr::rename(description = xlsx_description) |>
#   readr::write_csv(file.path(basePath, "STOICH_metadata.csv"))
# for (iTbl in keep(names(stoichData), ~str_detect(.x, "^tbl_"))){
#   write_csv(stoichData[[iTbl]], file.path(basePath, paste0(iTbl, ".csv")))
# }

# pairSummary <- function(dataTbl, timeDiff){
#   return(dataTbl |>
#            mutate(WaterEventId = str_extract(Notes.WaterChemistry, "(?<=SampleEventId = )[0-9]+"), .after="SampleDate.SampleEvent") |>
#            mutate(CheckDate = ymd(str_extract(Notes.WaterChemistry, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))  - SampleDate.SampleEvent < timeDiff, .after="SampleDate.SampleEvent") |>
#            mutate(CheckLat = as.numeric(str_extract_all(Notes.WaterChemistry, "(?<=Latitude = )-?[0-9]+\\.[0-9]*")) - Latitude.Site, .after="SampleDate.SampleEvent") |>
#            mutate(CheckLong = as.numeric(str_extract_all(Notes.WaterChemistry, "(?<=Longitude = )-?[0-9]+\\.[0-9]*")) - Longitude.Site, .after="SampleDate.SampleEvent"))
# }

pairedRows <- function(dataTbl){
  return(dataTbl |>
           mutate(paired = (!is.na(Id.WaterChemistry) | str_detect(Notes.WaterChemistry, "tbl_WaterChemistry::Id")) & !is.na(Id.OrganismStoichiometry)) |>
           filter(paired==TRUE) |>
           nrow())
}

# rawPairs <- pairedRows(joinSTOICH(stoichData))

# check that nothing is paired with the original (NEON doesn't sample water and organisms on the same day)
stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=0, timeUnits="days", distance=0, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH()

expect_equal(pairedRows(joinSTOICH(stoichData)), 0)

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=4, timeUnits="weeks", distance=10, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH()

# check rows are paired for <=1 week & <=5km
expect_equal(pairedRows(stoichPaired), 637)

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=2, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH()

# check rows are paired for <=2 weeks & <=5km
expect_equal(pairedRows(stoichPaired), 588)

stoichPaired <- locateDataPairsSTOICH(stoichData, timeDiff=1, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH()

# check rows are paired for 1 week & <=5km
expect_equal(pairedRows(stoichPaired), 412)

stoichPairedAvg <- locateDataPairsSTOICH(stoichData, timeDiff=3, timeUnits="weeks", distance=5, pairMethod="Avg Water", ignoreExisting=TRUE) %>%
  joinSTOICH()

# View(stoichPairedAvg |>
#        filter(!is.na(Id.OrganismStoichiometry) & Id.WaterChemistry==0) |>
#        select(c(SampleDate.SampleEvent, Type.OrganismStoichiometry, Id.OrganismStoichiometry, contains("WaterChem"))))

stoichPairedMin <- locateDataPairsSTOICH(stoichData, timeDiff=3, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE) %>%
  joinSTOICH()

# check rows are paired for 3 weeks & <=15km and it doesn't change when the pairing method changes
expect_equal(pairedRows(stoichPairedAvg), pairedRows(stoichPairedMin))

# joinSTOICH(stoichData) %>% View()

