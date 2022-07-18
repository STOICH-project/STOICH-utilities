# verify nothing is loaded
rm(list=ls())

library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(sf)
library(units)

library(stoichUtilities)

basePath <- do.call(file.path, as.list(str_split(Sys.getenv("HOME"), "\\\\")[[1]]))

stoichData <- loadSTOICH(dataPath=file.path(basePath, "data"))

# if there is a problem with the output there should be an error
stoichFiltered <- filterSTOICH(dataTables=stoichData, var="TrophicMode", val="photoautotroph", condition="equal")
stoichFiltered <- filterSTOICH(dataTables=stoichData, tableVar="tbl_OrganismStochiometry", var="Type", val="seston", condition="equal")
stoichFiltered <- filterSTOICH(dataTables=stoichData, var="Latitude", val=c(54.1, 103.1), condition="range")

stoichTable <- joinSTOICH(stoichFiltered)

rm(stoichFiltered)

# For testing the locateDataPairsSTOICH function
dataTables <- stoichData
timeDiff <- 2
timeUnits <- "weeks"
distance <- 2
pairMethod <- "Min Time"
ignoreExisting <- TRUE
