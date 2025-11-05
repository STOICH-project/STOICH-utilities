# verify nothing is loaded
rm(list=ls())

library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(sf)
library(units)
library(testthat)

library(stoichUtilities)

myPath <- system.file("testdata", "STOICH_NEON_Only_2025-09-10", package="stoichUtilities")
# dbVersionList <- list.files(myPath)
# dbCurrentVersion <- keep(dbVersionList, ~str_detect(.x, paste0(max(str_extract(dbVersionList, "[0-9]{4}-[0-9]{2}-[0-9]{2}"), na.rm=TRUE), "(?!.*\\.zip$)")))
# myPath <- file.path(myPath, dbCurrentVersion)

expect_error(stoichUtilities::loadSTOICH(dataPath=file.path(path.expand("~"), "Bad Directory Should Cause Failure")))

stoichData <- stoichUtilities::loadSTOICH(dataPath=myPath)

# Test the filter function
expect_true(all(stoichUtilities::filterSTOICH(stoichData,
                                              var="State",
                                              val="North Dakota",
                                              condition="equal")$tbl_Site$EcosystemType=="Lake"))

expect_true(all(unique(stoichUtilities::filterSTOICH(stoichData,
                                                     var="EcosystemType",
                                                     val="River",
                                                     condition="equal")$tbl_OrganismStoichiometry$Type)==c("Seston", "Periphyton")))

# if there is a problem with the output there should be an error
expect_no_error(
  filterSTOICH(dataTables=stoichData, var="Type.OrganismStoichiometry", val="Seston", condition="Equal") |>
    filterSTOICH(var="FirstAuthor", val="Neon", condition="Contains") |>
    filterSTOICH(var="SampleYear", val=as.integer(c(2011, 2018)), condition="Range") |>
    filterSTOICH(var="SampleMonth", val=as.integer(7), condition="Less Than") |>
    filterSTOICH(var="SampleMonth", val=as.integer(5), condition="Greater Than") |>
    filterSTOICH(var="State", val="Alaska", condition="Not Equal") |>
    joinSTOICH()
)

expect_error(filterSTOICH(stoichData,
                          var="EcosystemType",
                          val="Ocean",
                          condition="Equal"))
