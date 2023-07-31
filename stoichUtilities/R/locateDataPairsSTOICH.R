#' @title Pair STOICH Data with Similar Locations & Dates
#'
#' @description
#'   Looks for matches between organism stoichiometry and water chemistry data when the date & time or GPS locations are slightly off.
#'   This could be time intensive so if possible perform any filtering operations before locating data pairs.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#' @param timeDiff The maximum time difference to consider for matches between data.
#' @param timeUnits What units to use for the time difference search (months, weeks, days, hours).
#' @param distance Distance between data points in km.
#' @param pairMethod How to determine the best pair from multiple matches. Options are:
#'   Min Time - Prioritize the minimum time.
#'   Min Dist - Prioritize the minimum distance.
#'   #days=#km - Minimize in space and time using a relation for # days equal # kilometers (enter decimal numbers, i.e. "1.5days=2.1km").
#'   Avg Water - Takes the average for all the water chemistry data returned that matches an organism data entry.
#' @param ignoreExisting Determines if the existing data pairs should be ignored and processed the same as other data.
#'   Probably only useful for averaging water data since the other data points already have matching time and distance data.
#'
#' @return The STOICH data tables after pairs have been found. The water chemistry data will be copied to a new entry with a
#'  SampleEventId matching the organism stoichiometry data. The notes will be updated adding a comment about the original
#'  SampleEventId(s), such as "OriginalSampleEventId=(10, 11, 12)" for an average of 3 water chemistry samples to the Notes
#'  of tbl_WaterChemistry.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' # filtering by table and pairing example with pipes:
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="Latitude", val=c(54.1, 103.1), condition="range") %>%
#'   locateDataPairsSTOICH(timeDiff=2, timeUnits="weeks", distance=1, pairMethod="1.5days=2.1km")
#'
#' stoichTable <- joinSTOICH(stoichFiltered)
#'
#' }
#'
#' @export
locateDataPairsSTOICH <- function(dataTables, timeDiff=7, timeUnits="days", distance=1, pairMethod="Min Time", ignoreExisting=TRUE){
  # Verify the data tables are in the proper format
  verifySTOICH(dataTables)

  # Verify the input values are valid
  if (!is.numeric(timeDiff) | !is.numeric(distance)){
    stop("timeDiff and distance must be numeric values. Please check the input values.")
  }
  # Use a rough estimate to reduce the search pool for distance calculations
  # 1 degree of Latitude < 112km
  latDiff <- distance/112
  # Longitude varies with Latitude so create bins for each 5 degrees of Lat
  longDiff <- c(distance/(40000 * cos(pi*seq(5, 85, by=5)/180)/360), 360)

  if (!is.character(timeUnits)){
    stop("timeUnits must be a string. Please choose from on of the following (or the abbreviation): hours (h), days (d), weeks (w), months (m).")
  }
  # Check the time units and correct if the value wasn't plural.
  timeUnits <- paste(timeUnits, if_else(str_length(timeUnits) == 1, "", if_else(str_detect(timeUnits, "[sS]$"), "", "s")), sep="")
  if (!(tolower(timeUnits) %in% c("hours", "h", "days", "d", "weeks", "w", "months", "m"))){
    stop(paste("timeUnits should be one of the following (or the abbreviation): hours (h), days (d), weeks (w), months (m)"))
  } else if (str_length(timeUnits) == 1){
    timeUnits <- list("h"="hours", "d"="days", "w"="weeks", "m"="months")[[timeUnits]]
  }

  if (!is.character(pairMethod)){
    stop("pairMethod must be a string. Please choose from one of the following options: Min Time, Min Dist, #days=#km, Avg Water.")
  }
  # Check if the pairMethod is valid
  if (!(tolower(str_replace_all(pairMethod, "[0-9\\.]+", "#")) %in% c("min time", "min dist", "#days=#km", "avg water"))){
    stop("An undefined pairMethod was entered. Please choose from one of the following options: Min Time, Min Dist, #days=#km, Avg Water.")
  }

  if (ignoreExisting){
    orgEventIds <- setdiff(unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId),
                        unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId))
    watEventIds <- setdiff(unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId),
                           unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId))
  } else {
    orgEventIds <- unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId)
    watEventIds <- unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId)
  }

  # Create a table of sites to compare GPS coordinates
  sites <- dataTables[["tbl_Site"]] %>%
    tidyr::drop_na(c("Latitude", "Longitude")) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326, remove=FALSE)

  closeSites <- units::drop_units(units::set_units(sf::st_distance(sites$geometry), km))
  siteId2Index <- seq(1, length(sites$Id))
  names(siteId2Index) <- sites$Id

  sites <- sites %>%
    mutate(closeIds = sapply(seq(1, length(sites$Id)), function (x) {list(sites$Id[closeSites[x,] < distance])}), .keep="all") %>%
    mutate(closeDist = sapply(seq(1, length(sites$Id)), function (x) {list(closeSites[x,][closeSites[x,] < distance])}), .keep="all") %>%
    sf::st_drop_geometry()

  tempTables <- dataTables[["tbl_SampleEvent"]] |>
    dplyr::select(!c("Notes")) |>
    rename(SampleEventId = Id) |>
    dplyr::inner_join(sites, by=c("SiteId"="Id")) |>
    dplyr::select(!c("Notes"))

  watTable <- tempTables %>%
    dplyr::filter(SampleEventId %in% watEventIds) %>%
    dplyr::select(!c("closeIds", "closeDist"))

  orgTable <- tempTables %>%
    dplyr::filter(SampleEventId %in% orgEventIds) %>%
    dplyr::mutate(minDate = SampleDate - period(timeDiff, unit=timeUnits), .keep="all") %>%
    dplyr::mutate(maxDate = SampleDate + period(timeDiff, unit=timeUnits), .keep="all") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check = list(date_compare(watTable, c(minDate, maxDate), "range")), .keep="unused") %>%
    dplyr::mutate(watIds = list(watTable$SampleEventId[check]), .keep="all") %>%
    dplyr::mutate(watSiteIds = list(watTable$SiteId[check]), .keep="all") %>%
    dplyr::mutate(watLat = list(watTable$Latitude[check]), .keep="all") %>%
    dplyr::mutate(watLong = list(watTable$Longitude[check]), .keep="all") %>%
    dplyr::mutate(watDate = list(watTable$SampleDate[check]), .keep="all") %>%
    dplyr::mutate(check = any(check)) %>%
    tidyr::unnest(cols=c(watIds, watSiteIds, watLat, watLong, watDate)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check = watSiteIds %in% unlist(closeIds)) %>%
    dplyr::filter(check) %>%
    dplyr::mutate(closeDist = closeDist[sapply(closeIds, function(x){x == watSiteIds})], .keep="all") %>%
    # dplyr::ungroup() %>%
    dplyr::select(!c("closeIds", "check")) %>%
    dplyr::mutate(DateDiff = abs(as.numeric(SampleDate - watDate)), .keep="all") %>%
    dplyr::mutate(watDate = paste(watDate)) %>%
    resolvePairs(pairMethod) %>%
    dplyr::relocate(water.Date, .after="water.SampleEventId") %>%
    mergeWater(dataTables[["tbl_WaterChemistry"]], pairMethod)

  dataTables[["tbl_WaterChemistry"]] <- bind_rows(dataTables[["tbl_WaterChemistry"]], orgTable)

  return(dataTables)
}

# Function to handle the the resolution of multiple pairs within the search range.
# "min time", "min dist", "#days=#km", "avg water"
resolvePairs <- function(dataTable, pairMethod){
  if (tolower(pairMethod) == "min time"){
    dataTable <- dataTable %>%
      dplyr::group_by(SampleEventId, SampleDate, SiteId, Latitude, Longitude) %>%
      dplyr::summarise(water.Id=list(watIds), water.SiteId=list(watSiteIds), water.Lat=list(watLat), water.Long=list(watLong), water.Date=list(watDate), water.days=list(DateDiff), water.Dist=list(closeDist), water.index=which.min(DateDiff), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(pairMethod) == "min dist"){
    dataTable <- dataTable %>%
      dplyr::group_by(SampleEventId, SampleDate, SiteId, Latitude, Longitude) %>%
      dplyr::summarise(water.Id=list(watIds), water.SiteId=list(watSiteIds), water.Lat=list(watLat), water.Long=list(watLong), water.Date=list(watDate), water.days=list(DateDiff), water.Dist=list(closeDist), water.index=which.min(closeDist), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(stringr::str_replace_all(pairMethod, "[0-9\\.]+", "#")) == "#days=#km"){
    timeDist = stringr::str_extract_all(pairMethod, "[0-9\\.]+")[[1]]
    timeDistRatio = as.numeric(timeDist[1])/as.numeric(timeDist[2])
    dataTable <- dataTable %>%
      dplyr::mutate(DateDiff = DateDiff + closeDist*timeDistRatio) %>%
      dplyr::group_by(SampleEventId, SampleDate, SiteId, Latitude, Longitude) %>%
      dplyr::summarise(water.Id=list(watIds), water.SiteId=list(watSiteIds), water.Lat=list(watLat), water.Long=list(watLong), water.Date=list(watDate), water.days=list(DateDiff), water.Dist=list(closeDist), water.index=which.min(DateDiff), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(pairMethod) == "avg water"){
    dataTable <- dataTable %>%
      dplyr::mutate(watDate = as.numeric(lubridate::as_datetime(watDate)), .keep="all") %>%
      dplyr::group_by(SampleEventId, SampleDate, SiteId, Latitude, Longitude) %>%
      # dplyr::summarise(water.SampleEventId=str_flatten(watIds, ", "), water.SiteId=str_flatten(watSiteIds, ", "), water.Lat=mean(watLat), water.Long=mean(watLong), water.Date=mean(lubridate::as_datetime(watDate)), water.days=mean(DateDiff), water.Dist=mean(closeDist), .groups="keep") %>%
      dplyr::summarise(water.SampleEventId=watIds, water.SiteId=watSiteIds, water.Lat=mean(watLat), water.Long=mean(watLong), water.Date=mean(lubridate::as_datetime(watDate)), water.days=mean(DateDiff), water.Dist=mean(closeDist), .groups="keep") %>%
      dplyr::ungroup()
  }

  if (tolower(tolower(stringr::str_replace_all(pairMethod, "[0-9\\.]+", "#")) %in% c("min time", "min dist", "#days=#km"))){
    dataTable <- dataTable %>%
      dplyr::rowwise() %>%
      dplyr::mutate(water.SiteId = water.SiteId[water.index], .keep="all") %>%
      dplyr::mutate(water.Lat = water.Lat[water.index], .keep="all") %>%
      dplyr::mutate(water.Long = water.Long[water.index], .keep="all") %>%
      dplyr::mutate(water.Date = water.Date[water.index], .keep="all") %>%
      dplyr::mutate(water.days = water.days[water.index], .keep="all") %>%
      dplyr::mutate(water.Dist = water.Dist[water.index], .keep="all") %>%
      dplyr::mutate(water.SampleEventId = water.Id[water.index], .keep="unused", .after="Longitude")
  }

  return(dataTable)
}

mergeWater <- function(dataTable, waterTable, pairMethod){
  dataTable <- dataTable %>%
    dplyr::left_join(dplyr::rename_with(waterTable, ~str_c(., ".water"), everything()), by=c("water.SampleEventId"="SampleEventId.water"))

  if (tolower(pairMethod) == "avg water"){
    ###############################################################################
    # Raise an error until unit conversions are ready
    ###############################################################################
    stop("avg water isn't ready due to unit conversions")

    varList <- list("DO"="DOUnits", "DOC"="DOCUnits", "NH4"="NH4Units",
                    "SRP"="SRPUnits", "NO3"="NO3Units",
                    "TDN"="TDNUnits", "TN"="TNUnits",
                    "TDP"="TDPUnits", "TP"="TPUnits")

    dataTable <- dataTable %>%
      dplyr::rowwise() %>%
      dplyr::mutate(DO = if_else(DOUnits %in% varUnitsList("DOUnits"), units::units(DO.water) <- as_units(DOUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(DOCUnits %in% varUnitsList("DOCUnits"), units::set_units(DOC.water) <- as_units(DOCUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(NH4Units %in% varUnitsList("NH4Units"), units::set_units(NH4.water) <- as_units(NH4Units.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(SRPUnits %in% varUnitsList("SRPUnits"), units::set_units(SRP.water) <- as_units(SRPUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(NO3Units %in% varUnitsList("NO3Units"), units::set_units(NO3.water) <- as_units(NO3Units.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(TDNUnits %in% varUnitsList("TDNUnits"), units::set_units(TDN.water) <- as_units(TDNUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(TNUnits %in% varUnitsList("TNUnits"), units::set_units(TN.water) <- as_units(TNUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(TDPUnits %in% varUnitsList("TDPUnits"), units::set_units(TDP.water) <- as_units(TDPUnits.water), as.numeric(NA))) %>%
      dplyr::mutate(DO = if_else(TPUnits %in% varUnitsList("TPUnits"), units::set_units(TP.water) <- as_units(TPUnits.water), as.numeric(NA))) %>%
      dplyr::group_by(SampleEventId, SampleDate, SiteId, Latitude, Longitude) %>%
      dplyr::summarise(SampleEventId.water = paste("SampleEventIds = <", str_flatten(water.SampleEventId, ", "), ">", sep=""),
                       SpecConductivity.water = mean(SpecConductivity.water),
                       pHValue.water = mean(pHValue.water),
                       DO.water = mean(DO.water),
                       DOC.water = mean(DOC.water))
  }

  dataTable <- dataTable %>%
    dplyr::mutate(Notes.water = paste("Paired water chemistry data & original Ids: <SampleEventId = ", water.SampleEventId,
                                      ", SampleDate = ", water.Date, ", SiteId = ", water.SiteId,
                                      ", Latitude = ", water.Lat, ", Longitude = ", water.Long,
                                      ", tbl_WaterChemistry::Id = ", Id.water,
                                      "> ", Notes.water, sep=""), .keep="all") %>%
    dplyr::mutate(Id.water = as.numeric(NA)) %>%
    dplyr::select(!c(water.SampleEventId, water.Date, water.SiteId, water.Lat, water.Long,
                     water.days, water.Dist,
                     SampleDate, SiteId, Latitude, Longitude)) %>%
    dplyr::rename(SampleEventId.water = SampleEventId) %>%
    dplyr::rename_with(~stringr::str_replace(., "\\.water", ""), everything()) %>%
    dplyr::relocate(Id, .before = SampleEventId)

  return(dataTable)
}

varUnitsList <- function(metadataTable, varName){
  varUnits <- str_replace_all(tolower(str_trim(str_split((stoichData[["metadata"]] %>%
                                                          filter(variable==varName))$values, ",")[[1]])), "_", "/")
  return(varUnits[varUnits != "other" & varUnits != "na"])
}

