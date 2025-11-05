items <- function(x){
  return(unique(discard(x, is.na)))
}

cleanDOUnits <- function(u){
  if (any(!is.na(u))){
    u <- u[!is.na(u)]
    if (any(u=="mg_L")){
      return("mg_L")
    }
    return((summarize(group_by(tibble("x"=u), .data$x), n=n()) |> filter(n==max(n)))[["x"]])
  }
  return(NA)
}

cleanDO <- function(x, u, preferedU){
  x <- x[!is.na(u)]
  u <- u[!is.na(u)]
  if (any(!is.na(u))){
    # preferedU <- cleanDOUnits(u)
    x <- x[u==preferedU]
    u <- u[u==preferedU]
    return(cleanX(x, u))
  }
  return(NA)
}

cleanX <- function(x, u){
  return(ifelse(all(is.na(x)), NA,
                ifelse(length(items(u))==1,
                       ifelse(all(items(x) < -999), -999999999,
                              mean(discard(discard(x, is.na), ~.x < -999))), NA)))
}

cleanU <- function(u){
  return(ifelse(length(items(u))==1, items(u), NA))
}

cleanList <- function(x){
  x <- as.list(unlist(x))
  if (length(unique(names(x)))<length(names(x))){
    for (i in unique(names(x))){
      if (ifelse(is.na(i), FALSE, str_length(i)>0)){
        iNames <- str_detect(names(x), i)
        if (sum(iNames) > 1){
          temp <- NULL
          if (sum(iNames) == length(names(x))){
            temp[[i]] <- unname(unlist(x[iNames]))
          } else {
            temp <- x[!iNames]
            temp[[i]] <- unname(unlist(x[iNames]))
          }
          x <- temp
        }
      }
    }
  }
  return(x)
}

cleanJSON <- function(x){
  x <- discard(discard(x, is.na), ~.x=="")
  if (is_empty(x) | all(is.na(x))){
    return(NA)
    # return(jsonlite::toJSON(list()))
  }
  build_jsonArray <- function(x,y){
    if (is.null(y)){
      return(c(x))
    } else if (is.na(y)){
      return(c(x))
    } else if (str_detect(y, "[{}]", negate=TRUE)){
      return(c(x, list("text"=y))) # y is just text and doesn't contain JSON so don't convert it
    }
    return(c(x, jsonlite::fromJSON(y)))
  }
  temp <- discard(reduce(c(NA, str_split(str_c(x, sep=", ", collapse=", "), ", ")[[1]]),
                         build_jsonArray), is.na)
  if (is_empty(temp)){
    return(as.character(NA))
  }
  return(stringr::str_replace_all(jsonlite::toJSON(cleanList(temp)), '(\\\\){1,}"', "\""))
}


condense <- function(var, x, u){
  if (all(is.na(u)) | length(x)==1){
    return(NA)
  } else if (length(items(u))==1){
    temp <- list("x"=discard(x, is.na), "u"=items(u))
  } else {
    temp <- list("x"=discard(x, is.na), "u"=discard(u, is.na))
  }
  names(temp) <- c(var, paste0(var, "Units"))
  if (length(temp)==0){
    return(NA)
  } else {
    return(jsonlite::toJSON(temp))
  }
}


#' @title Pair STOICH Data with Similar Locations & Dates
#'
#' @description
#'   Looks for matches between organism stoichiometry and water chemistry data when the
#'   date & time or GPS locations are slightly off.  This could be time intensive so if
#'   possible perform any filtering operations before locating data pairs.
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
#'   #days=#km - Minimize in space and time using a relation for # days equal # kilometers
#'   (enter decimal numbers, i.e. "1.5days=2.1km").
#'   Avg Water - Takes the average for all the water chemistry data returned that matches an
#'   organism data entry.
#' @param ignoreExisting Determines if the existing data pairs should be ignored and processed
#'   the same as other data.  Probably only useful for averaging water data since the other
#'   data points already have matching time and distance data.
#'
#' @return The STOICH data tables after pairs have been found. The water chemistry data will be
#'   copied to a new entry with a SampleEventId matching the organism stoichiometry data. The
#'   notes will be updated adding a comment about the original SampleEventId(s), such as
#'   "OriginalSampleEventId=(10, 11, 12)" for an average of 3 water chemistry samples to the
#'   Notes of tbl_WaterChemistry.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' # filtering by table and pairing example with pipes:
#' stoichFiltered <- stoichData |>
#'   filterSTOICH(var="Latitude", val=c(54.1, 103.1), condition="range") |>
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
    orgEventIds <- unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId)
    watEventIds <- unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId)
  } else {
    orgEventIds <- setdiff(unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId),
                           unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId))
    watEventIds <- unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId)
    # watEventIds <- setdiff(unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId),
    #                        unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId))
  }

  if (length(orgEventIds)==0){
    return(dataTables) # Don't continue if no organisms will be paired (already paired)
  }

  # Create a table of sites to compare GPS coordinates
  sites <- dataTables[["tbl_Site"]] %>%
    tidyr::drop_na(c("Latitude", "Longitude")) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326, remove=FALSE)

  closeSites <- units::drop_units(set_units(sf::st_distance(sites$geometry), "km"))
  siteId2Index <- seq(1, length(sites$Id))
  names(siteId2Index) <- sites$Id

  sites <- sites %>%
    mutate(closeIds = sapply(seq(1, length(sites$Id)), function (x) {list(sites$Id[closeSites[x,] < distance])}), .keep="all") %>%
    mutate(closeDist = sapply(seq(1, length(sites$Id)), function (x) {list(closeSites[x,][closeSites[x,] < distance])}), .keep="all") %>%
    sf::st_drop_geometry()

  if (none((rowwise(sites) |> mutate(test=length(.data$closeIds)))$test, ~.x>0)){
    return(dataTables) # Don't continue if there aren't any close sites
  }

  tempTables <- dataTables[["tbl_SampleEvent"]] |>
    dplyr::select(!c("Notes")) |>
    rename(SampleEventId = "Id") |>
    dplyr::inner_join(sites, by=c("SiteId"="Id")) |>
    dplyr::select(!c("Notes"))

  watTable <- tempTables %>%
    dplyr::filter(.data$SampleEventId %in% watEventIds) %>%
    dplyr::select(!c("closeIds", "closeDist"))

  orgTable <- tempTables %>%
    dplyr::filter(.data$SampleEventId %in% orgEventIds) |>
    rowwise() |>
    mutate(check = list(datesInRange(.data$SampleYear, .data$SampleMonth, .data$SampleDay,
                                     watTable$SampleYear, watTable$SampleMonth, watTable$SampleDay,
                                     timeDiff, timeUnits))) |>
    dplyr::mutate(watIds = list(watTable$SampleEventId[.data$check]), .keep="all") %>%
    dplyr::mutate(watSiteIds = list(watTable$SiteId[.data$check]), .keep="all") %>%
    dplyr::mutate(watLat = list(watTable$Latitude[.data$check]), .keep="all") %>%
    dplyr::mutate(watLong = list(watTable$Longitude[.data$check]), .keep="all") %>%
    dplyr::mutate(watDate = list(watTable$SampleDate[.data$check]), .keep="all") %>%
    dplyr::mutate(check = any(.data$check)) %>%
    tidyr::unnest(cols=c("watIds", "watSiteIds", "watLat", "watLong", "watDate")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(check = .data$watSiteIds %in% unlist(.data$closeIds)) %>%
    dplyr::filter(.data$check) %>%
    dplyr::mutate(closeDist = .data$closeDist[sapply(.data$closeIds, function(x){x == .data$watSiteIds})], .keep="all") %>%
    # dplyr::ungroup() %>%
    dplyr::select(!c("closeIds", "check")) %>%
    dplyr::mutate(DateDiff = abs(as.numeric(.data$SampleDate - .data$watDate)), .keep="all") %>%
    dplyr::mutate(watDate = paste(.data$watDate))

  if (tolower(stringr::str_replace_all(pairMethod, "[0-9\\.]+", "#")) %in% c("min time", "min dist", "#days=#km")){
    orgTable2 <- orgTable %>%
      resolvePairs(pairMethod) %>%
      dplyr::rename(SampleEventId.water = "water.Id") %>%
      dplyr::left_join(dplyr::rename_with(dataTables[["tbl_WaterChemistry"]], ~str_c(., ".water"), everything()), by=c("SampleEventId.water"))
  } else if (tolower(pairMethod) == "avg water"){
    orgTable2 <- orgTable %>%
      dplyr::left_join(dplyr::rename_with(dataTables[["tbl_WaterChemistry"]], ~str_c(., ".water"), everything()), by=c("watIds"="SampleEventId.water")) |>
      resolvePairs(pairMethod) %>%
      # dplyr::mutate(WaterSampleId.water = cleanJSON(WaterSampleId.water)) %>%
      dplyr::rename(SampleEventId.water = "water.SampleEventId") %>%
      dplyr::relocate("water.Date", .after="SampleEventId.water")
  }

  orgTable3 <- orgTable2 |>
    rowwise() |>
    dplyr::mutate(Notes.water=paste0("SampleEvent.Id=[", str_c(.data$SampleEventId.water, sep=", ", collapse=", "), "], ", .data$Notes.water)) |>
    dplyr::mutate(SampleEventId.water=.data$SampleEventId) |>
    select(contains(".water")) |>
    dplyr::mutate(Id.water=ifelse(length(unique(.data$Id.water))>1, 0, unique(.data$Id.water))) |>
    dplyr::mutate(across(everything(), ~ifelse(length(unique(.x))>1, NA, unique(.x)))) |>
    dplyr::rename_with(~str_remove(., ".water$"), everything())

  dataTables[["tbl_WaterChemistry"]] <- bind_rows(filter(dataTables[["tbl_WaterChemistry"]], !(.data$SampleEventId %in% orgTable3$SampleEventId)), orgTable3)

  return(dataTables)
}

# Function to handle the the resolution of multiple pairs within the search range.
# "min time", "min dist", "#days=#km", "avg water"
resolvePairs <- function(dataTable, pairMethod){

  if (tolower(pairMethod) == "min time"){
    dataTable <- dataTable %>%
      dplyr::group_by(pick(any_of(c("SampleEventId", "SampleDate", "SiteId", "Latitude", "Longitude")))) %>%
      dplyr::summarise(water.Id=list(.data$watIds), water.SiteId=list(.data$watSiteIds), water.Lat=list(.data$watLat), water.Long=list(.data$watLong), water.Date=list(.data$watDate), water.days=list(.data$DateDiff), water.Dist=list(.data$closeDist), water.index=ifelse(length(unique(.data$DateDiff))==1, which.min(.data$closeDist), which.min(.data$DateDiff)), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(pairMethod) == "min dist"){
    dataTable <- dataTable %>%
      dplyr::group_by(pick(any_of(c("SampleEventId", "SampleDate", "SiteId", "Latitude", "Longitude")))) %>%
      dplyr::summarise(water.Id=list(.data$watIds), water.SiteId=list(.data$watSiteIds), water.Lat=list(.data$watLat), water.Long=list(.data$watLong), water.Date=list(.data$watDate), water.days=list(.data$DateDiff), water.Dist=list(.data$closeDist), water.index=ifelse(length(unique(.data$closeDist))==1, which.min(.data$DateDiff), which.min(.data$closeDist)), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(stringr::str_replace_all(pairMethod, "[0-9\\.]+", "#")) == "#days=#km"){
    timeDist = stringr::str_extract_all(pairMethod, "[0-9\\.]+")[[1]]
    timeDistRatio = as.numeric(timeDist[1])/as.numeric(timeDist[2])
    dataTable <- dataTable %>%
      dplyr::mutate(DateDiff = .data$DateDiff + .data$closeDist*timeDistRatio) %>%
      dplyr::group_by(pick(any_of(c("SampleEventId", "SampleDate", "SiteId", "Latitude", "Longitude")))) %>%
      dplyr::summarise(water.Id=list(.data$watIds), water.SiteId=list(.data$watSiteIds), water.Lat=list(.data$watLat), water.Long=list(.data$watLong), water.Date=list(.data$watDate), water.days=list(.data$DateDiff), water.Dist=list(.data$closeDist), water.index=which.min(.data$DateDiff), .groups="keep") %>%
      dplyr::ungroup()
  } else if (tolower(pairMethod) == "avg water"){
    dataTable <- dataTable %>%
      dplyr::mutate(watDate = as.numeric(lubridate::as_datetime(.data$watDate)), .keep="all") %>%
      rowwise() |>
      mutate(Notes.water = list(json2List(str_remove(.data$Notes.water, "^.{1,2}(?=\\{)")))) |>
      ungroup() |>
      dplyr::group_by(pick(any_of(c("SampleEventId", "SampleDate", "SiteId", "Latitude", "Longitude")))) %>%
      # dplyr::summarise(water.SampleEventId=str_flatten(watIds, ", "), water.SiteId=str_flatten(watSiteIds, ", "), water.Lat=mean(watLat), water.Long=mean(watLong), water.Date=mean(lubridate::as_datetime(watDate)), water.days=mean(DateDiff), water.Dist=mean(closeDist), .groups="keep") %>%
      summarise(notes=jsonlite::toJSON(list(cleanList(list(.data$Notes.water)))),
                across(any_of(c("Id.water", "SpecConductivity.water", "pHValue.water",
                                "DO.water", "DOUnits.water", "DOC.water", "DOCUnits.water",
                                "NH4.water", "NH4Units.water", "NO3.water", "NO3Units.water",
                                "TN.water", "TNUnits.water", "TDN.water", "TDNUnits.water",
                                "SRP.water", "SRPUnits.water",
                                "TP.water", "TPUnits.water", "TDP.water", "TDPUnits.water")), ~list(.x)),
                water.SampleEventId=list(.data$watIds),
                water.SiteId=list(unique(.data$watSiteIds)),
                water.Lat=cleanU(.data$watLat),
                water.Long=cleanU(.data$watLong),
                water.Date=cleanU(lubridate::as_datetime(.data$watDate)),
                water.days=cleanU(.data$DateDiff),
                water.Dist=cleanU(.data$closeDist),
                WaterSampleId.water = cleanJSON(.data$WaterSampleId.water),
                InputFileId.water = list(unique(.data$InputFileId.water)),
                OtherElementsReported.water=any(.data$OtherElementsReported.water),
                StatisticalPooling.water=ifelse(n()==1, .data$StatisticalPooling.water, NA),
                SampleSize.water=ifelse(n()==1, .data$SampleSize.water, NA),
                n=n(), .groups="keep") |>
      rowwise() |>
      mutate(Notes.water=jsonlite::toJSON(discard(c(.data$notes,
                                                    jsonlite::toJSON(list("Id.WaterChemistry"=.data$Id.water)),
                                                    jsonlite::toJSON(list("SpecConductivity"=.data$SpecConductivity.water)),
                                                    jsonlite::toJSON(list("pH"=.data$pHValue.water)),
                                                    condense("DO", unlist(.data$DO.water), unlist(.data$DOUnits.water)),
                                                    condense("DOC", unlist(.data$DOC.water), unlist(.data$DOCUnits.water)),
                                                    condense("NH4", unlist(.data$NH4.water), unlist(.data$NH4Units.water)),
                                                    condense("SRP", unlist(.data$SRP.water), unlist(.data$SRPUnits.water)),
                                                    condense("NO3", unlist(.data$NO3.water), unlist(.data$NO3Units.water)),
                                                    condense("TDN", unlist(.data$TDN.water), unlist(.data$TDNUnits.water)),
                                                    condense("TN", unlist(.data$TN.water), unlist(.data$TNUnits.water)),
                                                    condense("TDP", unlist(.data$TDP.water), unlist(.data$TDPUnits.water)),
                                                    condense("TP", unlist(.data$TP.water), unlist(.data$TPUnits.water)),
                                                    jsonlite::toJSON(list("WaterSampleId"=.data$WaterSampleId.water))),
                                                  is.na))) |>
      mutate(SpecConductivity.water = cleanX(.data$SpecConductivity.water, c("uS_cm")),
             pHValue.water = cleanX(.data$pHValue.water, c("none")),
             DOUnitsPrefered.water = cleanDOUnits(.data$DOUnits.water),
             DO.water = cleanDO(.data$DO.water, .data$DOUnits.water, .data$DOUnitsPrefered.water),
             DOUnits.water = .data$DOUnitsPrefered.water,
             # DO.water = ifelse(any(.data$DOUnits.water=="mg_L", na.rm=TRUE),
                         # mean(.data$DO_mg_L, na.rm=TRUE),
             #             cleanX(.data$DO.water, .data$DOUnits.water)),
             # DOUnits.water = ifelse(any(.data$DOUnits.water=="mg_L", na.rm=NA),
             #                  "mg_L",
             #                  cleanU(.data$DOUnits.water)),
             DOC.water = cleanX(.data$DOC.water, .data$DOCUnits.water),
             DOCUnits.water = cleanU(.data$DOCUnits.water),
             NH4.water = cleanX(.data$NH4.water, .data$NH4Units.water),
             NH4Units.water = cleanU(.data$NH4Units.water),
             SRP.water = cleanX(.data$SRP.water, .data$SRPUnits.water),
             SRPUnits.water = cleanU(.data$SRPUnits.water),
             NO3.water = cleanX(.data$NO3.water, .data$NO3Units.water),
             NO3Units.water = cleanU(.data$NO3Units.water),
             TDN.water = cleanX(.data$TDN.water, .data$TDNUnits.water),
             TDNUnits.water = cleanU(.data$TDNUnits.water),
             TN.water = cleanX(.data$TN.water, .data$TNUnits.water),
             TNUnits.water = cleanU(.data$TNUnits.water),
             TDP.water = cleanX(.data$TDP.water, .data$TDPUnits.water),
             TDPUnits.water = cleanU(.data$TDPUnits.water),
             TP.water = cleanX(.data$TP.water, .data$TPUnits.water),
             TPUnits.water = cleanU(.data$TPUnits.water), .keep="unused") |>
      ungroup() |>
      select(!any_of(c("notes", "DOUnitsPrefered.water")))
  }

  if (tolower(tolower(stringr::str_replace_all(pairMethod, "[0-9\\.]+", "#")) %in% c("min time", "min dist", "#days=#km"))){
    dataTable <- dataTable %>%
      dplyr::rowwise() %>%
      dplyr::mutate(water.SiteId = .data$water.SiteId[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.Lat = .data$water.Lat[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.Long = .data$water.Long[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.Date = .data$water.Date[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.days = .data$water.days[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.Dist = .data$water.Dist[.data$water.index], .keep="all") %>%
      dplyr::mutate(water.Id = .data$water.Id[.data$water.index], .keep="all", .after="Longitude")
  }

  return(dataTable)
}

mergeWater <- function(dataTable, waterTable, pairMethod){
  dataTable <- dataTable %>%
    dplyr::left_join(dplyr::rename_with(waterTable, ~str_c(., ".water"), everything()), by=c("water.SampleEventId"="SampleEventId.water"))

  varList <- list("DO"="DOUnits", "DOC"="DOCUnits", "NH4"="NH4Units",
                  "SRP"="SRPUnits", "NO3"="NO3Units",
                  "TDN"="TDNUnits", "TN"="TNUnits",
                  "TDP"="TDPUnits", "TP"="TPUnits")

  df <- dataTable |>
    mutate(DO_mg_L = if_else(.data$DOUnits=="mg_L", .data$DO, NA)) |>
    group_by(.data$InputFileId) |> #, SampleEventId) |>
    summarise(across(any_of(c("DO", "DOUnits", "DO_mg_L",
                              "DOC", "DOCUnits", "NH4", "NH4Units",
                              "SRP", "SRPUnits", "NO3", "NO3Units",
                              "TDN", "TDNUnits", "TN", "TNUnits",
                              "TDP", "TDPUnits", "TP", "TPUnits")), ~list(c(.x))),
              n=n(), .groups="keep")
  df1 <- filter(df, n>1) |>
    mutate(note = jsonlite::toJSON(discard(c(condense("DO", unlist(.data$DO), unlist(.data$DOUnits)),
                                             condense("DOC", unlist(.data$DOC), unlist(.data$DOCUnits)),
                                             condense("NH4", unlist(.data$NH4), unlist(.data$NH4Units)),
                                             condense("SRP", unlist(.data$SRP), unlist(.data$SRPUnits)),
                                             condense("NO3", unlist(.data$NO3), unlist(.data$NO3Units)),
                                             condense("TDN", unlist(.data$TDN), unlist(.data$TDNUnits)),
                                             condense("TN", unlist(.data$TN), unlist(.data$TNUnits)),
                                             condense("TDP", unlist(.data$TDP), unlist(.data$TDPUnits)),
                                             condense("TP", unlist(.data$TP), unlist(.data$TPUnits))), is.na))) |>
    mutate(DO = ifelse(any(unlist(.data$DOUnits)=="mg_L", na.rm=TRUE),
                       mean(unlist(.data$DO_mg_L), na.rm=TRUE),
                       cleanX(unlist(.data$DO), unlist(.data$DOUnits)))) |>
    mutate(DOUnits = ifelse(any(unlist(.data$DOUnits)=="mg_L", na.rm=NA),
                            "mg_L",
                            cleanU(unlist(.data$DOUnits)))) |>
    mutate(DOC = cleanX(unlist(.data$DOC), unlist(.data$DOCUnits))) |>
    mutate(DOCUnits = cleanU(unlist(.data$DOCUnits))) |>
    mutate(NH4 = cleanX(unlist(.data$NH4), unlist(.data$NH4Units))) |>
    mutate(NH4Units = cleanU(unlist(.data$NH4Units))) |>
    mutate(SRP = cleanX(unlist(.data$SRP), unlist(.data$SRPUnits))) |>
    mutate(SRPUnits = cleanU(unlist(.data$SRPUnits))) |>
    mutate(NO3 = cleanX(unlist(.data$NO3), unlist(.data$NO3Units))) |>
    mutate(NO3Units = cleanU(unlist(.data$NO3Units))) |>
    mutate(TDN = cleanX(unlist(.data$TDN), unlist(.data$TDNUnits))) |>
    mutate(TDNUnits = cleanU(unlist(.data$TDNUnits))) |>
    mutate(TN = cleanX(unlist(.data$TN), unlist(.data$TNUnits))) |>
    mutate(TNUnits = cleanU(unlist(.data$TNUnits))) |>
    mutate(TDP = cleanX(unlist(.data$TDP), unlist(.data$TDPUnits))) |>
    mutate(TDPUnits = cleanU(unlist(.data$TDPUnits))) |>
    mutate(TP = cleanX(unlist(.data$TP), unlist(.data$TPUnits))) |>
    mutate(TPUnits = cleanU(unlist(.data$TPUnits))) |>
    select(!c("DO_mg_L", "n"))
  df2 <- filter(df, n<2) |>
    mutate(across(any_of(c("DO", "DOUnits",
                           "DOC", "DOCUnits", "NH4", "NH4Units",
                           "SRP", "SRPUnits", "NO3", "NO3Units",
                           "TDN", "TDNUnits", "TN", "TNUnits",
                           "TDP", "TDPUnits", "TP", "TPUnits")), unlist)) |>
    mutate(note=jsonlite::toJSON(NA)) |>
    select(!c("DO_mg_L", "n"))
  if (nrow(df1)>0){
    if (nrow(df2)>0){
      df <- bind_rows(df1, df2)
    } else {
      df <- df1
    }
  } else {
    if (nrow(df2)>0){
      df <- df2
    }
  }
  # df |> View()

  return(df)
}

varUnitsList <- function(df, varName){
  varUnits <- str_replace_all(tolower(str_trim(str_split((df[["metadata"]] %>%
                                                          filter(.data$variable==varName))$values, ",")[[1]])), "_", "/")
  return(varUnits[varUnits != "other" & varUnits != "na"])
}

json2List <- function(JSONstr){
  if (is.na(JSONstr)){
    return(list("NA"=NA))
  } else if (str_count(JSONstr, "(\\{|\\})") > 1){
    tempJSON <- JSONstr
    j <- 1
    subLists <- list()

    while (str_detect(tempJSON, "\\{.*\\}") | j > 4){
      subJSON <- as.list(unlist(str_extract_all(tempJSON, "\\{[^\\{\\}]*\\}")))

      for (i in 1:length(subJSON)){
        tempJSON <- str_replace(tempJSON, fixed(subJSON[[i]]), paste0("\"::JSON", 10^(j*2)+i, "::\""))
        subLists[paste0("::JSON", 10^(j*2)+i, "::")] <- list(fromJSON(subJSON[[i]]))
      }
      tempJSON <- str_replace_all(tempJSON, "((?<=::)\"\\s*\"|\"\\s*\"(?=::))", "\"")
      j <- j+1
    }

    reduceList <- function(x, y){return(x[[y]])}

    recurseDiscard <- function(x){
      nameList <- names(x)
      for (iName in nameList){
        if (!is.list(x[[iName]])){
          if (length(x[[iName]])>1){
            x[[iName]] <- discard(x[[iName]], is.na)
          } else if (is.na(x[[iName]])){
            x[[iName]] <- NULL
          }
        } else {
          x[[iName]] <- recurseDiscard(x[[iName]])
        }
      }
      if (is.null(x)){return(NULL)}
      return(x)
    }

    subLists <- recurseDiscard(subLists)

    if (any(str_detect(unname(unlist(subLists)), "::JSON[0-9]{1,7}::"))){
      for (i in str_subset(unname(unlist(subLists)), "::JSON[0-9]{1,7}::")){
        listIndex <- str_split(names(unlist(subLists)[unlist(subLists)==i]), "\\.")[[1]]

        listIndex <- names(unlist(subLists)[unlist(subLists)==i])
        L <- discard(str_extract(listIndex, fixed(names(subLists))), is.na)
        j <- 2
        while (!str_detect(str_c(L, sep=".", collapse="."), str_remove(listIndex, "[0-9]+$")) & j<6){
          L[j] <- names(reduce(L, reduceList, .init=subLists))[which.max(str_length(str_extract(listIndex, fixed(names(reduce(L, reduceList, .init=subLists))))))]

          j <- j+1
        }

        if (length(L)>=1){
        #   subLists[[L]][unlist(subLists[[L[1]]][L[2]]) == i] <- list(subLists[[i]])[[1]]
        #   subLists <- subLists[!(names(subLists)==i)]
        # } else if (length(L)==2){
          subLists[[L]][unlist(subLists[[L]]) == i] <- list(subLists[[i]])
          # subLists[[L[1]]][L[2]] <- list(subLists[[i]])
          subLists <- subLists[!(names(subLists)==i)]
        # } else if (length(L)==3){
        #   subLists[[L[1]]][[L[2]]][[L[3]]][unlist(subLists[[L[1]]][L[2]]) == i] <- list(subLists[[i]])[[1]]
        #   subLists <- subLists[!(names(subLists)==i)]
        # } else if (length(L)==4){
        #   subLists[[L[1]]][[L[2]]][[L[3]]][[L[4]]][unlist(subLists[[L[1]]][L[2]]) == i] <- list(subLists[[i]])[[1]]
        #   subLists <- subLists[!(names(subLists)==i)]
        }
      }
    }
    return(subLists[[1]])
  } else {
    return(list(JSONstr))
  }
}
