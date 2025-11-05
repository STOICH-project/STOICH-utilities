#' @title Manage Queries Working With SampleDate Values
#'
#' @description
#'   Joins the STOICH sample year, month & day columns into a SampleDate column and allows filtering based
#'   on sample month/year or dates.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#' @param mode The type of operation you want to perform on the data table.
#'
#' @return The STOICH data table with a new column for the SampleDate created from the SampleYear,
#'   SampleMonth and SampleDay columns.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' stoichTable <- sampleDateSTOICH(stoichDate, mode="create")
#' stoichTable <- sampleDateSTOICH(stoichTable, mode="filter", val="", condition="")
#'
#' }
#'
#' @noRd
sampleDateSTOICH <- function(dataTables, mode="create", val=NA, condition=NA){
  if (mode == "create"){
    dataTables[["tbl_SampleEvent"]] <- createSampleDate(dataTables[["tbl_SampleEvent"]])
  } else if (mode == "filter"){
    val <- as.integer(val)
    dataTables[["tbl_SampleEvent"]] <- dataTables[["tbl_SampleEvent"]] |>
      # Filter the table using the logical comparison vector from "date_compare" to select indexes for the slice function.
      slice((1:nrow(dataTables[["tbl_SampleEvent"]]))[date_compare(dataTables[["tbl_SampleEvent"]], val, condition)])
  }
  return(dataTables)
}


# Compare date to SampleDate
#' @noRd
date_basicCompare <- function(tbl_SampleEvent, testDate, condition){
  condition <- tolower(condition)
  if (is.Date(testDate)){
    if (condition == "greater than"){
      return(if_else(is.na(tbl_SampleEvent$SampleDate),
                     tbl_SampleEvent$SampleYear > year(testDate) | (tbl_SampleEvent$SampleYear == year(testDate) & tbl_SampleEvent$SampleMonth > month(testDate)),
                     tbl_SampleEvent$SampleDate > testDate))
    } else if (condition == "less than"){
      return(if_else(is.na(tbl_SampleEvent$SampleDate),
                     tbl_SampleEvent$SampleYear < year(testDate) | (tbl_SampleEvent$SampleYear == year(testDate) & tbl_SampleEvent$SampleMonth < month(testDate)),
                     tbl_SampleEvent$SampleDate < testDate))
    } else if (condition == "equal"){
      return(if_else(is.na(tbl_SampleEvent$SampleDate),
                     FALSE,
                     tbl_SampleEvent$SampleDate %in% testDate))
    } else if (condition == "not equal"){
      return(if_else(is.na(tbl_SampleEvent$SampleDate),
                     !(tbl_SampleEvent$SampleYear == year(testDate) & tbl_SampleEvent$SampleMonth == month(testDate)),
                     !(tbl_SampleEvent$SampleDate %in% testDate)))
    } else {
      stop(paste("Condition not recognized, expected one of ('greater than', 'less than', 'equal' or 'not equal'). The condition received is: ", condition, ".", sep=""))
    }
  } else {
    stop("Expected testDate to be a YYYYMMDD date string or of type date.")
  }
}


# Convert date strings to date variables
#' @noRd
convertToDate <- function(datesIn){
  datesOut <- as.Date(NA)
  dateCheck <- TRUE
  for (i in 1:length(datesIn)){
    if (is.character(datesIn[i])){
      if (str_length(datesIn[i]) == 8){
        datesOut[i] <- ymd(datesIn[i])
      } else {
        stop(paste("Expected testDate as YYYYMMDD, but received ", datesIn[i], ".", sep=""))
      }
    } else if (is.Date(datesIn[i])){
      datesOut[i] <- datesIn[i]
    }
  }
  if (length(datesOut)!=i){
    stop("At least one date wasn't able to be converted, please ensure the date entered is of type character in the format YYYYMMDD or of type date.")
  }
  return(datesOut)
}


#
#' @noRd
date_compare <- function(tbl_SampleEvent, testDates, condition){
  condition <- tolower(condition)
  testDates <- convertToDate(testDates)
  if (condition %in% c("greater than", "less than", "equal", "not equal", "range")){
    if (condition == "range"){
      return(date_basicCompare(tbl_SampleEvent, min(testDates), "greater than") &
        date_basicCompare(tbl_SampleEvent, max(testDates), "less than"))
    } else {
      for (i in 1:length(testDates)){
        if (i == 1){
          outList <- date_basicCompare(tbl_SampleEvent, testDates, condition)
        } else {
          outList <- date_basicCompare(tbl_SampleEvent, testDates, condition) | outList
        }
      }
      return(outList)
    }
    stop(paste("Date (", testDates, ") or condition (", condition, ") not recognized.", ".", sep=""))
  }
}


#   Create SampleDate Values from Year, Month and Day Values
#' @noRd
check_DateType <- function(tbl_SampleEvent){
  yearType <- FALSE
  if (is.integer(tbl_SampleEvent$SampleYear)){
    yearType <- every(tbl_SampleEvent$SampleYear, ~is.na(.x) | log10(.x) > 3)
  } else if (is.character(tbl_SampleEvent$SampleYear)){
    yearType <- every(tbl_SampleEvent$SampleYear, ~is.na(.x) | str_length(.x) == 4)
  }
  return(yearType &
           typeof(tbl_SampleEvent$SampleMonth) %in% c("character", "integer") &
           typeof(tbl_SampleEvent$SampleDay) %in% c("character", "integer"))
}


# Create SampleDate Values from Year, Month and Day Values
#' @noRd
createSampleDate <- function(tbl_SampleEvent){
  if (check_DateType(tbl_SampleEvent)){
    return(tbl_SampleEvent |>
      mutate(SampleDate = ymd(if_else(is.na(.data$SampleDay) | is.na(.data$SampleMonth) | is.na(.data$SampleYear),
                                      "00010101",
                                      paste(as.character(.data$SampleYear),
                                            str_pad(as.character(.data$SampleMonth), width=2, side="left", pad="0"),
                                            str_pad(as.character(.data$SampleDay), width=2, side="left", pad="0"),
                                            sep="")))) |>
      mutate(SampleDate = if_else(.data$SampleDate == ymd("00010101"), as.Date(NA), .data$SampleDate)))
  } else {
    stop("SampleYear, SampleMonth or SampleDay is expected as an integer or character please investigate and correct.")
  }
}

# Check if the dates are within a specified time duration
#' @noRd
datesInRange <- function(iY, iM, iD, jY, jM, jD, tPeriod, tUnits){
  # convert to a lubridate period
  tPeriod <- period(tPeriod, tUnits)

  printCleanDate <- function(Y, M, D){
    return(ifelse(is.na(D) | is.na(M) | is.na(Y),
                  NA,
                  paste0(Y, "-", str_pad(M, width=2, side="left", pad="0"), "-", str_pad(D, width=2, side="left", pad="0"))))
  }

  # tibble(Year=jY, Month=jM, Day=jD) |>
  #   mutate(strDate=ymd(printCleanDate(Year, Month, Day))) |>
  #   View()

  return(ifelse(is.na(iD) | is.na(jD),
                case_when(tPeriod < months(1) ~ FALSE, # lubridate treats 31 days as 1 month for comparisons
                          (is.na(iM) | is.na(jM)) & tPeriod < years(1)  ~ FALSE,
                          (is.na(iM) | is.na(jM)) ~ years(abs(iY - jY)) < tPeriod,
                          TRUE ~ months(abs(iM + 12*iY - (jM + 12*jY))) < tPeriod),
                abs(ymd(printCleanDate(iY, iM, iD)) - ymd(printCleanDate(jY, jM, jD))) < tPeriod))
}
