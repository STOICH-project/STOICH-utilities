#' @title Filter Data in the STOICH Data Tables
#'
#' @description
#'   Filters the STOICH data from release files based on selected criteria.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#' @param var The STOICH variable to filter. Use View(stoichData[["metadata"]]) to see a table of all variables and their descriptions
#'   (assumes data was loaded into the variable "stoichData").
#' @param val The value or value range for the variable you want to filter.
#'   The conditions "greater than" and "less than" only work with a single numeric or date value, while "equal" and "not equal" work with
#'   a single value or an array of values (text, numeric or date). The "range" condition requires an array of 2 numeric or date values.
#' @param condition The criteria used for filtering (greater than, less than, equal, not equal, range, etc.). See the val parameter for
#'   the possible inputs for each condition.
#'
#' @return The STOICH data tables after filtering has been applied.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' # filtering by table such as:
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="TrophicMode", val="photoautotroph", condition="not equal")
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="Latitude", val=c(104.92, 103.01), condition="range")
#'
#' stoichTable <- joinSTOICH(stoichFiltered)
#'
#' }
#'
#' @export
filterSTOICH <- function(dataTables, var, val, condition){
  # Verify the tables are in the proper format for filtering
  verifySTOICH(dataTables)

  if (!is.character(var)){
    stop(paste("The variable entered:", var, "has class:", class(var), " and should be character. Please enter a valid variable name."))
  }

  # Get the name of the table the variable belongs to
  varMetadata <- dataTables[["metadata"]] %>%
    dplyr::filter(variable = var)

  if (varMetadata$dataType == "numeric" & !is.numeric(val)){
    stop(paste("Variable type is numeric, but:", class(val), "was provided. Please provide a valid value for \"val\"."))
  } else if (varMetadata$dataType == "integer" & !is.integer(val)){
    stop(paste("Variable type is integer, but:", class(val), "was provided. Please provide a valid value for \"val\"."))
  } else if (varMetadata$dataType == "character" & !is.character(val)){
    stop(paste("Variable type is character, but:", class(val), "was provided. Please provide a valid value for \"val\"."))
  } else if (varMetadata$dataType == "logical" & !is.logical(val)){
    stop(paste("Variable type is logical, but:", class(val), "was provided. Please provide a valid value for \"val\"."))
  } else if (varMetadata$dataType == "datetime" & !lubridate::is.POSIXct(val)){
    stop(paste("Variable type is datetime, but:", class(val), "was provided. Please provide a valid value for \"val\"."))
  }

  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  # finish checking the conditions and values entered (single value or array)


  filteredTables <-
}
