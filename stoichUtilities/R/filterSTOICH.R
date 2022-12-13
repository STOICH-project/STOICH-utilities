#' @title Filter Data in the STOICH Data Tables
#'
#' @description
#'   Filters the STOICH data from release files based on selected criteria.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#' @param tableVar The STOICH table that the variable belongs to. Only used if there are multiple variables with the same name (such as "Notes")
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
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="TrophicMode", val="photoautotroph", condition="equal")
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="Latitude", val=c(54.1, 103.1), condition="range")
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, tableVar="tbl_OrganismStoichiometry", var="Type", val="seston", condition="equal")
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="Name", val=c("Suggs", "Barco"), condition="contains")
#'
#' stoichTable <- joinSTOICH(stoichFiltered)
#'
#' }
#'
#' @export
filterSTOICH <- function(dataTables, tableVar=NA, var, val, condition){
  # Verify the tables are in the proper format for filtering
  verifySTOICH(dataTables)

  if (!is.character(var)){
    stop(paste("The variable entered:", var, "has class:", class(var), " and should be character. Please enter a valid variable name."))
  }

  # Get the metadata for the selected variable
  if (var %in% dataTables[["metadata"]]$variable){
    varMetadata <- dataTables[["metadata"]] %>%
      dplyr::filter(variable == var)
  } else {
    stop(paste("The variable entered:", var, "is not listed in the metadata table. Please enter a valid variable name."))
  }

  # Check if the variable selected is valid
  if (count(varMetadata)[[1]] == 0){
    stop(paste("The variable entered:", var, "is not listed in the metadata table. Please enter a valid variable name."))
  } else if (count(varMetadata)[[1]] > 1){
    # If there are multiple entries for the selected variable, filter based on table.
    if (!is.character(tableVar) | is.na(tableVar)){
      stop(paste("The variable entered: \"", var,
                 "\" is not unique and no value was supplied for tableVar.\nPlease supply the proper table.\n\n",
                 "Tables associated with variable: \"", var, "\" are:\n",
                 str_c(varMetadata$table, collapse=",  "), sep=""))
    } else {
      varMetadata <- varMetadata %>%
        dplyr::filter(table == tableVar)

      if (count(varMetadata)[[1]] == 0){
        stop(paste("The table entered:", tableVar, "is not listed in association with variable:", var, "in the metadata table. Please enter the proper table."))
      }
    }

    if (count(varMetadata)[[1]] > 1){
      stop(paste("An error occured, selecting the table & variable combo, the info entered returned multiple entries. The metadata table may have been altered."))
    }
  }

  # Verify the data type of the entered value: "val" is consistent with the data type for the selected variable.
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

  # Store any changes to the filtered tables in a new variable
  filteredTables <- dataTables

  # Verify the values provided are compatible with the selected filtering type.
  #  condition %in% greater than, less than, equal, not equal, range
  progConds <- c("greater than", "less than", "equal", "not equal", "range", "contains")
  condition <- tolower(condition)
  if (!(condition %in% progConds)){
    stop(paste("Condition entered: ", condition, " was not in the list of programmed filtering conditions.\n\n",
               "Please select from the following list:\n",
               str_c(progConds, collapse=",  "), sep=""))

  } else if (condition == "greater than"){
    if (!is.numeric(val) & !is.integer(val) & !lubridate::is.POSIXct(val)){
      stop(paste("The value entered is not compatible with the condition \"greater than\". \"greater than\" only works with values of type: numeric, integer or date.",
                 "\nPlease adjust the filter value/condition combination."))
    }
    if (length(val) > 1){
      warning("The \"greater than\" condition only uses one value and multiple were supplied. Using the minimum from the array.")
      val <- min(val)
    }
    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(!!rlang::sym(varMetadata$variable) > val) %>%
      dplyr::filter(!is.na(!!rlang::sym(varMetadata$variable))) # This fixes the case where the value is not defined
  } else if (condition == "less than"){
    if (!is.numeric(val) & !is.integer(val) & !lubridate::is.POSIXct(val)){
      stop(paste("The value entered is not compatible with the condition \"less than\". \"less than\" only works with values of type: numeric, integer or date.",
                 "\nPlease adjust the filter value/condition combination."))
    }
    if (length(val) > 1){
      warning("The \"less than\" condition only uses one value and multiple were supplied. Using the minimum from the array.")
      val <- max(val)
    }
    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(!!rlang::sym(varMetadata$variable) < val) %>%
      dplyr::filter(!is.na(!!rlang::sym(varMetadata$variable))) # This fixes the case where the value is not defined
  } else if (condition == "equal"){
    val <- discard(val, is.na)
    if (length(val) == 0){
      stop("NA or no value entered! Please enter a valid term for comparison.")
    }
    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(!!rlang::sym(varMetadata$variable) %in% val)
  } else if (condition == "not equal"){
    val <- discard(val, is.na)
    if (length(val) == 0){
      stop("NA or no value entered! Please enter a valid term for comparison.")
    }
    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(!(!!rlang::sym(varMetadata$variable) %in% val))
  } else if (condition == "range"){
    if (!is.numeric(val) & !is.integer(val) & !lubridate::is.POSIXct(val)){
      stop(paste("The value entered is not compatible with the condition \"range\". \"range\" only works with values of type: numeric, integer or date.",
                 "\nPlease adjust the filter value/condition combination."))
    }

    if (length(val) > 2){
      warning("The \"range\" condition only uses two values but more than two were supplied. Using the maximum and minimum from the array.")
    }
    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(!!rlang::sym(varMetadata$variable) < max(val)) %>%
      dplyr::filter(!!rlang::sym(varMetadata$variable) > min(val)) %>%
      dplyr::filter(!is.na(!!rlang::sym(varMetadata$variable))) # This fixes the case where the value is not defined
  }  else if (condition == "contains"){
    if (!is.character(val)){
      stop(paste("The value entered is not compatible with the condition \"contains\". \"contains\" only works with values of type character.",
                 "\nPlease adjust the filter value/condition combination."))
    }

    val <- discard(val, function(x) {str_length(x) < 0 | is.na(x)})

    if (length(val) == 0){
      stop("The value is empty. Please add search term(s) as strings.")
    }

    filteredTables[[varMetadata$table]] <- filteredTables[[varMetadata$table]] %>%
      dplyr::filter(str_detect(!!rlang::sym(varMetadata$variable), paste("(", str_flatten(val, collapse="|"), ")", sep="")))
  }

  filteredTables <- filterJoinSTOICH(filteredTables, varMetadata$table)

  # Verify the tables aren't empty
  verifySTOICH(filteredTables)

  return(filteredTables)
}
