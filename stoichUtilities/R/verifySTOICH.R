#' @title Check STOICH Data Table Format
#'
#' @description
#'   Checks if the STOICH data tables loaded from release files are in the proper format for joining and filtering.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#'
#' @return True if the STOICH data tables are the correct format or stop and return the first error found if an
#'   incorrect format is detected.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' verifySTOICH(stoichData)
#'
#' }
#'
#' @export
verifySTOICH <- function(dataTables){
  # Verify the dataTables variable is a list.
  if (!is.list(dataTables)){
    stop("The dataTables entered were not in the expected format as a list of tables. Please check that dataTables is a list, i.e. class(dataTables) == \"list\".")
  }

  # Verify all the tables are in the list of tables
  tblList <- c("tbl_InputFile", "tbl_Site", "tbl_SampleEvent", "tbl_OrganismStochiometry", "tbl_WaterChemistry", "tbl_Source", "tbl_Contact")
  if (any(!(tblList %in% str_subset(names(dataTables), "tbl_")))){
    stop(paste("One or more expected tables not found in the dataTable list. Please check the list and ensure required tables were loaded and not dropped during filtering/processing."))
  }

  # Verify the data tables aren't empty (otherwise there isn't anything to join)
  for (i in tblList){
    if (!is.data.frame(dataTables[[i]]) | !is_tibble(dataTables[[i]])){
      stop(paste("Expected table in dataTables is not a data.frame or tibble. Please check table: ", i, " i.e. class(dataTable[[", i, "]].", sep=""))
    }
    if (count(dataTables[[i]]) == 0){
      stop(paste("Can't join empty tables, no data in table: ", i, ". Please check input tables and possibly adjust filtering.", sep=""))
    }
    if (!("Id" %in% colnames(dataTables[[i]]))){
      stop(paste("Data table primary key not found for table: ", i, ". Please check input tables and adjust column selections.", sep=""))
    }
  }

  return(TRUE)
}
