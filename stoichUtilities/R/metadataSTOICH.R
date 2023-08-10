#' @title Explore The STOICH Data
#'
#' @description
#'   Used to help explore the STOICH data.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#' @param metadata A character describing what type of metadata is being requested. Possible options are:
#'   "variables", "joined", "full"
#'   "joined" returns the variable names after joinSTOICH is performed (you can use this name before the join to identify a table and variable).
#'   "full" returns a table of all the variables, the table name it belongs to and what data is stored in each variable.
#'
#' @return A vector or table with the requested data.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' # list the variable names
#' metadataSTOICH(stoichData, "variables")
#'
#' }
#'
#' @export
metadataSTOICH <- function(dataTables, metadata=NA){
  # Verify the tables are in the proper format for joining
  verifySTOICH(dataTables)

  if (is.na(metadata) | !is.character(metadata)){
    print("Options for metadata are: \"variables\", \"joined\", \"full\". You can get a list of variable names using metadataSTOICH(dataTables, \"variables\") and supplying loaded STOICH data as \"dataTables\".")
    return("")
  } else if (str_to_lower(metadata) == "variables"){
    return(dataTables[["metadata"]]$variable)
  } else if (str_to_lower(metadata) == "joined"){
    return((dataTables[["metadata"]] |>
              mutate(joined = paste(variable, str_remove_all(table, "tbl_"), sep=".")))$joined)
  } else if (str_to_lower(metadata) == "full"){
    return(dataTables[["metadata"]])
  } else {
    print("Options for metadata are: \"variables\", \"joined\", \"full\". You can get a list of variable names using metadataSTOICH(dataTables, \"variables\") and supplying loaded STOICH data as \"dataTables\".")
    return("")
  }
}
