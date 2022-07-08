#' @title Load STOICH Data
#'
#' @description
#'   Loads STOICH data from release files.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataPath The path to the STOICH data directory.
#'
#' @return A list containing the STOICH data tables
#'
#' @examples
#' \dontrun{
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#' }
#'
#' @export
loadSTOICH <- function(dataPath=file.path(path.expand("~"), "data")){
  if (!dir.exists(dataPath)){
    stop(paste("Directory not found (please check the path):", dataPath))
  }

  # Search the folder for a metadata csv file to describe the variables
  metaFile <- list.files(dataPath, pattern="\\.csv")
  if (length(metaFile) > 0){
    metaFile <- (metaFile %>% stringr::str_subset("metadata.*\\.csv"))
    if (length(metaFile) > 0){
      metaFile <- metaFile[1]
    } else {stop(paste("No metadata file found (please check the folder):", dataPath))}
  } else {stop(paste("No metadata file found (please check the folder):", dataPath))}

  dataTables <- list()
  # Read the metadata file
  dataTables[["metadata"]] <- readr::read_csv(file.path(dataPath, metaFile), col_types = list(variable="c", table="c", description="c", values="c", dataType="c"))

  tables <- (dataTables[["metadata"]] %>%
               dplyr::select(table) %>%
               dplyr::distinct() %>%
               dplyr::mutate(table = stringr::str_extract(table, "(?<=tbl_)[a-zA-Z]+"))
             )[[1]]
  dataTables[["join_tables"]] <- dataTables[["metadata"]] %>%
    dplyr::mutate(refTable = if_else(str_extract(variable, "[A-Za-z]+(?=Id)") %in% tables,
                                     paste("tbl_", str_extract(variable, "[A-Za-z]+(?=Id)"), sep=""),
                                     as.character(NA))) %>%
    dplyr::select(c("variable", "table", "refTable")) %>%
    tidyr::drop_na(refTable)

  for (i in tables){
    if (!file.exists(file.path(dataPath, paste("tbl_", i, ".csv", sep="")))){
      stop(paste("Expected file: ", paste("tbl_", i, ".csv", sep=""), ", not found in the data directory: ", dataPath, sep=""))
    }
    varType <- (dataTables[["metadata"]] %>%
                    dplyr::filter(table == paste("tbl_", i, sep="")) %>%
                    dplyr::select(c("variable", "dataType")))
    varTypeList <- setNames(as.list(varType$dataType), varType$variable)
    dataTables[[paste("tbl_", i, sep="")]] <- readr::read_csv(file.path(dataPath, paste("tbl_", i, ".csv", sep="")), col_types = varTypeList)
  }

  return(dataTables)
}
