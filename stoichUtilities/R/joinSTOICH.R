#' @title Join STOICH Data Tables
#'
#' @description
#'   Joins the STOICH data tables from release files into a single table.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables loaded using loadSTOICH.
#'
#' @return A single table merged from the individual STOICH data tables.
#'
#' @examples
#' \dontrun{
#' # Load the data
#' stoichData <- loadSTOICH(dataPath="C:/Users/example_user/Documents/data")
#'
#' # filtering by table such as:
#' stoichFiltered <- filterSTOICH(dataTables=stoichData, var="TrophicMode", val="photoautotroph", condition="not equal") |>
#'   filterSTOICH(dataTables=stoichFiltered, var="Latitude", val=c(104.92, 103.01), condition="range")
#'
#' stoichTable <- joinSTOICH(stoichFiltered)
#'
#' }
#'
#' @export
joinSTOICH <- function(dataTables){
  # Verify the tables are in the proper format for joining
  verifySTOICH(dataTables)

  joinedData <- dataTables[["tbl_Contact"]] %>%
    dplyr::rename_with(~str_c(., ".Contact"), everything()) %>%
    dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_Source"]], ~str_c(., ".Source"), everything()),
                      by=c("Id.Contact"="ContactId.Source")) %>%
    dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_SampleEvent"]], ~str_c(., ".SampleEvent"), everything()),
                      by=c("Id.Source"="SourceId.SampleEvent")) %>%
    dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_Site"]], ~str_c(., ".Site"), everything()),
                      by=c("SiteId.SampleEvent"="Id.Site")) %>%
    dplyr::rename(Id.Site = SiteId.SampleEvent) %>%
    dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_InputFile"]], ~str_c(., ".InputFile"), everything()),
                      by=c("InputFileId.SampleEvent"="Id.InputFile")) %>%
    dplyr::rename(Id.InputFile = InputFileId.SampleEvent) %>%
    dplyr::full_join(dplyr::rename_with(dataTables[["tbl_WaterChemistry"]], ~str_c(., ".WaterChemistry"), everything()),
                      by=c("Id.SampleEvent"="SampleEventId.WaterChemistry")) %>%
    dplyr::full_join(dplyr::rename_with(dataTables[["tbl_OrganismStoichiometry"]], ~str_c(., ".OrganismStoichiometry"), everything()),
                      by=c("Id.SampleEvent"="SampleEventId.OrganismStoichiometry"))

  return(joinedData)
}
