#' @title Filter Based on Simulated Joins of STOICH Data Tables
#'
#' @description
#'   Filters STOICH data tables following a filtering operation against rows of other tables to remove
#'   any corresponding entries from other tables.
#'
#' @author Chad Petersen \email{cpetersen4@unl.edu}
#'
#' @param dataTables The STOICH data tables filtered using filterSTOICH.
#' @param filtTable The table name filtering was performed on.
#'
#' @return Tables with corresponding entries that were removed from one table removed from the other tables.
#'
#' @examples
#' \dontrun{
#' # Not typically run independently. This function is called by filterSTOICH to clean up before returning data.
#' }
#'
#' @export
filterJoinSTOICH <- function(dataTables, filtTable){
  # Verify the tables are in the proper format for joining
  verifySTOICH(dataTables)

  # Order should only matter for the contact table, everything else is filtered through the SampleEvent table.
  if (filtTable == "tbl_Contact"){
    dataTables[["tbl_Source"]] <- dataTables[["tbl_Source"]] %>%
      dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$SourceId)) %>%
      dplyr::filter(ContactId %in% unique(dataTables[["tbl_Contact"]]$Id))
  }

  dataTables[["tbl_SampleEvent"]] <- dataTables[["tbl_SampleEvent"]] %>%
    dplyr::filter(InputFileId %in% unique(dataTables[["tbl_InputFile"]]$Id)) %>%
    dplyr::filter(SourceId %in% unique(dataTables[["tbl_Source"]]$Id)) %>%
    dplyr::filter(SiteId %in% unique(dataTables[["tbl_Site"]]$Id)) %>%
    dplyr::filter(Id %in% unique(c(unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId),
                                   unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId))))

  dataTables[["tbl_Contact"]] <- dataTables[["tbl_Contact"]] %>%
    dplyr::filter(Id %in% unique(dataTables[["tbl_Source"]]$ContactId))

  dataTables[["tbl_Site"]] <- dataTables[["tbl_Site"]] %>%
    dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$SiteId))

  dataTables[["tbl_InputFile"]] <- dataTables[["tbl_InputFile"]] %>%
    dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$InputFileId))

  dataTables[["tbl_WaterChemistry"]] <- dataTables[["tbl_WaterChemistry"]] %>%
    dplyr::filter(SampleEventId %in% unique(dataTables[["tbl_SampleEvent"]]$Id))

  dataTables[["tbl_OrganismStoichiometry"]] <- dataTables[["tbl_OrganismStoichiometry"]] %>%
    dplyr::filter(SampleEventId %in% unique(dataTables[["tbl_SampleEvent"]]$Id))

  # If the contact table wasn't the table being filtered filter it last.
  if (filtTable != "tbl_Contact"){
    dataTables[["tbl_Source"]] <- dataTables[["tbl_Source"]] %>%
      dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$SourceId)) %>%
      dplyr::filter(ContactId %in% unique(dataTables[["tbl_Contact"]]$Id))
  }

  return(dataTables)
}
