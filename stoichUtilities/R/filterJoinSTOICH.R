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

  # Order should only matter for the Contact and InputFile tables, everything else is filtered through the SampleEvent table.
  if (filtTable == "tbl_Contact"){
    dataTables[["tbl_Source"]] <- dataTables[["tbl_Source"]] %>%
      dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$SourceId)) %>%
      dplyr::filter(ContactId %in% unique(dataTables[["tbl_Contact"]]$Id))
  } else if (filtTable == "tbl_InputFile"){
    dataTables[["tbl_WaterChemistry"]] <- dataTables[["tbl_WaterChemistry"]] %>%
      dplyr::filter(InputFileId %in% unique(dataTables[["tbl_InputFile"]]$Id))

    dataTables[["tbl_OrganismStoichiometry"]] <- dataTables[["tbl_OrganismStoichiometry"]] %>%
      dplyr::filter(InputFileId %in% unique(dataTables[["tbl_InputFile"]]$Id))
  }

  # If the OrganismStoichiometry or WaterChemistry tables were filtered then filter the other to ensure the final join
  # doesn't pull a bunch of empty rows into the filtered table.
  if (filtTable == "tbl_OrganismStoichiometry"){
    sampleEventIds <- unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId)
  } else if (filtTable == "tbl_WaterChemistry") {
    sampleEventIds <- unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId)
  } else {
    sampleEventIds <- unique(c(unique(dataTables[["tbl_WaterChemistry"]]$SampleEventId),
                               unique(dataTables[["tbl_OrganismStoichiometry"]]$SampleEventId)))
  }

  dataTables[["tbl_SampleEvent"]] <- dataTables[["tbl_SampleEvent"]] %>%
    dplyr::filter(SourceId %in% unique(dataTables[["tbl_Source"]]$Id)) %>%
    dplyr::filter(SiteId %in% unique(dataTables[["tbl_Site"]]$Id)) %>%
    dplyr::filter(Id %in% sampleEventIds)

  dataTables[["tbl_Contact"]] <- dataTables[["tbl_Contact"]] %>%
    dplyr::filter(Id %in% unique(dataTables[["tbl_Source"]]$ContactId))

  dataTables[["tbl_Site"]] <- dataTables[["tbl_Site"]] %>%
    dplyr::filter(Id %in% unique(dataTables[["tbl_SampleEvent"]]$SiteId))

  dataTables[["tbl_InputFile"]] <- dataTables[["tbl_InputFile"]] %>%
    dplyr::filter(Id %in% unique(c(dataTables[["tbl_WaterChemistry"]]$InputFileId,
                                   dataTables[["tbl_OrganismStoichiometry"]]$InputFileId)))

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
