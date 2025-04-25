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
  if (verifySTOICH(dataTables)){
    joinedData <- dataTables[["tbl_Contact"]] %>%
      dplyr::rename_with(~str_c(., ".Contact"), everything()) %>%
      dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_Source"]], ~str_c(., ".Source"), everything()),
                        by=c("Id.Contact"="ContactId.Source")) %>%
      dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_SampleEvent"]], ~str_c(., ".SampleEvent"), everything()),
                        by=c("Id.Source"="SourceId.SampleEvent")) %>%
      dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_Site"]], ~str_c(., ".Site"), everything()),
                        by=c("SiteId.SampleEvent"="Id.Site")) %>%
      dplyr::rename(Id.Site = SiteId.SampleEvent) %>%
      dplyr::full_join(dplyr::rename_with(dataTables[["tbl_WaterChemistry"]], ~str_c(., ".WaterChemistry"), everything()) %>%
                         dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_InputFile"]], ~str_c(., ".InputFile.WaterChemistry"), everything()),
                                           by=c("InputFileId.WaterChemistry"="Id.InputFile.WaterChemistry")),
                       by=c("Id.SampleEvent"="SampleEventId.WaterChemistry"),
                       relationship = "one-to-one") %>%
      dplyr::full_join(dplyr::rename_with(dataTables[["tbl_OrganismStoichiometry"]], ~str_c(., ".OrganismStoichiometry"), everything()) %>%
                         dplyr::inner_join(dplyr::rename_with(dataTables[["tbl_InputFile"]], ~str_c(., ".InputFile.OrganismStoichiometry"), everything()),
                                           by=c("InputFileId.OrganismStoichiometry"="Id.InputFile.OrganismStoichiometry")),
                       by=c("Id.SampleEvent"="SampleEventId.OrganismStoichiometry")) # %>%
    # dplyr::rename(Id.InputFile = InputFileId.SampleEvent)

    # if (all((joinedData |> mutate(test=ifelse(is.na(InputFileId.WaterChemistry) | is.na(InputFileId.OrganismStoichiometry),
    #                                           TRUE,
    #                                           InputFileId.OrganismStoichiometry == InputFileId.WaterChemistry)))$test)){
    #   # Only merge if all InputFileIds are the same or NA
    #   joinedData |>
    #     mutate(Id.InputFile=ifelse(is.na(InputFileId.WaterChemistry),
    #                                InputFileId.OrganismStoichiometry,  # if both are NA then the result should still be NA (both shouldn't be NA)
    #                                InputFileId.WaterChemistry)) |>
    #     mutate(EntryName.InputFile=ifelse(is.na(EntryName.InputFile.WaterChemistry),
    #                                       EntryName.InputFile.OrganismStoichiometry,  # if both are NA then the result should still be NA (both shouldn't be NA)
    #                                       EntryName.InputFile.WaterChemistry)) |>
    #     mutate(EntryDate.InputFile=ifelse(is.na(EntryDate.InputFile.WaterChemistry),
    #                                       EntryDate.InputFile.OrganismStoichiometry,  # if both are NA then the result should still be NA (both shouldn't be NA)
    #                                       EntryDate.InputFile.WaterChemistry)) |>
    #     mutate(FilePath.InputFile=ifelse(is.na(FilePath.InputFile.WaterChemistry),
    #                                      FilePath.InputFile.OrganismStoichiometry,  # if both are NA then the result should still be NA (both shouldn't be NA)
    #                                      FilePath.InputFile.WaterChemistry)) |>
    #     mutate(ImportDate.InputFile=ifelse(is.na(ImportDate.InputFile.WaterChemistry),
    #                                        ImportDate.InputFile.OrganismStoichiometry,  # if both are NA then the result should still be NA (both shouldn't be NA)
    #                                        ImportDate.InputFile.WaterChemistry)) |>
    #     select(!c(InputFileId.WaterChemistry, InputFileId.OrganismStoichiometry,
    #               EntryName.InputFile.WaterChemistry, EntryName.InputFile.OrganismStoichiometry,
    #               EntryDate.InputFile.WaterChemistry, EntryDate.InputFile.OrganismStoichiometry,
    #               FilePath.InputFile.WaterChemistry, FilePath.InputFile.OrganismStoichiometry,
    #               ImportDate.InputFile.WaterChemistry, ImportDate.InputFile.OrganismStoichiometry))
    # }

    return(joinedData)
  } else {
     return(tibble())
  }
}
