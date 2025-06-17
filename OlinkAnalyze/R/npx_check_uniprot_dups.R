assay_identifiers <- function(df) {
  assay_ids <- df |>
    dplyr::select(dplyr::any_of(c("OlinkID", "UniProt"))) |>
    dplyr::distinct()


  # warning if duplicate set of identifiers for 1 assay
  duplicated_oids <- unique(assay_ids$OlinkID[duplicated(assay_ids$OlinkID)])
  uniprot_replace_df <- data.frame(OlinkID = "",
                                   UniProt = "",
                                   new_UniProt = "")


  if (length(duplicated_oids) != 0L) {
    uniprot_dups <- assay_ids |>
      dplyr::filter(OlinkID %in% duplicated_oids) |>
      dplyr::select(dplyr::any_of(c("OlinkID", "UniProt", "Assay")))
    uniprot_original <- uniprot_dups[!duplicated(uniprot_dups$OlinkID), ]
    uniprot_original <- uniprot_original |>
      dplyr::rename("new_UniProt" = "UniProt")
    uniprot_replace_df <- dplyr::left_join(uniprot_dups,
                                           uniprot_original,
                                           by = "OlinkID") |>
      dplyr::filter(.data[["UniProt"]] != .data[["new_UniProt"]])

    uniprot_message <- paste0("UniProt ID `", uniprot_replace_df$UniProt,
                              "` will be replaced with UniProt ID `",
                              uniprot_replace_df$new_UniProt,
                              "` for OlinkID", uniprot_replace_df$OlinkID,
                              "`.\n")
    cli::cli_warn(c("!" = paste0(
                                 "{length(duplicated_oids)} OlinkID{?s}",
                                 " ha{?s/ve} multiple unique UniProt IDs. ",
                                 "The first iteration",
                                 " will be used for downstream analysis."),
    "*" = uniprot_message))
  }

  return(uniprot_replace_df)
}

uniprot_replace <- function(df, npx_check) {
  if (!all(npx_check$uniprot_replace == "")) {
    df <- df |>
      dplyr::left_join(npx_check$uniprot_replace,
                       by = c("UniProt", "OlinkID")) |>
      dplyr::mutate(new_UniProt = ifelse(is.na(.data[["new_UniProt"]]),
                                         .data[["UniProt"]],
                                         .data[["new_UniProt"]])) |>
      dplyr::mutate(UniProt = ifelse(.data[["UniProt"]] !=
                                       .data[["new_UniProt"]],
                                     .data[["new_UniProt"]],
                                     .data[["UniProt"]])) |>
      dplyr::select(-dplyr::any_of("new_UniProt"))
  }
  return(df)
}
