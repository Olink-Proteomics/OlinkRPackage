## code to prepare internal dataset goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata

# Specifications for top matrix of Olink wide format data sets ----

olink_wide_spec <- dplyr::tibble(
  data_type = c(
    "NPX",
    "Ct",
    "Quantified"
  ),
  has_qc_data = c(
    TRUE,
    FALSE,
    TRUE
  ),
  n_na_rows = c(
    2L,
    1L,
    2L
  ),
  top_matrix_v1 = list(
    c("Panel", "Assay", "Uniprot ID", "OlinkID"),
    c("Panel", "Assay", "Uniprot ID", "OlinkID"),
    c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit")
  ),
  top_matrix_assay_labels = list(
    c("plate" = "Plate ID", "qc_warn" = "QC Warning"),
    c("plate" = "Plate ID"),
    c("plate" = "Plate ID", "qc_warn" = "QC Warning")
  ),
  top_matrix_assay_int_ctrl = list(
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl"),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl"),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl")
  ),
  top_matrix_assay_dev_int_ctrl = list(
    c("QC Deviation from median"),
    character(0L),
    c("QC Deviation from median")
  ),
  top_matrix_uniprot_dev_int_ctrl = list(
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl"),
    character(0L),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl")
  )
)
