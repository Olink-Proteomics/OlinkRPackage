#' Function to read NPX data into long format
#'
#' Imports an NPX or QUANT file exported from Olink Software.
#' No alterations to the output format is allowed.
#'
#' @param filename Path to Olink Software output file.
#' @return A "tibble" in long format. Columns include:
#' \itemize{
#'    \item{SampleID:} Sample ID
#'    \item{Index:} Index
#'    \item{OlinkID:} Olink ID
#'    \item{UniProt:} UniProt ID
#'    \item{Assay:} Protein symbol
#'    \item{MissingFreq:} Proportion of sample below LOD
#'    \item{Panel_Version:} Panel Version
#'    \item{PlateID:} Plate ID
#'    \item{QC_Warning:} QC Warning Status
#'    \item{LOD:} Limit of detection
#'    \item{NPX:} Normalized Protein Expression
#' }
#' Additional columns may be present or missing depending on the platform
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#' @importFrom magrittr %>%
#' @importFrom tools file_ext md5sum file_path_sans_ext
#' @importFrom dplyr as_tibble distinct pull filter bind_cols mutate left_join select rename matches bind_rows arrange slice_head
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect str_replace_all str_to_upper str_to_title str_replace
#' @importFrom tidyr tibble separate gather
#' @importFrom utils tail
#' @importFrom zip unzip
#' @importFrom stats na.omit
#' @importFrom utils tail

read_NPX <- function(filename) {
  # If the file is csv or txt read_NPX assumes Explore NPX data in long format
  if (tools::file_ext(filename) %in% c("csv", "txt", "zip")) {
    read_NPX_explore(filename = filename)
  } else if (tools::file_ext(filename) %in% c("xls", "xlsx")) { # otherwise we assume T48 or T96
    read_NPX_target(filename = filename)
  } else {
    stop("Unrecognized input file extension!")
  }
}

read_NPX_explore <- function(filename) {

  # flag checking if we need to cleanup after extracting compressed data
  file_is_compressed <- FALSE

  # if input file is compressed
  if (tools::file_ext(filename) == "zip") {

    # **** Prep ****

    # check flag set to true
    file_is_compressed <- TRUE

    # check contents of the compressed file
    # keep all files but the README.txt
    compressed_file_contents <- utils::unzip(zipfile = filename, list = TRUE) %>%
      dplyr::filter(Name != "README.txt") %>%
      dplyr::pull(Name)

    # Check if checksum file (MD5 or SHA256) exists and prepare list of files to unzip
    if ("MD5_checksum.txt" %in% compressed_file_contents) {

      compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "MD5_checksum.txt"] # md5 txt file
      compressed_file_csv   <- compressed_file_contents[compressed_file_contents != "MD5_checksum.txt"] # NPX csv file
      files_to_extract      <- c(compressed_file_csv, compressed_file_chksm) # array of files to extract

    } else if("checksum_sha256.txt" %in% compressed_file_contents) {

      compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "checksum_sha256.txt"] # sha256 txt file
      compressed_file_csv   <- compressed_file_contents[compressed_file_contents != "checksum_sha256.txt"] # NPX csv file
      files_to_extract      <- c(compressed_file_csv, compressed_file_chksm) # array of files to extract

    } else {

      compressed_file_chksm <- NA_character_
      compressed_file_csv   <- compressed_file_contents
      files_to_extract      <- compressed_file_csv

    }

    # Make sure that there is only one NPX file
    # Make sure that the files to extract have *.csv and *.txt extnsions
    if (length(compressed_file_csv) != 1 ||
        !(tools::file_ext(compressed_file_csv) %in% c("csv", "txt"))) {
      stop("The compressed file does not contain a valid NPX file. Expecting: \"README.txt\", \"MD5_checksum.txt\" or \"checksum_sha256.txt\" and the NPX file.")
    }

    # **** Extract ****

    # temporary directory to extract
    tmp_unzip_dir <- tempfile()

    zip::unzip(zipfile = filename, files = files_to_extract, exdir = tmp_unzip_dir, overwrite = TRUE)

    # Extracted NPX csv file
    extracted_file_csv <- file.path(tmp_unzip_dir, compressed_file_csv)

    # **** Checksum ****

    # Checksum of the NPX file
    if (!is.na(compressed_file_chksm)) {

      # Extracted checksum file
      extracted_file_chksm <- file.path(tmp_unzip_dir, compressed_file_chksm) # MD5 relative path

      # make the checksum filename easier to parse
      chksm_string <- tolower(tools::file_path_sans_ext(compressed_file_chksm))

      # check that checksum matches NPX csv file
      extracted_file_chksm_con <- file(extracted_file_chksm, "r")

      if (stringr::str_detect(chksm_string, "md5")) {
        chksm <- tools::md5sum(extracted_file_csv) %>%
          unname()
      } else if(stringr::str_detect(chksm_string, "sha256")) {
        chksm <- openssl::sha256(file(extracted_file_csv)) %>%
          stringr::str_replace(string = ., pattern = ":", replacement = "")
      }

      if (readLines(con = extracted_file_chksm_con, n = 1, warn = FALSE) != chksm) { # check if checksum matches
        # clean up files
        close(extracted_file_chksm_con)
        invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))
        stop(paste("Checksum of NPX file does not match the one from \"", compressed_file_chksm, "\"! Loss of data?", sep = ""))
      }
      close(extracted_file_chksm_con)
    }

    filename <- extracted_file_csv
  }

  # **** Read ****

  out <- read.table(file = filename,
                    header = TRUE,
                    sep = ";",
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", ""),
                    comment.char = "")

  # if only one column in the data, try "," as delimiter
  if (is.data.frame(out) && ncol(out) == 1) {
    out <- read.table(file = filename,
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE,
                      na.strings = c("NA", ""))
  }

  # if this fails too, then throw an error
  if (is.data.frame(out) && ncol(out) == 1) {
    stop("Could not read NPX csv file! Delimiter is not \";\" or \",\". ")
  }

  # cleanup temporary directory with extracted files
  if (file_is_compressed == TRUE) {
    invisible(unlink(x = tmp_unzip_dir, recursive = TRUE)) # remove files
  }

  # Check that all column names are present
  # We have had 5 column names versions so far: 1, 1.1 and 2, 2.1, and 3
  # please add newer versions to the list chronologically
  header_standard <- c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "PlateID", "QC_Warning",
                       "LOD", "NPX")
  header_ext_standard <- c(header_standard, "Sample_Type", "Panel_Lot_Nr",
                           "WellID", "Normalization", "Assay_Warning",
                           "IntraCV", "InterCV", "Processing_StartDate",
                           "Processing_EndDate", "AnalyzerID")
  header_v <- list("header_npx_v1"   = c(header_standard,
                                         "Panel_Version"),
                   "header_npx_v1.1" = c(header_standard,
                                         "Panel_Version", "Normalization",
                                         "Assay_Warning"),
                   "header_npx_v2"   = c(header_standard,
                                         "Panel_Lot_Nr", "Normalization"),
                   "header_npx_v2.1" = c(header_standard,
                                         "Panel_Lot_Nr", "Normalization",
                                         "Assay_Warning"),
                   "header_npx_v3"   = c(header_standard,
                                         "Sample_Type", "Panel_Lot_Nr",
                                         "Assay_Warning", "Normalization",
                                         "ExploreVersion"),

                   "header_ext_v1"   = c(header_ext_standard,
                                         "INC_Warning",
                                         "AMP_Warning",
                                         "Count_Warning"),
                   "header_ext_v2"   = c(header_ext_standard,
                                         "ExploreVersion"))

  header_match <-  header_v |>
    sapply(function(x)
      identical(sort(x),
                { colnames(out) |> sort() })
      ) |>
    any() # look for one full match

  if (header_match == FALSE) {

    # if there is a mismatch of the input data with the expected column names
    # - we pick the set of column names with the shortest distance to any of the expected ones
    # - if multiple matches occur, we pick the most recent
    header_diff_1 <- lapply(header_v, function(x) setdiff(x, colnames(out))) %>%
      lapply(length)
    header_diff_2 <- lapply(header_v, function(x) setdiff(colnames(out), x)) %>%
      lapply(length)
    header_pick  <- tidyr::tibble(v_name = names(header_v),
                                  v1     = unlist(header_diff_1),
                                  v2     = unlist(header_diff_2)) |>
      dplyr::mutate(v = v1 + v2) |>
      dplyr::arrange(v, v_name) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(v_name)

    # find missing columns
    missing_cols <- setdiff(header_v[[header_pick]], colnames(out))
    
    
    if (length(missing_cols)  != 0) {
      #If missing columns, throw a warning and print out which ones we guessed
      # that are missing
      warning(paste0("Cannot find columns: ", paste(missing_cols, collapse = ",")))
      
    }

  }

  out <- out %>%
    dplyr::mutate(NPX         = as.numeric(NPX),
                  LOD         = as.numeric(LOD),
                  MissingFreq = as.numeric(MissingFreq),
                  SampleID    = as.character(SampleID)) %>%
    dplyr::as_tibble()

  return(out)

}

read_NPX_target <- function(filename) {

  NORM_FLAG <-  FALSE
  long_form <- FALSE

  # Check if the data is npx or concentration as well as if it is T48 or T96

  data_type <- readxl::read_excel(path = filename,
                                  range = 'A2',
                                  col_names = FALSE,
                                  .name_repair = "minimal")
  data_type_long <- readxl::read_excel(path = filename,
                                  range = 'G2',
                                  col_names = FALSE,
                                  .name_repair = "minimal")

  if (grepl(pattern = 'NPX', x = data_type, fixed = TRUE)) {

    is_npx_data     <- TRUE
    isTarget <- FALSE
    n_max_meta_data <- 4

    # Check whether it is target 48 or 96
    panel_name <- readxl::read_excel(path = filename,
                                     range = 'B3',
                                     col_names = FALSE,
                                     .name_repair="minimal")

    if (grepl(pattern = 'Target 48', x = panel_name, fixed = TRUE)) {
      target_type <- '48'
      BASE_INDEX  <- 45
    } else {
      target_type <- '96'
      BASE_INDEX  <- 92
    }
  } else if (grepl(pattern = 'Quantified', x = data_type, fixed = TRUE)) {
    # Quant data given, which also means it is target 48
    is_npx_data     <- FALSE
    n_max_meta_data <- 5
    target_type     <- '48'
    BASE_INDEX      <- 45
  } else if (grepl(pattern = "Target", data_type_long, fixed = TRUE)){
    message("Target data in long form detected.")
    long_form <- TRUE
    isTarget <- TRUE
  } else if (
      grepl(pattern = "Flex", data_type_long, fixed = TRUE) |
      grepl(pattern = "F[A-Z]{3}-[A-Z]{4}",
            data_type_long, fixed = FALSE)
      ) {
    message("Flex data in long form detected.")
    long_form <- TRUE
    isTarget <- FALSE
  } else {
    stop("Cannot find whether the given data is NPX or concentration")
  }

  if (long_form == TRUE && isTarget == TRUE){
    out <- readxl::read_excel(path = filename,
                       col_names = T) %>%
      dplyr::filter(!is.na(SampleID)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Panel_Version = gsub(".*\\(","",Panel)) %>%
      dplyr::mutate(Panel_Version = gsub("\\)","",Panel_Version)) %>%
      dplyr::mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>%
      dplyr::mutate(Panel = stringr::str_to_title(Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 96", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 48", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Olink", "", Panel)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "left")) %>%
      tidyr::separate(Panel, " ", into = c("Panel_Start", "Panel_End"), fill = "right") %>%
      dplyr::mutate(Panel_End = ifelse(grepl("Ii", Panel_End), stringr::str_to_upper(Panel_End), Panel_End)) %>%
      dplyr::mutate(Panel_End = ifelse(is.na(Panel_End), " ", Panel_End)) %>%
      dplyr::mutate(Panel = paste("Olink", Panel_Start, Panel_End)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "right")) %>%
      dplyr::select(-Panel_Start, -Panel_End) %>%
      dplyr::select(SampleID, Index, OlinkID,
                    UniProt, Assay, MissingFreq,
                    Panel,Panel_Version,PlateID,
                    QC_Warning,dplyr::matches("Plate_LQL"),
                    dplyr::matches("LOD"),
                    dplyr::matches("Plat_LOD"),
                    dplyr::matches("LLOQ"),
                    dplyr::matches("ULOQ"),
                    dplyr::matches("NPX"),
                    dplyr::matches("Quantified_value"),
                    dplyr::matches("Unit"),
                    dplyr::matches("Assay_Warning"),
                    dplyr::matches("Normalization"),
                    dplyr::matches("*Inc Ctrl*"),
                    dplyr::matches("*Det Ctrl*"))
    is_npx_data <- ifelse(any(names(out) %in% "NPX"), TRUE, FALSE)
  } else  if (long_form == TRUE && isTarget == FALSE){
    out <- readxl::read_excel(path = filename,
                              col_names = T) %>%
      dplyr::rename(dplyr::any_of(c("SampleID" = "SampleId"))) %>%
      dplyr::filter(!is.na(SampleID)) %>%
      dplyr::as_tibble() %>%
      dplyr::select(SampleID, Index, OlinkID,
                    UniProt, Assay, MissingFreq,
                    Panel,Panel_Version,PlateID,
                    QC_Warning,dplyr::matches("Plate_LQL"),
                    dplyr::matches("LOD"),
                    dplyr::matches("Plat_LOD"),
                    dplyr::matches("LLOQ"),
                    dplyr::matches("ULOQ"),
                    dplyr::matches("NPX"),
                    dplyr::matches("Quantified_value"),
                    dplyr::matches("Unit"),
                    dplyr::matches("Assay_Warning"),
                    dplyr::matches("Normalization"),
                    dplyr::matches("*Inc Ctrl*"),
                    dplyr::matches("*Det Ctrl*"))
    is_npx_data <- ifelse(any(names(out) %in% "NPX"), TRUE, FALSE)
  } else if (long_form == FALSE & isTarget == FALSE) {
    message("Flex data detected.")
    out <- read_npflex(filename)
    
  }else {
    # Load initial meta data (the first rows of the wide file)
    meta_dat <-  readxl::read_excel(path = filename,
                                    skip = 2,
                                    n_max = n_max_meta_data,
                                    col_names = FALSE,
                                    .name_repair="minimal")
    meta_dat[4,1] <- 'SampleID'

    NR_DEVIATIONS <- sum(stringr::str_detect(meta_dat[2,], 'QC Deviation from median'))
    control_index <- (stringr::str_detect(meta_dat[2,], 'Det Ctrl') |
                        stringr::str_detect(meta_dat[2,], 'Inc Ctrl 2') |
                        stringr::str_detect(meta_dat[2,], 'Inc Ctrl 1') |
                        stringr::str_detect(meta_dat[2,], 'Ext Ctrl'))

    meta_dat[4, control_index] <- meta_dat[2, control_index]
    meta_dat[3, control_index] <- '-'

    NR_CONTROLS <- sum(control_index)

    nr_panel <- (ncol(meta_dat) - 1 - NR_DEVIATIONS - NR_CONTROLS)/(BASE_INDEX + 2)

    nr_col          <- ncol(meta_dat)
    names(meta_dat) <- as.character(1:nr_col)

    meta_dat <- meta_dat %>%
      dplyr::rename(Name = `1`)

    # Load NPX or QUANT data including the last rows of meta data
    dat <- readxl::read_excel(path = filename,
                              skip = n_max_meta_data + 2,
                              col_names = FALSE,
                              .name_repair="minimal",
                              col_types = c('text'))

    nr_col     <- ncol(dat)
    names(dat) <- as.character(1:nr_col)

    dat <- dat %>%
      dplyr::rename(Name = `1`)

    # Calculate number of plates
    plates <- dat[,nr_col - nr_panel] %>%
      dplyr::distinct() %>%
      stats::na.omit() %>%
      dplyr::pull()
    nr_plates <- length(plates)

    # Extract the meta data from the last rows of data
    missfreq <- dat %>% dplyr::filter(stringr::str_detect(Name, "Missing Data freq."))
    norm_method <- dat %>% dplyr::filter(stringr::str_detect(Name, "Normalization"))
    if (!is_npx_data) {
      assay_warning <- dat %>% dplyr::filter(stringr::str_detect(Name, "Assay warning"))
      Plate_LQL <- dat %>% dplyr::filter(stringr::str_detect(Name, "Lowest quantifiable level"))
      LOD <- dat %>% dplyr::filter(stringr::str_detect(Name, "Plate LOD"))
      LLOQ <- dat %>% dplyr::filter(stringr::str_detect(Name, "LLOQ"))
      ULOQ <- dat %>% dplyr::filter(stringr::str_detect(Name, "ULOQ"))
    } else {
      LOD <- dat %>% dplyr::filter(stringr::str_detect(Name, "LOD"))
    }

    # Add the new meta data to ´meta_dat´
    meta_dat <- rbind(meta_dat,missfreq)
    if (!is_npx_data) {
      meta_dat <- rbind(meta_dat, LLOQ, ULOQ, assay_warning, Plate_LQL)
    }
    meta_dat <- rbind(meta_dat, LOD, norm_method)

    # Remove the meta data from dat
    if (is_npx_data) {
      nbr_meta_data_rows_bottom <- 3
    } else {
      nbr_meta_data_rows_bottom <- 4 + 3 * nr_plates
    }

    if (nrow(norm_method) == 0) {
      nbr_meta_data_rows_bottom <- nbr_meta_data_rows_bottom - 1
    } else {
      NORM_FLAG <- TRUE
    }

    dat <- dat[c(-1 * (nrow(dat) - nbr_meta_data_rows_bottom):nrow(dat)),]

    # Create index vector
    SampleID <- dat$Name
    Index_nr <- c(1:length(SampleID))

    # Initiate lists for later use
    panel_data <- list() ##NPX values to be stored
    QC_list <- list()    ##QC data
    meta_data_list <- list() ## meta data
    panel_list <- list()  ## combination of panel data and QC
    assay_name_list <- list()
    panel_list_long <- list()
    deviations_list <- list()

    if (NR_CONTROLS > 0) {
      BASE_INDEX <- BASE_INDEX + NR_CONTROLS / nr_panel
    }

    # Construct a list of tibbles that match the long format
    for (i in 1:nr_panel) {

      panel_data[[i]] <- dat[,(2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX))]

      if (NR_DEVIATIONS == 0) {
        QC_list[[i]] <- dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                               (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]

        meta_data_list[[i]] <- meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                           (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]


      } else {

        QC_list[[i]] <- dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                               (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel,
                               (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1),
                               (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1)+1)]

        meta_data_list[[i]] <- meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                           (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel,
                                           (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel,
                                           (2+((nr_panel)*BASE_INDEX)+(i-1))+3*nr_panel)]

        meta_data_list[[i]][4,(BASE_INDEX+3)] <- "QC Deviation Inc Ctrl"
        meta_data_list[[i]][4,(BASE_INDEX+4)] <- "QC Deviation Det Ctrl"


      }

      meta_data_list[[i]][4,(BASE_INDEX+1)] <- meta_data_list[[i]][2,(BASE_INDEX+1)]
      meta_data_list[[i]][4,(BASE_INDEX+2)] <- meta_data_list[[i]][2,(BASE_INDEX+2)]


      panel_list[[i]] <- cbind(panel_data[[i]],QC_list[[i]])

      colnames(panel_list[[i]]) <- unlist(meta_data_list[[i]][4,])

      panel_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))] <-
        lapply(panel_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))],
               function(x) as.numeric(stringr::str_replace_all(x,
                                                               c('#' = '', ',' = '.',
                                                                 'No Data' = NA, '> ULOQ' = NA,
                                                                 '< LLOQ' = NA))))

      # Remove the last two columns since they contain redundant meta data and
      # will only cause warnings
      meta_data_list[[i]] <- meta_data_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))]

      assay_name_list[[i]] <- tidyr::tibble(ID=c(t(meta_data_list[[i]][4,])),
                                            Name=c(t(meta_data_list[[i]][2,])),
                                            UniProt = c(t(meta_data_list[[i]][3,])),
                                            Panel=c(t(meta_data_list[[i]][1,])))

      if (is_npx_data) {
        assay_name_list[[i]] <- dplyr::bind_cols(assay_name_list[[i]],
                                                 MissingFreq=c(t(meta_data_list[[i]][5,])),
                                                 LOD = as.numeric(c(t(meta_data_list[[i]][6,]))))

        if (NORM_FLAG == TRUE) {
          assay_name_list[[i]] <- dplyr::bind_cols(assay_name_list[[i]],
                                                   Normalization = c(t(meta_data_list[[i]][7,])))
        }

        panel_list_long[[i]] <- panel_list[[i]] %>%
          dplyr::mutate(SampleID = SampleID) %>%
          dplyr::mutate(Index = Index_nr) %>%
          tidyr::gather(Assay, NPX, -SampleID,-`QC Warning`,
                        -`Plate ID`,-Index,
                        -dplyr::matches("QC Deviation Inc Ctrl"),
                        -dplyr::matches("QC Deviation Det Ctrl")) %>%
          dplyr::left_join(assay_name_list[[i]], by = c('Assay' = 'ID')) %>%
          dplyr::select(SampleID, Index, Assay, UniProt, Name, MissingFreq, Panel,
                        `Plate ID`,`QC Warning`, LOD, NPX, dplyr::matches("Assay_Warning"),
                        dplyr::matches("Normalization"), dplyr::matches("QC Deviation Inc Ctrl"),
                        dplyr::matches("QC Deviation Det Ctrl")) %>%
          dplyr::rename(PlateID = `Plate ID`) %>%
          dplyr::rename(QC_Warning = `QC Warning`) %>%
          dplyr::rename(OlinkID = Assay, Assay = Name)
      } else {
        for (j in 1:nr_plates) {
          assay_name_by_plate <- dplyr::bind_cols(assay_name_list[[i]],
                                                  Unit=c(t(meta_data_list[[i]][5,])),
                                                  MissingFreq=c(t(meta_data_list[[i]][6,])),
                                                  LLOQ = as.numeric(c(t(meta_data_list[[i]][7,]))),
                                                  ULOQ = as.numeric(c(t(meta_data_list[[i]][8,]))),
                                                  Assay_Warning = c(t(meta_data_list[[i]][(9+(j-1)),])),
                                                  Plate_LQL = as.numeric(c(t(meta_data_list[[i]][(9+nr_plates+(j-1)),]))),
                                                  LOD = as.numeric(c(t(meta_data_list[[i]][(9+2*nr_plates+(j-1)),]))))
          if(NORM_FLAG == TRUE) {
            assay_name_by_plate <- dplyr::bind_cols(assay_name_by_plate,
                                                    Normalization = c(t(meta_data_list[[i]][9+3*nr_plates,])))
          }
          panel_list_long[[(i-1)*j+j]] <- panel_list[[i]] %>%
            dplyr::mutate(SampleID = SampleID) %>%
            dplyr::mutate(Index = Index_nr) %>%
            tidyr::gather(Assay, NPX, -SampleID,-`QC Warning`,-`Plate ID`,
                          -Index,
                          -dplyr::matches("QC Deviation Inc Ctrl"),
                          -dplyr::matches("QC Deviation Det Ctrl")) %>%
            dplyr::filter(`Plate ID` == plates[[j]]) %>%
            dplyr::left_join(assay_name_by_plate, by = c('Assay' = 'ID')) %>%
            dplyr::select(SampleID, Index, Assay, UniProt, Name, MissingFreq,
                          Panel,`Plate ID`,`QC Warning`, LOD, NPX, Unit,
                          ULOQ, LLOQ, Plate_LQL, Assay_Warning,
                          dplyr::matches("Normalization"),
                          dplyr::matches("QC Deviation Inc Ctrl"),
                          dplyr::matches("QC Deviation Det Ctrl")) %>%
            dplyr::rename(PlateID = `Plate ID`) %>%
            dplyr::rename(QC_Warning = `QC Warning`) %>%
            dplyr::rename(OlinkID = Assay, Assay = Name)
        }
      }
    }

    if (!is_npx_data) {
      for (i in 1:(nr_panel*nr_plates)) {
        panel_list_long[[i]] <- panel_list_long[[i]] %>%
          dplyr::rename(Plate_LOD = LOD) %>%
          dplyr::rename(Quantified_value = NPX)
      }
    }

    out <- dplyr::bind_rows(panel_list_long) %>%
      dplyr::filter(!is.na(SampleID)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Panel_Version = gsub(".*\\(","",Panel)) %>%
      dplyr::mutate(Panel_Version = gsub("\\)","",Panel_Version)) %>%
      dplyr::mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>%
      dplyr::mutate(Panel = stringr::str_to_title(Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 96", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 48", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Olink", "", Panel)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "left")) %>%
      tidyr::separate(Panel, " ", into = c("Panel_Start", "Panel_End"), fill = "right") %>%
      dplyr::mutate(Panel_End = ifelse(grepl("Ii", Panel_End), stringr::str_to_upper(Panel_End), Panel_End)) %>%
      dplyr::mutate(Panel_End = ifelse(is.na(Panel_End), " ", Panel_End)) %>%
      dplyr::mutate(Panel = paste("Olink", Panel_Start, Panel_End)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "right")) %>%
      dplyr::select(-Panel_Start, -Panel_End) %>%
      dplyr::select(SampleID, Index, OlinkID,
                    UniProt, Assay, MissingFreq,
                    Panel,Panel_Version,PlateID,
                    QC_Warning,dplyr::matches("Plate_LQL"),
                    dplyr::matches("LOD"),
                    dplyr::matches("Plat_LOD"),
                    dplyr::matches("LLOQ"),
                    dplyr::matches("ULOQ"),
                    dplyr::matches("NPX"),
                    dplyr::matches("Quantified_value"),
                    dplyr::matches("Unit"),
                    dplyr::matches("Assay_Warning"),
                    dplyr::matches("Normalization"),
                    dplyr::matches("*Inc Ctrl*"),
                    dplyr::matches("*Det Ctrl*"))
    }



  if (is_npx_data) {
    # Check for data completeness and warn on problems
    check_data_completeness(out)
    out <- out %>%
      mutate(NPX = as.numeric(NPX))
  }
  if (!is_npx_data){
    message("QUANT data detected. Some downstream functions may not be supported.")
    out <- out %>%
      mutate(Quantified_value = as.numeric(Quantified_value))
  }

  return(out)
}

#' Check data completeness
#'
#' Throw informative warnings if a dataset appears to have problems
#'
#' @param df a NPX dataframe, e.g. from read_NPX()
#'
#' @return None. Used for side effects (warnings)
#'
#' @examples
#'
#' npx_data1 %>%
#'     dplyr::mutate(NPX = dplyr::if_else(
#'                          SampleID == "A1" & Panel == "Olink Cardiometabolic",
#'                          NA_real_,
#'                          NPX)) %>%
#'     OlinkAnalyze:::check_data_completeness()

check_data_completeness <- function(df){
  NA_panel_sample <- df %>%
    group_by(SampleID, Panel) %>%
    summarize(`number of NA values` = sum(is.na(NPX)),
              .groups = "drop") %>%
    filter(`number of NA values` > 0) %>%
    as.data.frame()

  if (dim(NA_panel_sample)[1] > 0) {
    warning("The following samples have NA NPX values for the following panels.\n\n",
            print_and_capture(NA_panel_sample))
  }

}
