#' Function to read NPX data into long format
#'
#' Imports an NPX file exported from NPX Manager or MyData.
#' No alterations to the output NPX Manager format is allowed.
#'
#' @param filename Path to NPX Manager or MyData output file.
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
#' @importFrom dplyr as_tibble distinct pull filter bind_cols mutate left_join select rename matches bind_rows
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect str_replace_all str_to_upper str_to_title
#' @importFrom tidyr tibble gather separate
#' @importFrom zip unzip
#' @importFrom utils tail


read_NPX <- function(filename){

  # If the file is csv or txt, read_NPX assumes that the file is explore data in long format
  if (tools::file_ext(filename) %in% c("csv","txt","zip")) {
    file_is_compressed <- FALSE
    if (tools::file_ext(filename) == "zip") {

      file_is_compressed <- TRUE

      # check contents of the compressed file
      compressed_file_contents <- utils::unzip(zipfile = filename, list = T) %>%
        dplyr::filter(Name != "README.txt") %>%
        dplyr::pull(Name) # extract all but README.txt

      # check if MD5 file exists and prepare list of files to unzip
      if ("MD5_checksum.txt" %in% compressed_file_contents) {
        compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "MD5_checksum.txt"] # name of md5 .txt file
        compressed_file_csv <- compressed_file_contents[compressed_file_contents != "MD5_checksum.txt"]
        files_to_extract <- c(compressed_file_csv, compressed_file_chksm)
      } else if("checksum_sha256.txt" %in% compressed_file_contents){
        compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "checksum_sha256.txt"] # name of sha256 .txt file
        compressed_file_csv <- compressed_file_contents[compressed_file_contents != "checksum_sha256.txt"]
        files_to_extract <- c(compressed_file_csv, compressed_file_chksm)
      }else{
        compressed_file_chksm <- NA_character_
        compressed_file_csv <- compressed_file_contents
        files_to_extract <- compressed_file_csv
      }

      # check that there is only one NPX file left
      if (length(compressed_file_csv) != 1 || !(tools::file_ext(compressed_file_csv) %in% c("csv","txt"))) {
        stop("The compressed file does not contain a valid NPX file. Expecting: \"README.txt\", \"MD5_checksum.txt\" or \"checksum_sha256.txt\" and the NPX file.")
      }

      # unzip
      tmp_unzip_dir <- paste(tools::file_path_sans_ext(filename),
                             paste(sample(x = c(LETTERS, letters), size = 5, replace = T), collapse = ""),
                             sep = "_")
      zip::unzip(zipfile = filename, files = files_to_extract, exdir = tmp_unzip_dir, overwrite = T)

      # File name after unzip
      extracted_file_csv <- file.path(tmp_unzip_dir, compressed_file_csv)

      if (!is.na(compressed_file_chksm)) {
        extracted_file_chksm <- file.path(tmp_unzip_dir, compressed_file_chksm) # MD5 relative path

        # make the checksum filename easier to parse
        chksm_string <- tolower(tools::file_path_sans_ext(compressed_file_chksm))

        # check for matching
        extracted_file_chksm_con <- file(extracted_file_chksm, "r")
        if(stringr::str_detect(chksm_string, "md5")){
          if (readLines(con = extracted_file_chksm_con, n = 1) != unname(tools::md5sum(extracted_file_csv))) { # check if MD5's match
            # clean up files
            close(extracted_file_chksm_con)
            invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))
            stop(paste("MD5 checksum of NPX file does not match the one from \"MD5_checksum.txt\"! Loss of data?", sep = ""))
          }
          close(extracted_file_chksm_con)
        }else if(stringr::str_detect(chksm_string, "sha256")){
          chksm <- openssl::sha256(file(extracted_file_csv)) %>%
            stringr::str_replace(":","")
          if (readLines(con = extracted_file_chksm_con, n = 1, warn = FALSE) != chksm) { # check if Sha256's match
            # clean up files
            close(extracted_file_chksm_con)
            invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))
            stop(paste("Sha256 checksum of NPX file does not match the one from \"checksum_sha256.txt\"! Loss of data?", sep = ""))
          }
          close(extracted_file_chksm_con)
        }
      }

      filename <- extracted_file_csv
    }

    #read file using ; as delimiter
    out <- read.table(filename, header = TRUE, sep=";", stringsAsFactors = FALSE,
                      na.strings = c("NA",""))

    if (is.data.frame(out) & ncol(out) == 1) {
      #if only one column in the data, wrong delimiter. use , as delimiter
      out <- read.table(filename, header = TRUE, sep=",", stringsAsFactors = FALSE,
                        na.strings = c("NA",""))
    }

    # remove unzipped NPX file
    if (file_is_compressed == TRUE) {
      invisible(unlink(x = tmp_unzip_dir, recursive = TRUE)) # remove files
      rm(tmp_unzip_dir)
    }

    # Check that all column names are present
    # We have had 3 column names versions so far: 1, 1.1 and 2
    # please add newer versions to the list chronologically
    header_standard <-    c("SampleID", "Index", "OlinkID", "UniProt", "Assay", "MissingFreq",
                            "Panel", "PlateID", "QC_Warning", "LOD", "NPX")
    header_v        <- list("header_v1"   = c(header_standard, "Panel_Version"),
                            "header_v1.1" = c(header_standard, "Panel_Version", "Normalization", "Assay_Warning"),
                            "header_v2"   = c(header_standard, "Panel_Lot_Nr", "Normalization", "Assay_Warning"))
    header_match    <-  any( sapply(header_v, function(x) all(x %in% colnames(out))) ) # look for one full match

    if (header_match == TRUE) {

      out <- out %>%
        dplyr::mutate(NPX         = as.numeric(NPX),
                      LOD         = as.numeric(LOD),
                      MissingFreq = as.numeric(MissingFreq),
                      SampleID    = as.character(SampleID)) %>%
        dplyr::as_tibble()

      return(out)

    } else {

      # if there is a mismatch of the input data with the expected column names
      # - we pick the set of column names with the shortest distance to any of the expected ones
      # - if multiple matches occur, we pick the most recent
      header_diff <- lapply(header_v, function(x) setdiff(x, colnames(out))) %>%
        lapply(length)
      header_idx  <- unlist(header_diff) %>% min
      if(sum(header_diff == header_idx) > 1) {
        header_pick <- header_diff[header_diff == header_idx] %>% tail(1)
      } else {
        header_pick <- header_diff[header_diff == header_idx]
      }
      header_pick <- names(header_pick)

      # find missing columns
      missing_cols <- setdiff(header_v[[header_pick]], colnames(out))

      # If missing columns, stop and print out which are missing
      stop(paste0("Cannot find columns: ", paste(missing_cols, collapse = ",")))

    }
  }
  NORM_FLAG <-  FALSE

  # Check if the data is npx or concentration as well as if it is tg48 or tg96

  data_type <- readxl::read_excel(filename, range='A2',
                                  col_names = FALSE, .name_repair="minimal")
  if (grepl('NPX', data_type, fixed=TRUE)) {
    is_npx_data <- TRUE
    n_max_meta_data <- 4

    # Check whether it is target 48 or 96
    panel_name <- readxl::read_excel(filename, range='B3',
                                     col_names = FALSE, .name_repair="minimal")
    if (grepl('Target 48', panel_name, fixed=TRUE)) {
      target_type <- '48'
      BASE_INDEX <- 45
    } else {
      target_type <- '96'
      BASE_INDEX <- 92
    }
  } else if (grepl('Quantified', data_type, fixed=TRUE)) {
    # Quant data given, which also means it is target 48
    is_npx_data <- FALSE
    n_max_meta_data <- 5
    target_type <- '48'
    BASE_INDEX <- 45
  } else {
    stop("Cannot find whether the given data is NPX or concentration")
  }

  # Load initial meta data (the first rows of the wide file)
  meta_dat <-  readxl::read_excel(filename, skip = 2, n_max = n_max_meta_data,
                                  col_names = FALSE, .name_repair="minimal")
  meta_dat[4,1] <- 'SampleID'
  NR_DEVIATIONS <- sum(stringr::str_detect(meta_dat[2,],
                                           'QC Deviation from median'))
  control_index <- (stringr::str_detect(meta_dat[2,], 'Det Ctrl') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 2') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 1') |
                      stringr::str_detect(meta_dat[2,], 'Ext Ctrl'))
  meta_dat[4, control_index] <- meta_dat[2, control_index]
  meta_dat[3, control_index] <- '-'
  NR_CONTROLS <- sum(control_index)
  nr_panel<-(ncol(meta_dat)-1-NR_DEVIATIONS-NR_CONTROLS)/(BASE_INDEX+2)

  nr_col <- ncol(meta_dat)
  names(meta_dat) <- as.character(1:nr_col)

  meta_dat <- meta_dat %>%
    dplyr::rename(Name = `1`)

  # Load NPX or QUANT data including the last rows of meta data
  dat <- readxl::read_excel(filename, skip = n_max_meta_data+2, col_names = FALSE,
                            .name_repair="minimal", col_types = c('text'))

  nr_col <- ncol(dat)
  names(dat) <- as.character(1:nr_col)

  dat<-dat %>%
    dplyr::rename(Name = `1`)

  # Calc nbr of plates
  plates <- dat[,nr_col-nr_panel] %>% dplyr::distinct() %>% na.omit() %>% dplyr::pull()
  nr_plates <- length(plates)

  # Extract the meta data from the last rows of data
  missfreq<-dat %>% dplyr::filter(stringr::str_detect(Name, "Missing Data freq."))
  norm_method <- dat %>% dplyr::filter(stringr::str_detect(Name, "Normalization"))
  if (!is_npx_data) {
    assay_warning <- dat %>% dplyr::filter(stringr::str_detect(Name, "Assay warning"))
    Plate_LQL <- dat %>% dplyr::filter(stringr::str_detect(Name,
                                                    "Lowest quantifiable level"))
    LOD <- dat %>% dplyr::filter(stringr::str_detect(Name, "Plate LOD"))
    LLOQ <- dat %>% dplyr::filter(stringr::str_detect(Name, "LLOQ"))
    ULOQ <- dat %>% dplyr::filter(stringr::str_detect(Name, "ULOQ"))
  } else {
    LOD <- dat %>% dplyr::filter(stringr::str_detect(Name, "LOD"))
  }

  # Add the new meta data to ´meta_dat´
  meta_dat <- rbind(meta_dat,missfreq)
  if (!is_npx_data) {
    meta_dat <- rbind(meta_dat,LLOQ,ULOQ,assay_warning,Plate_LQL)
  }
  meta_dat <- rbind(meta_dat,LOD,norm_method)

  # Remove the meta data from dat
  if (is_npx_data) {
    nbr_meta_data_rows_bottom <- 3
  } else {
    nbr_meta_data_rows_bottom <- 4+3*nr_plates
  }
  if (nrow(norm_method) == 0) {
    nbr_meta_data_rows_bottom <- nbr_meta_data_rows_bottom - 1
  } else {
    NORM_FLAG <- TRUE
  }
  dat <- dat[c(-1*(nrow(dat) - nbr_meta_data_rows_bottom):nrow(dat)),]

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
    BASE_INDEX <- BASE_INDEX + NR_CONTROLS/nr_panel
  }

  # Construct a list of tibbles that match the long format
  for (i in 1:nr_panel) {

    panel_data[[i]]<-dat[,(2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX))]

    if (NR_DEVIATIONS == 0) {
      QC_list[[i]]<-dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]

      meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]


    } else {

      QC_list[[i]]<-dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel,
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1)+1)]

      meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel,
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel,
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+3*nr_panel)]

      meta_data_list[[i]][4,(BASE_INDEX+3)] <- "QC Deviation Inc Ctrl"
      meta_data_list[[i]][4,(BASE_INDEX+4)] <- "QC Deviation Det Ctrl"


    }

    meta_data_list[[i]][4,(BASE_INDEX+1)] <- meta_data_list[[i]][2,(BASE_INDEX+1)]
    meta_data_list[[i]][4,(BASE_INDEX+2)] <- meta_data_list[[i]][2,(BASE_INDEX+2)]


    panel_list[[i]]<-cbind(panel_data[[i]],QC_list[[i]])

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

    assay_name_list[[i]]<-tidyr::tibble(ID=c(t(meta_data_list[[i]][4,])),
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

      panel_list_long[[i]]<- panel_list[[i]] %>%
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
        if(NORM_FLAG == TRUE){
          assay_name_by_plate <- dplyr::bind_cols(assay_name_by_plate,
                                           Normalization = c(t(meta_data_list[[i]][9+3*nr_plates,])))
        }
        panel_list_long[[(i-1)*j+j]]<- panel_list[[i]] %>%
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

  dplyr::bind_rows(panel_list_long) %>%
    dplyr::filter(!is.na(SampleID)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Panel_Version = gsub(".*\\(","",Panel)) %>%
    dplyr::mutate(Panel_Version = gsub("\\)","",Panel_Version)) %>%
    dplyr::mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>%
    dplyr::mutate(Panel = stringr::str_to_title(Panel)) %>%
    dplyr::mutate(Panel = gsub("Target 96", "", Panel)) %>%
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
