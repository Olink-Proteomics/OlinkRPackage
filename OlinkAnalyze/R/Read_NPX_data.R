
#' Convert Excel-style column names to column number
#'
#' @param character column name (e.g. 'AWG') or a number to pass-through
#'
#' @return return numeric column number
colname_to_number <- function(x) {
  # return any numbers passed in
  if(is.numeric(x)) { return(x) }
  
  # letters encoding
  encoding <- setNames(seq_along(LETTERS), LETTERS)
  
  # uppercase
  x <- toupper(x)
  
  # convert string to a list of vectors of single letters
  x <- strsplit(x, split = "")
  
  # convert each letter to the corresponding number
  # calculate the column number
  # return a numeric vector
  sapply(x, function(xs) sum(encoding[xs] * 26^((length(xs)-1):0)))
}


#' Function to read NPX data into long format
#'
#' Imports an NPX file exported from NPX Manager.
#' No alterations to the output NPX Manager format is allowed.
#'
#' @param filename Path to file NPX Manager output file.
#' @param extra_metadata_start_col optional column number or (Excel) column name
#'   indicating extra metadata starting column
#'
#' @return A tibble in long format.
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#' @importFrom magrittr %>%
#' @importFrom tools file_ext
#' @importFrom dplyr as_tibble distinct pull filter bind_cols mutate left_join select rename matches bind_rows
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect str_replace_all str_to_upper str_to_title
#' @importFrom tidyr tibble gather separate


read_NPX <- function(filename, extra_metadata_start_col=NULL){
  
  # If the file is csv or txt, read_NPX assumes that the file is explore data in long format
  if (tools::file_ext(filename) %in% c("csv","txt")) {
    #read file using ; as delimiter
    out <- read.table(filename, header = TRUE, sep=";", stringsAsFactors = F,
                      na.strings = c("NA",""))
    if (is.data.frame(out) & ncol(out) == 1) {
      #if only one column in the data, wrong delimiter. use , as delimiter
      out <- read.table(filename, header = TRUE, sep=",", stringsAsFactors = F,
                        na.strings = c("NA",""))
    }
    #check that all colnames are present
    match_old_header <- all(c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                              "MissingFreq", "Panel", "Panel_Version", "PlateID",
                              "QC_Warning", "LOD", "NPX") %in% colnames(out))
    match_new_header <- all(c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                              "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                              "QC_Warning", "LOD", "NPX") %in% colnames(out))
    if (match_old_header || match_new_header) {
      out$NPX <- as.numeric(out$NPX)
      out$LOD <- as.numeric(out$LOD)
      out$MissingFreq <- as.numeric(out$MissingFreq)
      out$SampleID <- as.character(out$SampleID)
      return(dplyr::as_tibble(out))
    } else {
      missing.cols <- setdiff(c("SampleID", "Index", "OlinkID", "UniProt",
                                "Assay", "MissingFreq", "Panel", "Panel_Lot_Nr",
                                "PlateID", "QC_Warning", "LOD", "NPX"),
                              colnames(out))
      #If columns are missing, stop and print out which are missing
      stop(paste0("Cannot find columns ", paste(missing.cols,collapse=",")))
    }
  }
  NORM_FLAG <-  FALSE

  # Check if the data is npx or concentration as well as if it is tg48 or tg96

  data_type <- readxl::read_excel(filename, range='A2',
                                  col_names = F, .name_repair="minimal")
  if (grepl('NPX', data_type, fixed=TRUE)) {
    is_npx_data <- TRUE
    n_max_meta_data <- 4

    # Check whether it is target 48 or 96
    panel_name <- readxl::read_excel(filename, range='B3',
                                     col_names = F, .name_repair="minimal")
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
                                  col_names = F, .name_repair="minimal")
  meta_dat[4,1] <- 'SampleID'
  NR_DEVIATIONS <- sum(stringr::str_detect(meta_dat[2,],
                                           'QC Deviation from median'), na.rm=TRUE)
  control_index <- (stringr::str_detect(meta_dat[2,], 'Det Ctrl') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 2') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 1') |
                      stringr::str_detect(meta_dat[2,], 'Ext Ctrl'))
  
  # some data files are delivered with extra sample metadata columns
  # after the data.  Identify the last data columns and extract this 
  # sample metadata if present
  extra_metadata_flag <- FALSE
  last_file_col<-length(control_index)
  last_data_col <- last_file_col
  
  # if defined, use extra_metadata_start_col to determine where extra
  # metadata starts within the data file, otherwise try to detect 
  # additional metadata columns based on extracted control data
  if(!is.null(extra_metadata_start_col)) {
    # convert extra_metadata_start_col to a number if necessary
    extra_metadata_start_col <- colname_to_number(extra_metadata_start_col)
    # infer the last data column
    last_data_col <- extra_metadata_start_col-1
    # redefine control index vector
    control_index <- control_index[c(1:last_data_col)]
    
  } else {
    # redefine the control index by removing NA values
    # TODO: verify that any NAs are at the very end and not at the start or middle of the control_index
    control_index<-na.omit(control_index)
    # infer the last data column based on the control_index length
    last_data_col<-length(control_index)
    # infer starting point of any extra metadata
    extra_metadata_start_col <- last_data_col+1
  }
  
  # if the file's last column is greater than the last data column, assume 
  # extra metadata fields exist and should be collected
  if(last_file_col > last_data_col) {
    extra_metadata_flag <- TRUE
    
    extra_metadata_header <- meta_dat[,c(extra_metadata_start_col:last_file_col)] %>% 
      tidyr::drop_na() %>%
      head(n=1) %>% 
      unlist(., use.names=FALSE)
    meta_dat <- meta_dat[, c(1:last_data_col)]
    message(sprintf("Additional metadata columns found starting at column %d: %s.", extra_metadata_start_col, paste0("'", extra_metadata_header,"'", collapse=", ") ))
  }
  
  meta_dat[4, control_index] <- meta_dat[2, control_index]
  meta_dat[3, control_index] <- '-'
  NR_CONTROLS <- sum(control_index)
  nr_panel<-(ncol(meta_dat)-1-NR_DEVIATIONS-NR_CONTROLS)/(BASE_INDEX+2)

  nr_col <- ncol(meta_dat)
  names(meta_dat) <- as.character(1:nr_col)

  meta_dat <- meta_dat %>%
    dplyr::rename(Name = `1`)

  # Load NPX or QUANT data including the last rows of meta data
  dat <- readxl::read_excel(filename, skip = n_max_meta_data+2, col_names = F,
                            .name_repair="minimal", col_types = c('text'))
  
  if(extra_metadata_flag) {
    extra_metadata <- cbind(dat[,1],         # column 1: SampleID
                            1:nrow(dat), # column 2: Index
                            dat[,c(extra_metadata_start_col:last_file_col)]) # columns 3-n: Other metadata
    names(extra_metadata) <- c("SampleID", "Index", extra_metadata_header)
    dat <- dat[,c(1:last_data_col)]
  }
  
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
                  dplyr::matches("*Det Ctrl*")) %>%
    {if (extra_metadata_flag) dplyr::left_join(., extra_metadata, by=c("SampleID", "Index")) else .}
}
