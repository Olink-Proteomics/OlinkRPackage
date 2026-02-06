#' Generates a scheme for how to plate samples with an option to 
#' keep subjects on the same plate and/or to keep studies together.
#'
#' @param manifest tibble/data frame in long format containing all sample ID's. 
#' Sample ID column must be named SampleID.
#' @param software string. name of software, one of "NPX Map", "NPX Signature",
#'  or "AS" (For projects run in Olink's Analysis Service labs)
#' @param seed Seed to set. Highly recommend setting this for reproducibility.
#' @param product String. Name of Olink product used to set plate size and 
#' determine plate layout. Optional.
#' @param subject_column 	(Optional) Column name of the subject ID column. 
#' Cannot contain missing values. If provided, subjects are kept on the same 
#' plate. This argument is used for longitudinal studies and must be a separate
#'  column from the SampleID column.
#' @param study_column 	String. Optional. Name of column that includes study 
#' information. For when multiple studies are being plated and randomizing 
#' within studies. If study column is present in manifest, within study 
#' randomization will be performed.
#' @param total_controls Numeric. Number of controls on each plate (default = 8)
#' @param num_rand_controls Numeric. Number of randomized controls (default = 0)
#' @param plate_size 	Integer. Either 96 or 48. (default = 96)
#' @param available_spots Numeric vector. Length is equal to the number of 
#' plates to be used and each value indicates the number of wells available on 
#' each plate.
#' @param iterations Number of iterations for fitting subjects on the same plate.
#'
#' @return A "tibble" assigning each sample to a well position on a plate.
#'  Columns and format will depend on software.
#' @export
#'
#' @examples
olink_plate_randomizer <- function(manifest,
                                   software,
                                   seed = NULL,
                                   product = NULL,
                                   subject_column = NA,
                                   study_column = NA,
                                   total_controls = 8,
                                   num_rand_controls = 0,
                                   plate_size = NULL,
                                   available_spots = NULL,
                                   iterations = 500)
{
randomizer_input_check(manifest = manifest,
                       software = software,
                       subject_column = subject_column,
                       study_column = study_column,
                         total_controls = total_controls,
                         available_spots = available_spots,
                         plate_size = plate_size)
  manifest <- manifest_clean(manifest = manifest)
  
  type <- randomizer_type(subject_column = subject_column,
                          study_column = study_column)
  
  plate_layout <- generate_plate_layout(product = product,
                                        plate_size = plate_size,
                                        software = software,
                                        total_controls = total_controls,
                                        num_rand_controls = num_rand_controls,
                                        available_spots = available_spots)
  
  
  
  manifest_list <- internal_randomizer(manifest = manifest,
                                       type = type,
                                       plate_layout = plate_layout,
                                       num_rand_controls = num_rand_controls)
  
  randomized_manifest <- randomizer_output(manifest_list = manifest_list,
                                           software = software)
  
  return(randomized_manifest)
}


#' Checking the inputs for plate randomizer
#'
#' @param manifest 
#' @param software 
#' @param subject_column 
#' @param study_column 
#' @param total_controls 
#' @param available_spots 
#' @param plate_size 
#'
#' @return
#' @keywords internal
#'
#' @examples
randomizer_input_check <- function(manifest,
                                   software,
                                   subject_column,
                                   study_column,
                                   total_controls,
                                   available_spots,
                                   plate_size){
  
  # Sample ID present
  
  if(!all(names(manifest) %in% c("SampleID"))){
    cli::cli_abort("manifest does not contain SampleID column.")
  }
  
  # SampleID criteria - characters
  
  if(!is.character(manifest[["SampleID"]])){
    manifest$SampleID <- as.character(manifest$SampleID)
    cli::cli_inform("SampleIDs converted to character.")
  }
  
  accepted_characters <- c(LETTERS, 
                           letters,
                           as.character(0:9),
                           "\.",
                           "_",
                           "-",
                           "#",
                           " ")
  not_accepted_SampleIDs <- stringr::str_remove_all(manifest$SampleID, 
                                                    paste0(accepted_characters, 
                                                           collapse = "|"))
  
  if(!not_accepted_SampleIDs == ""){
    bad_IDs <- manifest$SampleID[!not_accepted_SampleIDs == ""]
    cli::cli_abort(c(paste("Invalid character detected in SampleIDs: \n",
                         paste(bad_IDs, collapse = "\n")),
                   "i" = paste0("SampleIDs can only contain A-Z, ",
                                "a-z, 0-9, #, \".\", \"_\", \"-\", and \" \"" )))
  }
  
  if(software == "AS" & any(stringr::str_detect(manifest$SampleID, "\.")){
     cli::cli_abort("For AS, SampleIDs cannot have \".\"")
  }
  
  # SampleID criteria - sample length
  id_max_length <- ifelse(software == "AS", 30, 100)
  
  if(any(stringr::str_length(manifest$SampleID) > id_max_length)){
    cli::cli_abort(paste("Sample ID is limited to",
                         id_max_length,
                         "characters"))
  }
  
  # Sample and study columns
  
  if (!is.na(subject_column)){
    if(!any(names(manifest) %in% subject_column)){
      cli::cli_abort(paste(subject_column, "is missing from manifest."))
    }
    if(any(is.na(manifest[[subject_column]]))){
      cli::cli_abort(paste("Missing", 
                           subject_column,
                           "in one or more samples."))
    }
  }
  
  if(!is.na(study_column)){
    if(!any(names(manifest) %in% study_column)){
      cli::cli_abort(paste(study_column, "is missing from manifest."))
    }
  }
  
  # Total sample requirements
  
  if(!is.na(total_controls) & 
     !(total_controls %in% c(8, 10)) &
     total_controls < 12){
    cli::cli_alert(c(
      "Unexpected `total_controls` value detected.",
      "i" = paste("`total_controls` is set to", total_controls,
                  "Expected values are 8, 10, or greater than 12.")))
  }
  
  # Plate size must be 48 or 96
  if(!is.na(plate_size) &
     !(plate_size %in% c(96, 48))){
  cli::cli_abort("Plate size must be 96 or 48.")
  }
  
  if(!is.na(available_spots) & 
     sum(available_spots) < length(unique(manifest[["SampleID"]]))){
    cli::cli_abort("More samples detected than spots in `available_spots`")
  }
  
  if(!is.na(available_spots) &
     !is.na(plate_size) &
     any(available_spots > plate_size)){
    cli::cli_abort(paste("`available_spots` is larger than `plate_size`",
    "for 1 or more plates."))
  }
  
  if(!software %in% c("NPX Map", "NPX Signature", "AS")){
    cli::cli_alert_danger(c("`software` is not a supported NPX software.",
                     "!" = "Manifest will not be reformatted for software."))
  }
  return()
}

#' Title
#'
#' @param manifest 
#'
#' @return
#' @keywords internal
#'
#' @examples
manifest_clean <- function(manifest){
  
}

#' determine what type of randomization is being performed
#'
#' @param subject_column 
#' @param study_column 
#'
#' @return
#' @keywords internal
#'
#' @examples
randomizer_type <- function(subject_column,
                            study_column){
  type <- list(longitudinal = !is.na(subject_column),
               multistudy = !is.na(study_column))
  return(type)
}

#' Title
#'
#' @param product 
#' @param plate_size 
#' @param software 
#' @param total_controls 
#' @param num_rand_controls 
#'
#' @return
#' @export
#'
#' @examples
generate_plate_layout <- function(product,
                                  plate_size,
                                  software,
                                  total_controls,
                                  num_rand_controls,
                                  available_spots){
  # Must have either product or plate_size and total controls
  message("Generating plate layout")
  if(any(is.na(product), all(is.na(plate_size),is.na(total_controls)))){
    cli::cli_abort(paste0("`product` or `plate_size` and `total_controls`",
                          " are needed to generate plate layout")
  }
  
  if(product %in% c("Flex", "Target 48") & is.na(plate_size)){
    cli::cli_abort(paste0("`plate_size` required for product ", product))
  }
  
  if(product == "Focus" & total_controls == 8){
    cli::cli_alert("Default value (8) detected for `total_controls` in Focus.",
                   "i"= paste0("Check that `total_controls` has been set ",
                               "according to plate design.")
  }
  if(is.na(available_spots)){
    available_spots_list <- generate_available_spots(product = product,
                                                plate_size = plate_size,
                                                total_controls = total_controls,
                                                num_rand_controls = num_rand_controls)
    available_spots <- available_spots_list$available_spots
    plate_size <- available_spots_list$plate_size
    control_wells <- available_spots_list$control_wells
  }  else {
    control_wells <- total_controls - num_rand_controls
  }
  
  well_ids <- generate_well_ids(plate_size = plate_size,
                                available_spots = available_spots,
                                control_wells = control_wells,
                                starting_well = "A1")
  
} 

#' Title
#'
#' @param manifest 
#' @param type 
#' @param plate_layout 
#' @param num_rand_controls 
#'
#' @return
#' @export
#'
#' @examples
internal_randomizer <- function(manifest,
                                type,
                                plate_layout,
                                num_rand_controls){
  
} 

#' Title
#'
#' @param product 
#' @param plate_size 
#' @param software 
#' @param total_controls 
#' @param num_rand_controls 
#'
#' @return
#' @export
#'
#' @examples
generate_available_spots <- function(product,
                                     plate_size,
                                     software,
                                     total_controls,
                                     num_rand_controls){
  default_layout <- data.frame(name = c("Explore HT",
                                        "Reveal",
                                        "Explore 384",
                                        "Explore 3072",
                                        "Target 96",
                                        "Target 48",
                                        "Flex",
                                        "Focus"),
                               size = c(96,
                                        96,
                                        96,
                                        96,
                                        96,
                                        plate_size,
                                        plate_size,
                                        96),
                               ctrl_num = c(10,
                                            10,
                                            8,
                                            8,
                                            8,
                                            8,
                                            8,
                                            total_controls))
  if(!is.na(product) & any(is.na(plate_size), is.na(total_controls))){
    if(is.na(plate_size)){
      plate_size <- default_layout$size[default_layout$name == product]
    }
    if(is.na(total_controls)){
      total_controls <- default_layout$ctrl_num[default_layout$name == product]
    }
  }
  
  message(paste0("Detected ",
                 product, " with plate size of ",
                 plate_size, " and ",
                 total_controls, " controls."))
  control_wells <- total_controls - num_rand_controls
  nplates <- ceiling(nrow(manifest)/(plate_size-total_controls))
  available_spots <- rep(plate_size-control_wells, times = nplates)
  return(list(available_spots = available_spots,
              plate_size = plate_size,
              control_wells = control_wells))
}

#' Title
#'
#' @param plate_size 
#' @param available_spots 
#' @param control_wells 
#' @param starting_well 
#'
#' @return
#' @export
#'
#' @examples
generate_well_ids <- function(plate_size,
                              available_spots,
                              control_wells,
                              starting_well
){
  plate_wells <- paste0(rep(LETTERS[1:8], times = 12), rep(1:12, each = 8))
  plate_wells <- plate_wells[1:(length(plate_wells)-control_wells)]
  nplates <- length(available_spots)
  plate1 <- plate_wells[which(plate_wells == starting_well):length(plate_wells)]
  

  
}

#' Title
#'
#' @param manifest_list 
#' @param software 
#'
#' @return
#' @export
#'
#' @examples
randomizer_output <- function(manifest_list,
                              software){
  
}