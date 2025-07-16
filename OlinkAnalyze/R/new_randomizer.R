
#' Randomly assign samples to plates
#'
#' Generates a scheme for how to plate samples with an option to keep subjects on the same plate and/or to keep studies together.
#'
#' @param manifest tibble/data frame in long format containing sample ids (in SampleID column)
#' @param seed integer. seed to set for reproducibility (optional)
#' @param product string. name of Olink product. used to set plate size and number of controls (optional)
#' @param num_rand_controls integer. number of controls to randomize on the plate. (default = 0)
#' @param total_controls integer. total number of controls. (default = 8)
#' @param available_spots Numeric vector. Takes a vector equal to the number of plates to be used indicating the number of wells available for samples on each plate. (optional)
#' @param iterations 	Integer. Number of iterations for fitting subjects on the same plate. (default = 500)
#' @param subject_column String. Column name of the subject ID column. Cannot contain missing values. If provided, subjects are kept on the same plate. This argument is used for longitudinal studies and must be a separate column from the SampleID column. (optional)
#' @param study_column String. Name of column that includes study information. For when multiple studies are being plated and randomizing within studies. If study column is present in manifest, within study randomization will be performed. (optional)
#' @param plate_size integer. number of wells on a plate. (default = 96)
#' @param software string. name of NPX software being used. Must be one of "NPX Map", "NPX Signature", or "AS"
#' @param starting_well string. well_id of first well on plate
#'
#' @return
#' @export
#'
#' @examples
olink_plate_randomizer <- function(manifest,
                                   seed = NULL,
                                   product= NULL,
                                   num_rand_controls = 0,
                                   total_controls = NULL,
                                   available_spots = NULL,
                                   iterations = 500,
                                   subject_column = NULL,
                                   study_column = NULL,
                                   plate_size = NULL,
                                   software,
                                   starting_well = "A1"){

  supported_software <- c("NPX Map", "NPX Signature", "AS")

  randomizer_check(manifest = manifest,
                   subject_column = subject_column,
                   available_spots = available_spots,
                   plate_size = plate_size,
                   software = software,
                   total_controls = total_controls,
                   supported_software = supported_software)

  type_list <- randomizer_type(subject_column = subject_column,
                               study_column = study_column)

  well_ids <- generate_plate_layout(total_samples = nrow(manifest),
                                    product = product,
                                    plate_size = plate_size,
                                    total_controls = total_controls,
                                    num_rand_controls = num_rand_controls,
                                    available_spots = available_spots,
                                    starting_well = starting_well)

  rand_manifest <- internal_randomizer(manifest,
                                       well_ids,
                                       num_rand_controls,
                                       total_controls,
                                       plate_size,
                                       longitudinal = type_list$longitudinal,
                                       multistudy = type_list$multistudy,
                                      )
  rand_manifest <- set_sample_type(rand_manifest,
                                   product)

  if (software %in% supported_software){
    rand_manifest <- randomizer_output(rand_manifest,
                                       software)
  }

  return(rand_manifest)
}
