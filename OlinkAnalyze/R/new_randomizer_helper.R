# Input checks ------------------------------------------------------------
randomizer_check <- function(manifest,
                             subject_column,
                             study_column,
                             available_spots,
                             plate_size,
                             software,
                             total_controls,
                             supported_software) {


  # SampleID format (if FALSE, result = ERROR) -------------------------------
  # SampleID column is present
  # SampleID is string/char
  # SampleID only contains following characters: A-Z, a-z, 0-9, ., _, -, #, " "
  # SampleID is less than 100 characters
  # If software is "AS" SampleID is less than 30 characters
  # No duplicated SampleIDs
  # All samples have a SampleID


  # Columns present (if FALSE, result = ERROR) -------------------------------
  # if subject_column != NULL, subject_column is in names(manifest)
  # if subject_column != NULL, all samples have a subject id
  # if study_column != NULL, study_column is in names(manifest)


  # Correct sizes -----------------------------------------------------------
  # WARNING: if total_controls != NULL, total_controls should be 8, 10, or >12
  # ERROR: if plate_size != NULL, plate_size should be 96 or 48
  # ERROR: if available_spots != NULL, sum(available_spots) must be >
  #       number of samples (length(unique(manifest$SampleID)))
  # ERROR: if available_spots != NULL, available_spots must fit on the plate


  # Supported Software ------------------------------------------------------
  # WARNING: If software is not one of supported software,
  #         manifest will not be formatted

  return()
}


# Determine type of randomization -----------------------------------------
randomizer_type <- function(subject_column,
                            study_column) {

  longitudinal <- FALSE
  multistudy <- FALSE
  # if subject_column != NULL, longitudinal = TRUE
  # if study_column != NULL, multistudy = TRUE

  return(list(
    longitudinal = longitudinal,
    multistudy = multistudy
  ))
}


# Generate plate layout ---------------------------------------------------
generate_plate_layout <- function(total_samples,
                                  product,
                                  plate_size,
                                  total_controls,
                                  num_rand_controls,
                                  available_spots) {

  # Checks ------------------------------------------------------------------

  # ERROR: Either product != NULL or plate_size and total_controls != NULL
  # ERROR: If product = "Target 48" or "Focus", plate_size cannot be NULL
  # ERROR: If product = "Focus", total_controls cannot be NULL

  #


  # Generate available spots if not available -------------------------------
  # Calculate number of control wells
  control_wells <- total_controls - num_rand_controls

  if (is.na(available_spots)) {

    available_spots <- gen_avail_spots(product = product,
                                       plate_size = plate_size,
                                       total_controls = total_controls,
                                       num_rand_controls = num_rand_controls,
                                       control_wells = control_wells)
  }

  # Determine well ids and generate blank plate layout
  well_ids <- generate_well_ids(plate_size = plate_size,
                                available_spots = available_spots,
                                control_wells = control_wells,
                                starting_well = starting_well,
                                product = product)

  return(well_ids)
}

# Available spots for plate layout ----------------------------------------
gen_avail_spots <- function(product,
                            plate_size,
                            total_controls,
                            num_rand_controls,
                            control_wells) {
  # Set plate_size and total_controls based on product if either are NULL
  # Print product, plate_size, total_controls




  # Calculate number of plates needed
  nplates <- ceiling(nrow(manifest) / (plate_size - total_controls))

  # Generate available spots: plate_size - control_wells repeated nplate times

  return(available_spots)
}


# Generate well ids ------------------------------------------------------
generate_well_ids <- function(plate_size,
                              available_spots,
                              control_wells,
                              starting_well,
                              product) {
  # Generate starting well ids (starting_well + plate_size)
  # Subset for each plate based on available_spots
  # Subset locations for controls
  control_wells
  # Subset locations for samples
  sample_wells
  # For T48 on 96 well plate, have odd and even plates to use half the plate
  # concatenate plates and order numerically (not alphabetically; 1,2 not 1,10)

  return(list(sample_wells = sample_wells,
              control_wells = control_wells))
}

# Internal randomization -----------------------------------------------------

internal_randomizer <- function(manifest,
                                well_ids,
                                seed,
                                iterations,
                                longitudinal,
                                multistudy,
                                subject_column,
                                study_column,
                                num_rand_controls,
                                product) {
  # Add randomized controls to manifest


  if (multistudy == TRUE) {
    well_ids <- internal_study_grouping(manifest = manifest,
                                        study_column,
                                        well_ids)
    if (longitudinal == TRUE) {
      # within each study, randomize subjects (group_by study)
      manifest <- internal_subject_randomizer(manifest,
                                              subject_column,
                                              well_ids)
    } else {
      # within each study, randomize samples (group_by study)
      manifest <- internal_complete_randomizer(manifest,
                                               well_ids)
    }
  } else if (longitudinal == TRUE) {
    manifest <- internal_subject_randomizer(manifest,
                                            subject_column,
                                            well_ids,
                                            iterations)
  } else {
    manifest <-  internal_complete_randomizer(manifest,
                                              well_ids,
                                              seed)
  }
  manifest <- add_control_strip(well_ids,
                                manifest,
                                product,
                                total_controls,
                                num_rand_controls)

  return(manifest)
}


# Group studies together --------------------------------------------------

internal_study_grouping <- function(manifest,
                                    study_column,
                                    well_ids) {
  # Sort manifest by study_column
  # arrange study to plate/well_ids, starting with the largest study

  return(well_ids_study)
}


# Randomize subjects across plates ----------------------------------------

internal_subject_randomizer <- function(manifest,
                                        subject_column,
                                        well_ids,
                                        iterations) {
  # Determine number of subjects per sample in manifest using subject_column
  # Assign subject to plates starting with subject with the most samples
  # There are other methods, this is First fit decreasing
  # If all the subjects dont fit, increase iteration and start next subject??
  # For each plate
  manifest <- internal_complete_randomizer(manifest,
                                           well_ids,
                                           seed)
}


# Complete randomization --------------------------------------------------

internal_complete_randomizer <- function(manifest,
                                         well_ids,
                                         seed) {
  # Given a set of wells, randomly assign the samples to those wells

}

# Control strip -----------------------------------------------------------

add_control_strip <- function(well_ids,
                              manifest,
                              product,
                              total_controls,
                              num_rand_controls) {
  # Add the control strip to the wells based on the product
  # if num_rand_controls != 0 -> decrease number of negative controls by this number
  # write a message

  control_strip_setup <- list(
    "Explore HT" = list("NC" = 2 - num_rand_controls,
                        "SC" = 3,
                        "PC" = 5),
    "Reveal" = list("NC" = 2 - num_rand_controls,
                        "SC" = 3,
                        "PC" = 5),
    "Explore 3072" = list("NC" = 3,
                          "SC" = 2,
                          "PC" = 3),
    "Target 96" = list("NC" = 3,
                       "SC" = 2,
                       "IPC" = 3),
    "Target 48" = list("NC" = 2,
                       "SC" = 3,
                       "CAL" = 3),
    "Flex" = list("NC" = 2,
                  "SC" = 3,
                  "CAL" = 3),
    "Focus" = list("NC" = 3,
                   "SC" = total_controls - 12,
                   "CAL" = 9)

  )

  # From the set up and the well ids that are allocated to the control wells
  # generate the sample type, sample id, and well ID combination for each plate.
  # See specification for the product layout
  # Add this to the randomized manifest


  manifest <- set_sample_type(product,
                              manifest) # add the sample types
  return(manifest)
}

# Set Sample Types column ------------------------------------------------------------

set_sample_type <- function(product,
                            manifest) {
  # Set sample type based on table 6.3 in specification

  return(manifest)

}

# Format output for software ----------------------------------------------

randomizer_output <- function(rand_manifest,
                              software){

  # "NPX Map", "NPX Signature", or "AS"

  # AS Column Names: "Plate ID", "Well ID", "Unique Sample ID", "Sample Type", "Sample Volume", "Subject ID", "Additional info"

  # NPX Map Column Names: "sample_id", "well_id", "sample_type" (name of file will contain plate id)

  # NPX Signature Column Names: "SampleID", "PlateID", "WellID", "SampleType" (name of file should contain plate id)

  # Provide instructions for exporting files as a message

}


