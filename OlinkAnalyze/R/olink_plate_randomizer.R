#' Check product name and set plate size accordingly
#' 
#' If plate size is not provided, function will use
#' accepted_olink_products tibble to map the product name to the plate size
#'
#' @param product (String) Name of product (needs to match one of names in
#' accepted_olink_platforms$name)
#'
#' @returns (Integer) Corresponding plate size per
#' accepted_olink_platforms$plate_size
#' @keywords internal
#'
product_to_platesize <- function(product) {
  if (!(product %in% accepted_olink_platforms[["name"]])) {
    cli::cli_abort(paste0("Product must be one of the following: ",
                          paste(accepted_olink_platforms[["name"]],
                                sep = " ",
                                collapse = ", ")))
  }
  plate_size <- accepted_olink_platforms |> 
    dplyr::filter(product == .data[["name"]]) |> 
    dplyr::pull(.data[["plate_size"]])
  return(plate_size)
}

#' Plot all plates colored by a variable
#'
#' Displays each plate in a facet with cells colored by the given variable
#' using ggplot and ggplot2::geom_tile.

#' @param data tibble/data frame in long format returned from the
#' olink_plate_randomizer function.
#' @param fill.color Column name to be used as coloring variable for wells.
#' @param PlateSize Integer. Either 96 or 48. 96 is default.
#' @param Product String. Name of Olink product used to set PlateSize if not
#' provided. Optional.
#' @param num_ctrl Numeric. Number of controls on each plate (default = 8)
#' @param rand_ctrl Logical. Whether controls are added to be randomized across
#' the plate (default = FALSE)
#' @param include.label Should the variable group be shown in the plot.
#' @return An object of class "ggplot" showing each plate in a facet with the
#' cells colored by values in column fill.color in input \code{data}.
#' @export
#' @examples
#' \donttest{
#' randomized_manifest <- OlinkAnalyze::olink_plate_randomizer(
#'   Manifest = manifest
#' )
#' OlinkAnalyze::olink_display_plate_layout(
#'   data = randomized_manifest,
#'   fill.color = "Site"
#' )
#' }
olink_display_plate_layout <- function(data,
                                       fill.color, # nolint: object_name_linter
                                       PlateSize = 96L, # nolint: object_name_linter
                                       num_ctrl = 8L,
                                       rand_ctrl = FALSE,
                                       Product, # nolint: object_name_linter
                                       include.label = FALSE) { # nolint: object_name_linter
  if (!missing(Product)) {
    PlateSize <- product_to_platesize(product = Product) # nolint: object_name_linter
  }

  if (!(PlateSize %in% unique(accepted_olink_platforms$plate_size))) {
    cli::cli_abort("Plate size needs to be either {
      cli::ansi_collapse(x = unique(accepted_olink_platforms$plate_size),
      sep = \",\",
      last = \" or \")
      }.")
  }

  ncols_per_plate <- PlateSize / 8

  missing_spots <- expand.grid(plate = unique(data$plate),
                               row = LETTERS[1:8],
                               column = paste("Column",
                                              1:(ncols_per_plate)),
                               fill.color = "Empty")
  missing_spots$unique.id <- paste(missing_spots[["plate"]],
                                   missing_spots[["row"]],
                                   missing_spots[["column"]])
  missing_spots <- missing_spots |>
    dplyr::filter(!.data[["unique.id"]] %in% paste(data[["plate"]],
                                                   data[["row"]],
                                                   data[["column"]])) |>
    dplyr::select(-dplyr::any_of("unique.id"))

  if (missing(fill.color)) { fill.color <- "plate" } # nolint: object_name_linter

  data$fill.color <- data[[fill.color]]
  data <- data |>
    dplyr::mutate(fill.color = ifelse(.data[["SampleID"]] == "CONTROL_SAMPLE",
                                      "CONTROL",
                                      fill.color))
  data <- data |>
    dplyr::select(dplyr::any_of(c("plate",
                                  "row",
                                  "column",
                                  "fill.color"))) |>
    dplyr::bind_rows(missing_spots) |>
    dplyr::mutate(row = factor(.data[["row"]], levels = LETTERS[8:1]),
                  column = factor(.data[["column"]],
                                  levels = paste("Column",
                                                 1:(ncols_per_plate))),
                  fill.color = factor(fill.color))

  fill_levels <- levels(data$fill.color)
  if ("Empty" %in% fill_levels) {
    hld <- which(fill_levels == "Empty")
    fills <- rep(NA, length(fill_levels))
    fills[-hld] <- olink_pal()(length(fill_levels) - 1)
    fills[hld] <- "#ffffff"
  } else {
    fills <- OlinkAnalyze::olink_pal()(length(fill_levels))
  }


  p <- ggplot2::ggplot(ggplot2::aes(x = .data[["column"]],
                                    y = .data[["row"]],
                                    fill = fill.color),
                       data = data) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::facet_wrap(~plate,
                        ncol = 1,
                        scales = "fixed") +
    OlinkAnalyze::set_plot_theme() +
    ggplot2::scale_fill_manual(values = fills) +
    ggplot2::labs(x = "",
                  y = "",
                  fill = fill.color) +
    ggplot2::scale_x_discrete(labels = paste0("Col",
                                              1:(ncols_per_plate)))

  if (include.label) {
    return(p +
             ggplot2::geom_text(ggplot2::aes(label = fill.color),
                                color = "black"))
  } else {
    return(p)
  }

}

#' @rdname olink_display_plate_layout
#' @export
olink_displayPlateLayout <- olink_display_plate_layout  # nolint: object_name_linter


#' Plot distributions of a given variable for all plates
#'
#' Displays a bar chart for each plate representing the distribution of the
#' given grouping variable on each plate using ggplot2::ggplot
#' and ggplot2::geom_bar.

#' @param data tibble/data frame in long format returned from the
#' olink_plate_randomizer function.
#' @param fill.color Column name to be used as coloring variable for wells.
#' @export
#' @return An object of class "ggplot" showing the percent distribution of
#' fill.color in each plate (x-axis)

#' @examples
#' \donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
#' \donttest{olink_display_plate_dist(data=randomized.manifest,
#' fill.color="Site")}
olink_display_plate_dist <- function(data,
                                     fill.color = "plate") { # nolint: object_name_linter

  check_columns(data, col_list = list(fill.color))
  data <- data |>
    dplyr::mutate(group.var = data[[fill.color]])

  p1 <- data |>
    dplyr::group_by(.data[["plate"]],
                    .data[["group.var"]]) |>
    dplyr::tally() |>
    dplyr::ungroup() |>
    dplyr::group_by(.data[["plate"]]) |>
    dplyr::mutate(percent = 100 * .data[["n"]] / sum(.data[["n"]])) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["plate"]],
                                 y = .data[["percent"]],
                                 fill = .data[["group.var"]])) +
    ggplot2::geom_bar(stat = "identity",
                      color = "gray") +
    OlinkAnalyze::olink_fill_discrete() +
    OlinkAnalyze::set_plot_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 0,
                                                       vjust = 0.5)) +
    ggplot2::labs(fill = fill.color,
                  x = "Plate",
                  y = "Percent") +
    ggplot2::theme(legend.position = "bottom")

  return(p1)
}

#' @rdname olink_display_plate_dist
#' @export
olink_displayPlateDistributions <- olink_display_plate_dist # nolint: object_name_linter


#' assign subject to a plate for longitudinal randomization
#'
#' @param plate_map character vector of locations available for samples
#' including plate, row and columns
#' @param manifest sample manifest mapping sample IDs and subject IDs
#' @param subject_id id of subject
#'
#' @returns plate_map adding sample IDs to plates keeping samples from the
#' same subject on the same plate
assign_subject2plate <- function(plate_map,
                                 manifest,
                                 subject_id) {

  # Select the samples from that subject
  sample_ids <- manifest$SampleID[manifest$SubjectID == subject_id]


  # Determine how many spots on each plate are still available
  spots_available <- plate_map |>
    dplyr::group_by(.data[["plate"]]) |>
    dplyr::summarize(available = sum(is.na(.data[["SampleID"]])))

  # do any of the plates have enough space for the samples from this subject
  if (any(spots_available$available >= length(sample_ids))) {
    # if so, choose a random plate where the samples will fit
    plate_assign <- sample(spots_available$plate[spots_available$available >=
                                                   length(sample_ids)],
                           1)
  } else {
    # return a warning
    return("This Sample does not fit!")
  }

  # list the samples on the plate where they can fit, note this doesnt
  # randomize them by sample, just subject per plate, this is where we can
  # add in the control samples if we are randomizing by control
  placement <- which(plate_map$plate == plate_assign &
                       is.na(plate_map$SampleID))[seq_along(sample_ids)]
  plate_map$SampleID[placement] <- sample_ids
  return(dplyr::as_tibble(plate_map))

}


#' Create empty plate layout
#'
#' @param nplates number of plates
#' @param nspots number of spots on each plate
#' @param nsamples number of samples
#' @param plate_size size of plate
#' @param num_ctrl number of controls
#' @param rand_ctrl if controls are randomized
#'
#' @keywords internal
#' @returns plate layout including plates, rows, and columns of available wells
generate_plate_holder <- function(nplates,
                                  nspots,
                                  nsamples,
                                  plate_size,
                                  num_ctrl,
                                  rand_ctrl) {
  # Both these are calculated in main function, is this sub function called
  # anywhere else plate size - # of ctrls = spots per plate
  spots_per_plate <- plate_size - num_ctrl * !rand_ctrl
  # cols given 8 row
  ncols_per_plate <- plate_size / 8


  #check right number of plates and spots given samples and given plate capacity
  if (nplates != length(nspots)) {
    cli::cli_abort("Vector of available spots must equal number of plates!")
  }
  if (any(nspots > spots_per_plate)) {
    cli::cli_abort(paste0("Number of samples per plates cannot exceed 40 ",
                          "for T48 and 88 for T96!"))
  }

  if (sum(nspots) < nsamples) {
    cli::cli_abort(paste0("More samples than available spots! ",
                          "Double check your numbers!"))
  }


  # Create a grid given number of columns - columns used by controls
  full_row_col <- expand.grid(column = paste0("Column ",
                                              1:(ncols_per_plate)),
                              row = LETTERS[1:8],
                              stringsAsFactors = TRUE) |>
    # String as factor needed so Column 10 doesn't come before column 2
    # in ordering
    dplyr::arrange(.data[["column"]], .data[["row"]]) |>
    #row is unnecessary here but its fine
    dplyr::slice_head(n = plate_size - num_ctrl * !rand_ctrl)

  # Instead we could generate entire plate and then filter out the end of the
  # plate (last column, end of rows, would need to make sure order is correct)
  # for the controls if they are not randomized throughout the plate

  # Create outline of data frame
  out <- data.frame(plate = NULL,
                    column = NULL,
                    row = NULL,
                    stringsAsFactors = FALSE)

  # Create plate layout based on available spots
  # this chunk could be simplified or at least clarified better -
  # maybe not using a for loop?
  for (i in 1:nplates) {
    hld <- cbind(plate = rep(paste0("Plate ", i), nspots[i]),
                 full_row_col[1:nspots[i], ])
    out <- rbind(out, hld)
  }
  return(out)
}




#' Randomly assign samples to plates
#'
#' Generates a scheme for how to plate samples with an option to keep subjects
#' on the same plate and/or to keep studies together.
#'
#' Variables of interest should if possible be randomized across plates to
#' avoid confounding with potential plate effects. In the case of multiple
#' samples per subject (e.g. in longitudinal studies), Olink recommends keeping
#' each subject on the same plate. This can be achieved using the SubjectColumn
#' argument.

#' @param Manifest tibble/data frame in long format containing all sample ID's.
#' Sample ID column must be named SampleID.
#' @param PlateSize Integer. Either 96 or 48. 96 is default.
#' @param Product String. Name of Olink product used to set PlateSize if not
#' provided. Optional.
#' @param SubjectColumn (Optional) Column name of the subject ID column. Cannot
#' contain missing values. If provided, subjects are kept on the same plate.
#' This argument is used for longitudinal studies and must be a separate column
#' from the SampleID column.
#' @param iterations Number of iterations for fitting subjects on the same
#' plate.
#' @param available.spots Numeric. Number of wells available on each plate.
#' Maximum 40 for T48 and 88 for T96. Takes a vector equal to the number of
#'  plates to be used indicating the number of wells available on each plate.
#' @param num_ctrl Numeric. Number of controls on each plate (default = 8)
#' @param rand_ctrl Logical. Whether controls are added to be randomized across
#'  the plate (default = FALSE)
#' @param seed Seed to set. Highly recommend setting this for reproducibility.
#' @param study String. Optional. Name of column that includes study
#' information. For when multiple studies are being plated and randomizing
#' within studies. If `study` column is present in manifest, within study
#' randomization will be performed.

#' @return A "tibble" including SampleID, SubjectID etc. assigned to
#' well positions.
#' Columns include same columns as Manifest with additional columns:
#' \itemize{
#'    \item{plate:} Plate number
#'    \item{column:} Column on the plate
#'    \item{row:} Row on the plate
#'    \item{well:} Well location on the plate
#' }

#' @export

#' @seealso \itemize{
#' \item{
#' \code{
#' \link[OlinkAnalyze:olink_displayPlateLayout]{olink_displayPlateLayout()}}
#' for visualizing the generated plate layouts}
#' \item{
#' \code{
#' \link[OlinkAnalyze:olink_displayPlateDistributions]{
#' olink_displayPlateDistributions()}}
#' for validating that sites are properly randomized}
#' }
#'
#' @examples
#' \donttest{
#' #Generate randomization scheme using complete randomization
#' randomized.manifest_a <- olink_plate_randomizer(manifest, seed=12345)
#'
#' # Generate randomization scheme that keeps subjects on the same plate
#' # (for longitudinal studies)
#' randomized.manifest_b <- olink_plate_randomizer(manifest,
#'                                                 SubjectColumn="SubjectID",
#'                                                 available.spots=c(88,88),
#'                                                 seed=12345)
#'
#' # Generate randomization scheme that keeps samples from the same
#' # study together
#' randomized.manifest_c <- olink_plate_randomizer(manifest, study = "Site")
#'
#' # Visualize the generated plate layouts
#' olink_displayPlateLayout(randomized.manifest_a, fill.color = 'Site')
#' olink_displayPlateLayout(randomized.manifest_a, fill.color = 'SubjectID')
#' olink_displayPlateLayout(randomized.manifest_b, fill.color = 'Site')
#' olink_displayPlateLayout(randomized.manifest_b, fill.color = 'SubjectID')
#' olink_displayPlateLayout(randomized.manifest_c, fill.color = 'Site')
#'
#' # Validate that sites are properly randomized
#' olink_displayPlateDistributions(randomized.manifest_a, fill.color = 'Site')
#' olink_displayPlateDistributions(randomized.manifest_b, fill.color = 'Site')
#' }
olink_plate_randomizer <- function(Manifest, # nolint: object_name_linter
                                   PlateSize = 96, # nolint object_name_linter
                                   Product, # nolint: object_name_linter
                                   SubjectColumn, # nolint: object_name_linter
                                   iterations = 500,
                                   available.spots, # nolint: object_name_linter
                                   num_ctrl = 8L,
                                   rand_ctrl = FALSE,
                                   seed,
                                   study = NULL) {
  if (num_ctrl < 1 || !check_is_numeric(num_ctrl) ||
        (check_is_numeric(num_ctrl) && num_ctrl != as.integer(num_ctrl))) {
    cli::cli_abort("`num_ctrl` must be a positive integer.")
  }

  #Check if SampleID column is present in manifest
  if (!"SampleID" %in% colnames(Manifest)) {
    cli::cli_abort(paste0("SampleID not found! ",
                          "Be sure the column of samples ID's is named",
                          "'SampleID'"))
  }

  if (!missing(Product)) {
    PlateSize <- product_to_platesize(product = Product) # nolint: object_name_linter
  }

  if (is.null(study) && ("study" %in% names(Manifest))) {
    study <- "study"
    cli::cli_alert_info(paste0("`study` column detected in manifest. ",
                               "Optional study argument is set to \"study\"."))
  }

  # Check if there are any duplicated Sample IDs in manifest
  if (any(which(duplicated(Manifest$SampleID)))) {
    duplications <- Manifest$SampleID[which(duplicated(Manifest$SampleID))]
    cli::cli_warn(paste("Following SampleID(s) was/were duplicated:",
                        paste(duplications, collapse = "\n"),
                        sep = "\n"))
  }

  # Check if there are any NAs in SampleID column
  if (any(is.na(Manifest$SampleID))) {
    cli::cli_abort(paste0("No NA allowed in the SampleID column. ",
                          "Check that all the samples are named."))
  }

  # Check plate size is acceptable
  if (!PlateSize %in% c(48, 96)) {
    cli::cli_abort("Plate size needs to be either 48 or 96.")
  }

  # do we use this if available.spots is present?
  spots_per_plate <- PlateSize - num_ctrl * !rand_ctrl
  # subtract number of controls if not randomizing controls

  # cols = platesize/8 (b/c 8 rows on each plate)
  ncols_per_plate <- PlateSize / 8

  #if there is a seed, set the seed
  if (!missing(seed)) {
    set.seed(seed)
  }

  # Check that the subject column provided is present
  if (!missing(SubjectColumn)) {
    if (!any(colnames(Manifest) == SubjectColumn)) {
      cli::cli_abort(paste0("The user assigned SubjectColumn name was not ",
                            "found! Make sure the SubjectColumn is present in ",
                            "the dataset."))
    }
    Manifest$SubjectID <- Manifest[[SubjectColumn]] # nolint: object_name_linter
  }

  # Check that the subjectID column does not have any NAs
  if (any(is.na(Manifest$SubjectID))) {
    cli::cli_abort(paste0("No NA allowed in the SubjectID column. ",
                          "Check that all the subjects are named."))
  }

  # Assuming all plates have same plate size
  if (missing(available.spots)) {
    # plates needed calculated based on # of samples/# of spots per plate
    plates_needed <- ceiling(nrow(Manifest) / spots_per_plate)

    # Add an additional plate if needed when randomizing the controls
    # (if ctrls cause plate to be overfilled)
    if (rand_ctrl) {
      plates_needed <- ifelse(nrow(Manifest) %% spots_per_plate <
                                plates_needed * num_ctrl,
                              plates_needed +
                                ceiling(plates_needed * num_ctrl /
                                          spots_per_plate),
                              plates_needed)

    }

    # Create a plate map with rows and columns based on number of spots
    # available on each plate, either using max that will fit on the
    # plate - controls, or specified number per plate based on available spots
    all.plates <- generate_plate_holder(plates_needed,
                                        rep(spots_per_plate,
                                            times = plates_needed),
                                        nsamples = length(Manifest$SampleID) +
                                          (num_ctrl * rand_ctrl *
                                             plates_needed),
                                        plate_size = PlateSize,
                                        num_ctrl = num_ctrl,
                                        rand_ctrl = rand_ctrl)
  } else {
    plates_needed <- length(available.spots)

    all.plates <- generate_plate_holder(length(available.spots),
                                        available.spots,
                                        nsamples = length(Manifest$SampleID) +
                                          (num_ctrl *
                                             rand_ctrl *
                                             plates_needed),
                                        plate_size = PlateSize,
                                        num_ctrl = num_ctrl,
                                        rand_ctrl = rand_ctrl)

  }

  #### Complete randomization if subjectID not given ####
  if (missing(SubjectColumn) && is.null(study)) {
    # randomly order the first X rows (x being number of samples) from plate
    # layout to assign samples to
    all.plates <- all.plates[sample(1:(nrow(Manifest) +
                                         num_ctrl *
                                           rand_ctrl *
                                           plates_needed)), ]

    # bind manifest to randomized plate layout, add wellID, reorder back to
    # standard plate
    # This doesnt work as well if # rows in manifest != # rows of all plates
    # (could get duplicate samples) - which is what we filtered for above


    # Create new column sampleID
    ctrl_locations <- all.plates |>
      dplyr::group_by(.data[["plate"]]) |>
      dplyr::slice(0) |>
      dplyr::mutate(ID = paste0(.data[["plate"]],
                                .data[["column"]],
                                .data[["row"]])) |>
      dplyr::mutate(SampleID = "CONTROL_SAMPLE")

    # When randomizing controls
    if (rand_ctrl) {
      ctrl_locations <- all.plates |>
        dplyr::group_by(.data[["plate"]]) |>
        dplyr::slice_sample(n = num_ctrl * rand_ctrl) |>
        # Select random locations from each plate when randomizing controls
        dplyr::mutate(ID = paste0(.data[["plate"]],
                                  .data[["column"]],
                                  .data[["row"]])) |>
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    }

    # Remove ctrl locations from list of possible locations
    all.plates <- all.plates |>
      dplyr::mutate(ID = paste0(.data[["plate"]],
                                .data[["column"]],
                                .data[["row"]])) |>
      dplyr::filter(!(.data[["ID"]] %in% ctrl_locations$ID))


    out_manifest <- dplyr::as_tibble(cbind(Manifest,
                                           all.plates)) |>
      dplyr::bind_rows(ctrl_locations) |>
      dplyr::mutate(well = paste0(.data[["row"]],
                                  gsub("Column ",
                                       "",
                                       as.character(.data[["column"]])))) |>
      dplyr::mutate(well = factor(.data[["well"]],
                                  levels = paste0(rep(LETTERS[1:8],
                                                      each = ncols_per_plate),
                                                  rep(1:ncols_per_plate,
                                                      times = 8)))) |>
      # This could use the row column in all.plates instead of regenerating it.
      dplyr::arrange(.data[["plate"]],
                     .data[["column"]],
                     .data[["row"]]) |>
      dplyr::select(-dplyr::any_of("ID"))
    cli::cli_alert_info("Random assignment of SAMPLES to plates\n")
    class(out_manifest) <- c("randomizedManifest", class(out_manifest))
    return(out_manifest)
    # Complete randomization does not require iteration
  }


  #### Keep subjects together ####
  if (!missing(SubjectColumn) && is.null(study)) {
    cli::cli_progress_message("Assigning subjects to plates...")
    for (i in 1:iterations) {

      # Create new column sampleID
      all.plates$SampleID <- NA_character_
      ctrl_locations <- all.plates |>
        dplyr::slice(0) |>
        dplyr::mutate(ID = NA_character_)
      # When randomizing controls
      if (rand_ctrl) {
        ctrl_locations <- all.plates |>
          dplyr::group_by(.data[["plate"]]) |>
          dplyr::slice_sample(n = num_ctrl * rand_ctrl) |>
          # Select random locations from each plate when randomizing controls
          dplyr::mutate(ID = paste0(.data[["plate"]],
                                    .data[["column"]],
                                    .data[["row"]])) |>
          dplyr::mutate(SampleID = "CONTROL_SAMPLE")
      }


      # Remove ctrl locations from list of possible locations when
      # randomizing controls
      all.plates <- all.plates |>
        dplyr::mutate(ID = paste0(.data[["plate"]],
                                  .data[["column"]],
                                  .data[["row"]])) |>
        dplyr::filter(!(.data[["ID"]] %in% ctrl_locations$ID))

      # randomize subject order
      rand_subjects <- sample(unique(Manifest$SubjectID))

      # for each subject in the randomized list of subjects
      for (sub in rand_subjects) {

        all.plates.tmp <- assign_subject2plate(plate_map = all.plates,
                                               manifest = Manifest,
                                               subject_id = sub)
        # Check if assign subject 2 plate returned a df or a warning string
        # (passed or didn't)
        # This could be written such that it didnt include a break statement
        if (check_is_tibble(all.plates.tmp)) {
          # reassign plate_map to now include assigned sample
          all.plates <- all.plates.tmp
        } else if (check_is_character(all.plates.tmp)) {
          passed <- FALSE
          break
        }
        passed <- TRUE
      }

      # Now we have determined which samples fit on which plates, and we
      # scramble the rows and columns within that plate
      if (passed) {
        out_manifest <- Manifest |>
          dplyr::left_join(all.plates, "SampleID") |>
          # merge plate assignment of each sample
          dplyr::group_by(.data[["plate"]]) |> # group by plates
          dplyr::mutate(scramble = sample(seq_len(dplyr::n()))) |>
          # assign random number per sample in each plate
          dplyr::mutate(row = .data[["row"]][.data[["scramble"]]],
                        column = .data[["column"]][.data[["scramble"]]]) |>
          # Move the sample to the row of the scramble number,
          # now each row and column has been moved to the location of
          # its scramble number
          dplyr::ungroup() |>
          dplyr::select(-dplyr::any_of("scramble"))
        break
      }

    }

    cli::cli_alert_info("Random assignment of SUBJECTS to plates\n")
    if (passed) {
      # Arrange updated manifest in order
      out_manifest <- out_manifest |>
        dplyr::bind_rows(ctrl_locations) |>
        dplyr::mutate(well = paste0(.data[["row"]],
                                    gsub("Column ",
                                         "",
                                         as.character(.data[["column"]])))) |>
        dplyr::mutate(well = factor(.data[["well"]],
                                    levels = paste0(rep(LETTERS[1:8],
                                                        each =
                                                          ncols_per_plate),
                                                    rep(1:ncols_per_plate,
                                                        times = 8)))) |>
        dplyr::arrange(.data[["plate"]],
                       .data[["column"]],
                       .data[["row"]]) |>
        dplyr::select(-dplyr::any_of("ID"))

      class(out_manifest) <- c("randomizedManifest", class(out_manifest))
      return(out_manifest)
    } else {
      cli::cli_abort(paste0("Could not keep all subjects on the same plate! ",
                            "Try increasing the number of iterations."))
    }

  }

  #### Keep subjects together and keep studies together ####
  if (!missing(SubjectColumn) && !is.null(study)) {
    cli::cli_alert_info(paste0("Assigning subjects to plates. ",
                               "Keeping studies together during randomization.",
                               " \n"))
    # When randomizing controls
    all.plates$SampleID <- NA_character_
    ctrl_locations <- all.plates |>
      dplyr::slice(0) |>
      dplyr::mutate(ID = NA_character_)
    # When randomizing controls
    if (rand_ctrl) {
      # Select random locations from each plate when randomizing controls
      ctrl_locations <- all.plates |>
        dplyr::group_by(.data[["plate"]]) |>
        dplyr::slice_sample(n = num_ctrl * rand_ctrl) |>
        dplyr::mutate(ID = paste0(.data[["plate"]],
                                  .data[["column"]],
                                  .data[["row"]])) |>
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    }


    # Remove ctrl locations from list of possible locations
    # when randomizing controls
    all.plates <- all.plates |>
      dplyr::mutate(ID = paste0(.data[["plate"]],
                                .data[["column"]],
                                .data[["row"]])) |>
      dplyr::filter(!(.data[["ID"]] %in% ctrl_locations$ID))


    out_manifest <- NULL

    #Randomize on SubjectID_study in case SubjectID is duplicated over studies
    Manifest <- Manifest |> # nolint: object_name_linter
      dplyr::mutate(SubjectID_old = .data[["SubjectID"]],
                    SubjectID = paste0(.data[["SubjectID"]],
                                       "_",
                                       study))

    Manifest <- Manifest |> dplyr::arrange(study) # nolint: object_name_linter
    j_tot <- 0

    #Keep every study together
    for (studyNo in unique(Manifest[[study]])) {
      passed <- FALSE
      rand_subjects <- sample({
        Manifest |>
          dplyr::filter(.data[[study]] == studyNo) |>
          dplyr::select(dplyr::any_of("SubjectID")) |>
          dplyr::distinct() |>
          dplyr::pull()
      })
      study_interval <- which(Manifest[[study]] == studyNo)
      sub_groups <- Manifest |>
        dplyr::filter(study == studyNo) |>
        dplyr::select(dplyr::any_of("SubjectID")) |>
        table()
      sub_groups_max <- as.numeric(max(sub_groups))

      #Append j for every well left empty
      for (j in 0:sub_groups_max) {

        #Extend number of positions available on the last plate
        if (j > 0) {
          study_total_j <- study_interval + j_tot
          extended_study_interval <- unique(c(study_total_j,
                                              (max(study_total_j):
                                                 (max(study_interval) +
                                                    j_tot +
                                                    j))))
          #Extend number of plates if needed
          if (missing(available.spots)) {
            nsamples <- length(Manifest$SampleID) +
              (num_ctrl *
                 rand_ctrl *
                 plates_needed) +
              j_tot +
              j
            plates_needed <- ceiling(nsamples / spots_per_plate)
            all.plates.New <- generate_plate_holder(plates_needed,
                                                    rep(spots_per_plate,
                                                        times = plates_needed),
                                                    nsamples = nsamples,
                                                    num_ctrl = num_ctrl,
                                                    rand_ctrl = rand_ctrl,
                                                    plate_size = PlateSize)
          } else {
            nsamples <- length(Manifest$SampleID) + j_tot + j
            all.plates.New <- generate_plate_holder(length(available.spots),
                                                    available.spots,
                                                    nsamples = nsamples,
                                                    num_ctrl = num_ctrl,
                                                    rand_ctrl = rand_ctrl,
                                                    plate_size = PlateSize)
          }
          all.plates.New$SampleID <- rep(NA, nrow(all.plates.New))
          all.plates <- all.plates.New
        } else {
          study_total_j <- study_interval + j_tot
          extended_study_interval <- unique(c(study_total_j,
                                              (max(study_total_j):
                                                 (max(study_interval) +
                                                    j_tot +
                                                    j))))
        }
        cli::cli_progress_message(paste0("Testing with ",
                                         j,
                                         " empty well(s) in the plate..."))
        manifest_study <- Manifest[study_interval, ]
        for (i in 1:iterations) {
          for (sub in rand_subjects) {
            all.plates.tmp <- assign_subject2plate(plate_map =
                                                     all.plates[
                                                       extended_study_interval,
                                                     ],
                                                   manifest = Manifest,
                                                   subject_id = sub)
            if (check_is_tibble(all.plates.tmp)) {
              all.plates[extended_study_interval, ] <- all.plates.tmp
            } else if (check_is_character(all.plates.tmp)) {
              passed <- FALSE
              break
            }
            passed <- TRUE
          }
          if (passed) {
            out_manifest_study <- manifest_study |>
              tidyr::drop_na() |>
              dplyr::left_join(all.plates, "SampleID") |>
              dplyr::arrange(.data[["plate"]]) |>
              dplyr::group_by(.data[["plate"]]) |>
              dplyr::mutate(scramble = sample(seq_len(dplyr::n()))) |>
              dplyr::mutate(row = .data[["row"]][.data[["scramble"]]],
                            column = .data[["column"]][.data[["scramble"]]]) |>
              dplyr::arrange(.data[["plate"]]) |>
              dplyr::ungroup() |>
              dplyr::select(-dplyr::any_of("scramble")) |>
              dplyr::arrange(.data[["plate"]],
                             .data[["column"]],
                             .data[["row"]])

            cli::cli_alert_success(paste(studyNo, "successful! \n"))
            out_manifest <- dplyr::bind_rows(out_manifest,
                                             out_manifest_study)
            manifest_study2 <- manifest_study |>
              dplyr::left_join(all.plates, "SampleID") |>
              dplyr::group_by(.data[["plate"]]) |>
              dplyr::mutate(scramble = sample(seq_len(dplyr::n())))
            manifest_study2 <- manifest_study2 |>
              dplyr::mutate(row = .data[["row"]][.data[["scramble"]]],
                            column = .data[["column"]][.data[["scramble"]]])
          }
          if (passed) {
            j_tot <- j_tot + j
            break
          }
        }
        if (passed) break
      }
    }
    cli::cli_alert_info("Random assignment of SUBJECTS to plates\n")
    if (passed) {
      cli::cli_alert_info(paste("Included total of",
                                j_tot,
                                "empty well(s) in first and/or",
                                "intermediate plate(s) to accomplish",
                                "the randomization.\n"))
      cli::cli_alert_warning(paste("Please try another seed or increase the",
                                   "number of iterations if there are",
                                   "indications that",
                                   "another randomization might leave fewer",
                                   "empty wells.\n"))
      out_manifest <- out_manifest |>
        dplyr::bind_rows(ctrl_locations) |>
        dplyr::mutate(well = paste0(.data[["row"]],
                                    gsub("Column ",
                                         "",
                                         as.character(.data[["column"]])))) |>
        dplyr::mutate(well = factor(.data[["well"]],
                                    levels = paste0(rep(LETTERS[1:8],
                                                        each = ncols_per_plate),
                                                    rep(1:ncols_per_plate,
                                                        times = 8))),
                      SubjectID = .data[["SubjectID_old"]]) |>
        dplyr::select(-dplyr::any_of(c("SubjectID_old", "ID"))) |>
        dplyr::arrange(.data[["plate"]],
                       .data[["column"]],
                       .data[["row"]])
      class(out_manifest) <- c("randomizedManifest", class(out_manifest))
      return(out_manifest)
    } else {
      cli::cli_abort(paste0("Could not keep all subjects on the same plate! ",
                            "Try increasing the number of iterations."))
    }
  }

  #### Complete randomization within studies when subjectID is not given ####
  if (missing(SubjectColumn) && !is.null(study)) {
    cli::cli_alert_info(paste0("Assigning subjects to plates. ",
                               "Multi-study project detected. ",
                               "Studies will be kept together during ",
                               "randomization. \n"))

    out_manifest <- matrix(nrow = 0, ncol = ncol(Manifest))

    all.plates$SampleID <- NA_character_
    ctrl_locations <- all.plates |>
      dplyr::slice(0) |>
      dplyr::mutate(ID = NA_character_)
    # When randomizing controls
    if (rand_ctrl) {
      ctrl_locations <- all.plates |>
        dplyr::group_by(.data[["plate"]]) |>
        dplyr::slice_sample(n = num_ctrl * rand_ctrl) |>
        # Select random locations from each plate when randomizing controls
        dplyr::mutate(ID = paste0(.data[["plate"]],
                                  .data[["column"]],
                                  .data[["row"]])) |>
        dplyr::mutate(SampleID = "CONTROL_SAMPLE")
    }


    # Remove ctrl locations from list of possible locations when randomizing
    # controls
    all.plates <- all.plates |>
      dplyr::mutate(ID = paste0(.data[["plate"]],
                                .data[["column"]],
                                .data[["row"]])) |>
      dplyr::filter(!(.data[["ID"]] %in% ctrl_locations$ID))

    Manifest <- Manifest |> dplyr::arrange(study) # nolint: object_name_linter
    for (studyNo in unique(Manifest[[study]])) {
      study_interval <- which(Manifest[[study]] == studyNo)

      manifest_study <- Manifest[study_interval, ]
      all.plates_study <- all.plates[study_interval, ]

      man_rows <- nrow(manifest_study)

      all.plates_study <- all.plates_study[sample(seq_len(man_rows)), ] |>
        dplyr::select(-dplyr::any_of("SampleID"))

      out_manifest_study <- dplyr::as_tibble(cbind(manifest_study,
                                                   all.plates_study)) |>
        dplyr::bind_rows(ctrl_locations) |>
        dplyr::mutate(well = paste0(.data[["row"]],
                                    gsub("Column ",
                                         "",
                                         as.character(.data[["column"]])))) |>
        dplyr::mutate(well = factor(.data[["well"]],
                                    levels = paste0(rep(LETTERS[1:8],
                                                        each = ncols_per_plate),
                                                    rep(1:ncols_per_plate,
                                                        times = 8)))) |>
        dplyr::arrange(.data[["plate"]],
                       .data[["column"]],
                       .data[["row"]]) |>
        dplyr::select(-dplyr::any_of("ID"))
      out_manifest <- rbind(out_manifest, out_manifest_study)

    }

    cli::cli_alert_info("Random assignment of SAMPLES to plates by study\n")
    class(out_manifest) <- c("randomizedManifest", class(out_manifest))
    return(out_manifest)
  }
}
