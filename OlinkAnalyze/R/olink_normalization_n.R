#' Bridge and/or subset normalization of all proteins among multiple NPX
#' projects
#'
#' This function normalizes pairs of NPX projects (data frames) using shared
#' samples or subsets of samples.\cr\cr
#'
#' This function is a wrapper of olink_normalization_bridge and
#' olink_normalization_subset.\cr\cr
#'
#' The input of this function is a tibble that contains all the necessary
#' information to normalize multiple NPX projects. This tibble is called the
#' normalization schema. The basic idea is that every row of the data frame is
#' a separate project to be normalized. We assume that there is always one
#' baseline project that does not normalize to any other. All other project
#' normalize to one or more projects. The function handles projects that are
#' normalized in a chain, for example:
#' \itemize{
#'    \item{1.} project 2 normalizes to project 1, and project 3 normalizes to
#'    project 2.
#'    \item{2.} project 2 normalizes to project 1, and project 3 normalizes to
#'    the combined data frame of projects 1 and 2 (that is already normalized).
#' }
#'
#' The function can also handle a mixed schema of bridge and subset
#' normalization.
#'
#' Specifications of the normalization schema data frame:
#' \itemize{
#'    \item{order:} should strictly be a numeric or integer array with unique
#'    identifiers for each project. It is necessary that this array starts from
#'    1 and that it contains no NAs.
#'    \item{name:} should strictly be a character array with unique identifiers
#'    for each project. Each entry should represent the name of the project
#'    located in the same row. No NAs are allowed.
#'    \item{data:} a named list of NPX data frames representing the projects to
#'    be normalized. Names of the items of the list should be identical to
#'    "names". No NAs are allowed.
#'    \item{samples:} a two-level nested named list of sample identifiers from
#'    each NPX project from "data". Names of the first level of the nested list
#'    should be identical to "names" and to the names of the list from "data".
#'    Projects that will be used only as reference should have their
#'    corresponding element in the list as NA, while all other projects should
#'    contain a named list of 2 arrays containing identifiers of samples to be
#'    used for the calculation of adjustment factor. The names of the two
#'    arrays should be DF1 and DF2 corresponding to the reference project and
#'    the project in the current row, respectively. For bridge normalization
#'    arrays should be of equal length and the index of each entry should
#'    correspond to the same sample. For subset normalization arrays do not
#'    need to be of equal length and the order the samples appear in does not
#'    matter. DF1 might contain sample identifiers from more than one project
#'    as long as the project in the current row is to be normalized to multiple
#'    other projects.
#'    \item{normalization_type:} a character array containing the flags "Bridge"
#'    or "Subset". Projects that will be used only as reference should have
#'    their corresponding element in the array as NA, while all other projects
#'    should contain a flag. For the time being the flag "Median" is not
#'    supported.
#'    \item{normalize_to:} a character array pointing to the project this
#'    project is to be normalized to. Elements of the array should be
#'    exclusively from the "order" column. Elements of the array may be
#'    comma-separated if the project is to be normalized to multiple projects.
#' }
#'
#' @param norm_schema A tibble with more than 1 rows and (strictly) the
#' following columns: "order", "name", "data", "samples", "normalization_type",
#' "normalize_to". See "Details" for the structure of the data frame
#' (required)
#'
#' @return A "tibble" of NPX data in long format containing normalized NPX
#' values, including adjustment factors and name of project.
#'
#' @export
#'
#' @keywords Normalization; Bridge normalization; Subset normalization;
#'
#' @examples
#' \donttest{
#' #### Bridge normalization of two projects
#'
#' # prepare datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find overlapping samples, but exclude Olink control
#' overlap_samples <- dplyr::intersect(unique(npx_df1$SampleID),
#'                                     unique(npx_df2$SampleID))
#' overlap_samples_list <- list("DF1" = overlap_samples,
#'                              "DF2" = overlap_samples)
#'
#' # create tibble for input
#' norm_schema_bridge <- dplyr::tibble(
#'   order              = c(1, 2),
#'   name               = c("NPX_DF1", "NPX_DF2"),
#'   data               = list("NPX_DF1" = npx_df1,
#'                             "NPX_DF2" = npx_df2),
#'   samples            = list("NPX_DF1" = NA_character_,
#'                             "NPX_DF2" = overlap_samples_list),
#'   normalization_type = c(NA_character_, "Bridge"),
#'   normalize_to       = c(NA_character_, "1")
#' )
#'
#' # normalize
#' olink_normalization_n(norm_schema = norm_schema_bridge)
#'
#' #### Subset normalization of two projects
#'
#' # datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find a suitable subset of samples from both projects, but exclude Olink
#' # controls and samples that fail QC.
#' df1_samples <- npx_df1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique() |>
#'   sample(size = 16, replace = FALSE)
#' df2_samples <- npx_df2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique() |>
#'   sample(size = 16, replace = FALSE)
#'
#' # create named list
#' subset_samples_list <- list("DF1" = df1_samples,
#'                             "DF2" = df2_samples)
#'
#' # create tibble for input
#' norm_schema_subset <- dplyr::tibble(
#'   order              = c(1, 2),
#'   name               = c("NPX_DF1", "NPX_DF2"),
#'   data               = list("NPX_DF1" = npx_df1,
#'                             "NPX_DF2" = npx_df2),
#'   samples            = list("NPX_DF1" = NA_character_,
#'                             "NPX_DF2" = subset_samples_list),
#'   normalization_type = c(NA_character_, "Subset"),
#'   normalize_to       = c(NA_character_, "1")
#' )
#'
#' # Normalize
#' olink_normalization_n(norm_schema = norm_schema_subset)
#'
#' #### Subset normalization  of two projects using all samples
#'
#' # datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find a suitable subset of samples from both projects, but exclude Olink
#' # controls and samples that fail QC.
#' df1_samples_all <- npx_df1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique()
#' df2_samples_all <- npx_df2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique()
#'
#' # create named list
#' subset_samples_all_list <- list("DF1" = df1_samples_all,
#'                                 "DF2" = df2_samples_all)
#'
#' # create tibble for input
#' norm_schema_subset_all <- dplyr::tibble(
#'   order              = c(1, 2),
#'   name               = c("NPX_DF1", "NPX_DF2"),
#'   data               = list("NPX_DF1" = npx_df1,
#'                             "NPX_DF2" = npx_df2),
#'   samples            = list("NPX_DF1" = NA_character_,
#'                             "NPX_DF2" = subset_samples_all_list),
#'  normalization_type = c(NA_character_, "Subset"),
#'  normalize_to       = c(NA_character_, "1")
#' )
#'
#' # Normalize
#' olink_normalization_n(norm_schema = norm_schema_subset_all)
#'
#' #### Multi-project normalization using bridge and subset samples
#'
#' ## NPX data frames to bridge
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # manipulating the sample NPX datasets to create another two random ones
#' npx_df3 <- npx_data2 |>
#'   dplyr::mutate(SampleID = paste(SampleID, "_mod", sep = ""),
#'                 PlateID = paste(PlateID, "_mod", sep = ""),
#'                 NPX = sample(x = NPX, size = dplyr::n(), replace = FALSE)) |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' npx_df4 <- npx_data1 |>
#'   dplyr::mutate(SampleID = paste(SampleID, "_mod2", sep = ""),
#'                 PlateID = paste(PlateID, "_mod2", sep = ""),
#'                 NPX = sample(x = NPX, size = dplyr::n(), replace = FALSE)) |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' ## samples to use for normalization
#' # Bridge samples with same identifiers between npx_df1 and npx_df2
#' overlap_samples <- dplyr::intersect(unique(npx_df1$SampleID),
#'                                     unique(npx_df2$SampleID))
#' overlap_samples_df1_df2 <- list("DF1" = overlap_samples,
#'                                 "DF2" = overlap_samples)
#' rm(overlap_samples)
#'
#' # Bridge samples with different identifiers between npx_df2 and npx_df3
#' overlap_samples_df2_df3 <- list("DF1" = sample(x = unique(npx_df2$SampleID),
#'                                                size = 10,
#'                                                replace = FALSE),
#'                                 "DF2" = sample(x = unique(npx_df3$SampleID),
#'                                                size = 10,
#'                                                replace = FALSE))
#'
#' # Samples to use for intensity normalization between npx_df4 and the
#' # normalized dataset of npx_df1 and npx_df2
#' overlap_samples_df12_df4 <- list("DF1" = sample(x = c(unique(npx_df1$SampleID),
#'                                                       unique(npx_df2$SampleID)),
#'                                                 size = 100,
#'                                                 replace = FALSE) |>
#'                                    unique(),
#'                                  "DF2" = sample(x = unique(npx_df4$SampleID),
#'                                                 size = 40,
#'                                                 replace = FALSE))
#'
#' # create tibble for input
#' norm_schema_n <- dplyr::tibble(
#'   order              = c(1, 2, 3, 4),
#'   name               = c("NPX_DF1", "NPX_DF2", "NPX_DF3", "NPX_DF4"),
#'   data               = list("NPX_DF1" = npx_df1,
#'                             "NPX_DF2" = npx_df2,
#'                             "NPX_DF3" = npx_df3,
#'                             "NPX_DF4" = npx_df4),
#'   samples            = list("NPX_DF1" = NA_character_,
#'                             "NPX_DF2" = overlap_samples_df1_df2,
#'                             "NPX_DF3" = overlap_samples_df2_df3,
#'                             "NPX_DF4" = overlap_samples_df12_df4),
#'   normalization_type = c(NA_character_, "Bridge", "Bridge", "Subset"),
#'   normalize_to       = c(NA_character_, "1", "2", "1,2")
#' )
#'
#' olink_normalization_n(norm_schema = norm_schema_n)
#'
#' }
#'
#' @importFrom dplyr filter pull mutate bind_rows select
#'
olink_normalization_n <- function(norm_schema) {
  # Upcoming updates/features:
  #   automatic detection of bridge samples
  #   automatic detection of samples for subset normalization
  #   check for control samples and warn the user
  #   Check that rows have all the information they need to work
  #   check that names, names of data and names of samples are identical

  # check that norm_schema has all the infor we need to proceed
  normalize_n_check <- olink_normalization_n_check(norm_schema = norm_schema)

  if (normalize_n_check != "TRUE") {
    stop(normalize_n_check)
  }
  rm(normalize_n_check)

  # create an empty list to store results
  normalized_npx <- list()

  # extract data for global reference project
  project_ref_global <- norm_schema |>
    dplyr::filter(order == 1L) |>
    dplyr::pull(name)

  normalized_npx <- norm_schema |>
    dplyr::filter(order == 1L) |>
    dplyr::pull(data)
  names(normalized_npx) <- project_ref_global

  normalized_npx[[project_ref_global]] <- normalized_npx[[project_ref_global]] |>
    dplyr::mutate(Project = project_ref_global,
                  Adj_factor = 0)

  # extract remaining batches
  project_remaining <- norm_schema |>
    dplyr::filter(order != 1L) |>
    dplyr::pull(order)

  # replace original dataset with its name to save memory
  # this way we do not keep two copies of the same dataset
  # the copy we need is under the normalized_npx list
  norm_schema$data[[project_ref_global]] <- project_ref_global

  # for each remaining batch run one normalization
  for (project_i in project_remaining) {

    # extract the next project to be normalized
    project_i_row <- norm_schema |>
      dplyr::filter(order == project_i)

    # non-reference npx df
    project_i_npx <- norm_schema |>
      dplyr::filter(order == project_i) |>
      dplyr::pull(data) |>
      unname() |>
      dplyr::bind_rows()

    # extract name and type of normalization to apply
    project_i_name <- project_i_row$name
    project_i_normalization_type <- project_i_row$normalization_type

    # extract names of projects to normalize to
    project_i_norm_to <- strsplit(x = project_i_row$normalize_to,
                                  split = ",",
                                  fixed = TRUE) |>
      unlist()

    project_ref_names <- norm_schema |>
      dplyr::filter(order %in% project_i_norm_to) |>
      dplyr::pull(name)

    project_ref_names_combo <- paste(project_ref_names,
                                     collapse = ",")

    # reference NPX dataset
    if (length(project_i_norm_to) > 1L) {
      project_ref_npx <- normalized_npx[project_ref_names] |>
        dplyr::bind_rows()
    } else {
      project_ref_npx <- normalized_npx[[project_ref_names]]
    }
    # drop these two columns as they will be reassigned from
    # the normalization function
    project_ref_npx <- project_ref_npx |>
      dplyr::select(-c(Project, Adj_factor))

    samples_norm <- project_i_row$samples |>
      unname() |>
      unlist(recursive = FALSE)

    # normalize
    if (project_i_normalization_type == "Subset") {
      project_i_npx_norm <- olink_normalization_subset(
        project_1_df = project_ref_npx,
        project_2_df = project_i_npx,
        reference_samples = samples_norm,
        project_1_name = project_ref_names_combo,
        project_2_name = project_i_name,
        project_ref_name = project_ref_names_combo
      )
    } else if (project_i_normalization_type == "Bridge") {
      project_i_npx_norm <- olink_normalization_bridge(
        project_1_df = project_ref_npx,
        project_2_df = project_i_npx,
        bridge_samples = samples_norm,
        project_1_name = project_ref_names_combo,
        project_2_name = project_i_name,
        project_ref_name = project_ref_names_combo
      )
    }

    # add to main list of normalized npx datasets
    normalized_npx[[project_i_name]] <- project_i_npx_norm |>
      dplyr::filter(Project != project_ref_names_combo)

    # replace original dataset with its name to save memory
    # this way we do not keep two copies of the same dataset
    # the copy we need is under the normalized_npx list
    norm_schema$data[[project_i_name]] <- project_i_name

    # cleanup to make sure that we reset variables and receive relevant errors
    rm(project_i_row, project_i_npx, project_i_name,
       project_i_normalization_type, project_i_norm_to, project_ref_names,
       project_ref_names_combo, project_ref_npx, samples_norm,
       project_i_npx_norm)
  }

  normalized_npx <- dplyr::bind_rows(normalized_npx)

  return(normalized_npx)
}

#' Bridge normalization of all proteins between two NPX projects.
#'
#' Normalizes two NPX projects (data frames) using shared samples.\cr\cr
#'
#' This function is a wrapper of olink_normalization.\cr\cr
#'
#' In bridging normalization one of the projects is adjusted to another using
#' shared samples (bridge samples). It is not necessary for the shared
#' samples to be named the same in each project. Adjustment between the two
#' projects is made using the median of the paired differences between the
#' shared samples. The two data frames are inputs project_1_df and project_2_df,
#' the one being adjusted to is specified in the input project_ref_name and the
#' shared samples are specified in bridge_samples.\cr\cr
#'
#' @param project_1_df Data frame of the first project (required).
#' @param project_2_df Data frame of the second project (required).
#' @param bridge_samples Named list of 2 arrays containing SampleID of shared
#' samples to be used for the calculation of adjustment factor. The
#' names of the two arrays should be DF1 and DF2 corresponding to projects 1
#' and 2, respectively. Arrays should be of equal length and index of each entry
#' should correspond to the same sample. (required)
#' @param project_1_name Name of the first project (default: P1).
#' @param project_2_name Name of the second project (default: P2).
#' @param project_ref_name Name of the project to be used as reference set.
#' Needs to be one of the project_1_name or project_2_name. It marks the
#' project to which the other project will be adjusted to (default: P1).
#'
#' @return A "tibble" of NPX data in long format containing normalized NPX
#' values, including adjustment factors and name of project.
#'
#' @export
#'
#' @keywords Normalization; Bridge normalization
#'
#' @examples
#' \donttest{
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find overlapping samples, but exclude Olink control
#' overlap_samples <- dplyr::intersect(unique(npx_df1$SampleID),
#'                                     unique(npx_df2$SampleID))
#' overlap_samples_list <- list("DF1" = overlap_samples,
#'                              "DF2" = overlap_samples)
#'
#' # Normalize
#' olink_normalization_bridge(project_1_df = npx_df1,
#'                            project_2_df = npx_df2,
#'                            bridge_samples = overlap_samples_list,
#'                            project_1_name = "P1",
#'                            project_2_name = "P2",
#'                            project_ref_name = "P1")
#' }
#'
#' @importFrom dplyr bind_cols rename left_join mutate select case_when if_else
#'
olink_normalization_bridge <- function(project_1_df,
                                       project_2_df,
                                       bridge_samples,
                                       project_1_name = "P1",
                                       project_2_name = "P2",
                                       project_ref_name = "P1") {

  check_project_name <- olink_normalization_project_name_check(
    project_1_name = project_1_name,
    project_2_name = project_2_name,
    project_ref_name = project_ref_name)
  if (check_project_name != "TRUE") {
    stop(check_project_name)
  }
  rm(check_project_name)

  check_bridge_samples <- olink_normalization_sample_check(
    list_samples = bridge_samples,
    check_mode = "bridge",
    project_1_all_samples = { project_1_df$SampleID |> unique() },
    project_2_all_samples = { project_2_df$SampleID |> unique() })
  if (check_bridge_samples != "TRUE") {
    stop(check_bridge_samples)
  }
  rm(check_bridge_samples)

  # place bridge samples side by side in a data frame
  update_sampleid <- dplyr::bind_cols(bridge_samples) |>
    dplyr::rename("SampleID_df1" = "DF1",
                  "SampleID_df2" = "DF2")

  # change the SampleID of the non-reference data frame to match the bridging
  # samples from the reference data frame. This is done because the
  # OlinkAnalyze::olink_normalization function requires so.
  project_2_df <- project_2_df |>
    dplyr::left_join(update_sampleid, by = c('SampleID' = 'SampleID_df2')) |>
    dplyr::mutate(SampleID_df1 = dplyr::if_else(is.na(SampleID_df1),
                                                SampleID,
                                                SampleID_df1)) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df1")

  # bridge normalize the two data frames
  norm_df <- olink_normalization(
    df1 = project_1_df,
    df2 = project_2_df,
    overlapping_samples_df1 = bridge_samples$DF1,
    overlapping_samples_df2 = NULL,
    df1_project_nr = project_1_name,
    df2_project_nr = project_2_name,
    reference_project = project_ref_name,
    reference_medians = NULL
  )

  # switch back to the original non-reference project's SampleID
  norm_df <- norm_df |>
    dplyr::left_join(update_sampleid, by = c('SampleID' = 'SampleID_df1')) |>
    dplyr::mutate(SampleID_df2 = dplyr::case_when(
      is.na(SampleID_df2) ~ SampleID,
      !is.na(SampleID_df2) & Project == project_ref_name ~ SampleID,
      !is.na(SampleID_df2) & Project != project_ref_name ~ SampleID_df2,
      TRUE ~ NA_character_)) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df2")
  rm(update_sampleid)

  return(norm_df)
}

#' Subset normalization of all proteins between two NPX projects.
#'
#' Normalizes two NPX projects (data frames) using all or a subset of samples.\cr\cr
#'
#' This function is a wrapper of olink_normalization.\cr\cr
#'
#' In subset normalization one of the projects is adjusted to another using
#' a subset of all samples from each. Please note that the subsets of samples
#' are not expected to be replicates of each other or to have the SampleID.
#' Adjustment between the two projects is made using the assay-specific
#' differences in median between the subsets of samples from the two projects.
#' The two data frames are inputs project_1_df and project_2_df, the one being
#' adjusted to is specified in the input project_ref_name and the shared
#' samples are specified in reference_samples. \cr\cr
#'
#' A special case of subset normalization is to use all samples (except control
#' samples) from each project as a subset. \cr\cr
#'
#' @param project_1_df Data frame of the first project (required).
#' @param project_2_df Data frame of the second project (required).
#' @param reference_samples Named list of 2 arrays containing SampleID of the
#' subset of samples to be used for the calculation of median NPX within each
#' project. The names of the two arrays should be DF1 and DF2 corresponding to
#' projects 1 and 2, respectively. Arrays do not need to be of equal length and
#' the order the samples appear in does not play any role. (required)
#' @param project_1_name Name of the first project (default: P1).
#' @param project_2_name Name of the second project (default: P2).
#' @param project_ref_name Name of the project to be used as reference set.
#' Needs to be one of the project_1_name or project_2_name. It marks the
#' project to which the other project will be adjusted to (default: P1).
#'
#' @return A "tibble" of NPX data in long format containing normalized NPX
#' values, including adjustment factors and name of project.
#'
#' @export
#'
#' @keywords Normalization; Subset normalization
#'
#' @examples
#' \donttest{
#' #### Subset normalization
#'
#' # datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find a suitable subset of samples from both projects, but exclude Olink
#' # controls and samples that fail QC.
#' df1_samples <- npx_df1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique() |>
#'   sample(size = 16, replace = FALSE)
#' df2_samples <- npx_df2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique() |>
#'   sample(size = 16, replace = FALSE)
#'
#' # create named list
#' subset_samples_list <- list("DF1" = df1_samples,
#'                             "DF2" = df2_samples)
#'
#' # Normalize
#' olink_normalization_subset(project_1_df = npx_df1,
#'                            project_2_df = npx_df2,
#'                            reference_samples = subset_samples_list,
#'                            project_1_name = "P1",
#'                            project_2_name = "P2",
#'                            project_ref_name = "P1")
#'
#'
#' #### Special case of subset normalization using all samples
#'
#' # datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#' npx_df2 <- npx_data2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::select(-Project) |>
#'   dplyr::mutate(Normalization = "Intensity")
#'
#' # Find a suitable subset of samples from both projects, but exclude Olink
#' # controls and samples that fail QC.
#' df1_samples_all <- npx_df1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique()
#' df2_samples_all <- npx_df2 |>
#'   dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
#'   dplyr::group_by(SampleID) |>
#'   dplyr::filter(all(QC_Warning == 'Pass')) |>
#'   dplyr::pull(SampleID) |>
#'   unique()
#'
#' # create named list
#' subset_samples_all_list <- list("DF1" = df1_samples_all,
#'                             "DF2" = df2_samples_all)
#'
#' # Normalize
#' olink_normalization_subset(project_1_df = npx_df1,
#'                            project_2_df = npx_df2,
#'                            reference_samples = subset_samples_all_list,
#'                            project_1_name = "P1",
#'                            project_2_name = "P2",
#'                            project_ref_name = "P1")
#' }
#'
olink_normalization_subset <- function(project_1_df,
                                       project_2_df,
                                       reference_samples,
                                       project_1_name = "P1",
                                       project_2_name = "P2",
                                       project_ref_name = "P1") {

  check_project_name <- olink_normalization_project_name_check(
    project_1_name = project_1_name,
    project_2_name = project_2_name,
    project_ref_name = project_ref_name)
  if (check_project_name != "TRUE") {
    stop(check_project_name)
  }
  rm(check_project_name)

  check_reference_samples <- olink_normalization_sample_check(
    list_samples = reference_samples,
    check_mode = "subset",
    project_1_all_samples = { project_1_df$SampleID |> unique() },
    project_2_all_samples = { project_2_df$SampleID |> unique() })
  if (check_reference_samples != "TRUE") {
    stop(check_reference_samples)
  }
  rm(check_reference_samples)

  norm_df <- olink_normalization(
    df1 = project_1_df,
    df2 = project_2_df,
    overlapping_samples_df1 = reference_samples$DF1,
    overlapping_samples_df2 = reference_samples$DF2,
    df1_project_nr = project_1_name,
    df2_project_nr = project_2_name,
    reference_project = project_ref_name,
    reference_medians = NULL
  )

  return(norm_df)
}

# olink_median_reference_normalization <- function(project_1_df,
#                                                  project_ref_samples,
#                                                  project_ref_medians) {
#   OlinkAnalyze::olink_normalization(
#     df1 = project_1_df,
#     df2 = NULL,
#     overlapping_samples_df1 = project_ref_samples,
#     overlapping_samples_df2 = NULL,
#     df1_project_nr = NULL,
#     df2_project_nr = NULL,
#     reference_project = NULL,
#     reference_medians = project_ref_medians
#   )
# }

#' An internal function to perform checks on the input samples in the functions
#' olink_normalization_bridge and olink_normalization_subset. The function is
#' expected to run all checks on SampleID to make sure that normalization can
#' be performed smoothly. It should work independently of the function calling
#' it.
#'
#' @param list_samples Named list of 2 arrays containing SampleID of the
#' subset or bridge samples to be used for normalization. The names of the two
#' arrays should be DF1 and DF2 corresponding to projects 1 and 2, respectively.
#' (required)
#' @param check_mode Flag "bridge" or "subset" indicating the type of
#' normalization the check should be tailored to (required)
#' @param project_1_all_samples Array of all samples from project 1 (required)
#' @param project_2_all_samples Array of all samples from project 2 (required)
#'
#' @return a character message. If the message is "TRUE" then all checks passed,
#' otherwise an error message will be printed.
#'
olink_normalization_sample_check <- function(list_samples,
                                             check_mode,
                                             project_1_all_samples,
                                             project_2_all_samples) {
  if (!(check_mode %in% c("bridge", "subset"))) {
    stop("\"check_mode\" should be \"bridge\" or \"subset\"")
  }

  if (check_mode == "bridge") {
    message_text <- "bridge_samples"
  } else if (check_mode == "subset") {
    message_text <- "reference_samples"
  }

  # check list_samples
  if (!is.list(list_samples)) {
    return(
      paste(message_text,
            "should be a list")
    )
  }

  if (length(list_samples) != 2L) {
    return(
      paste("length of ",
            message_text,
            " should exactly 2",
            sep = "")
    )
  }

  if (!identical({ names(list_samples) |> sort() }, c("DF1", "DF2"))) {
    return(
      paste(message_text,
            " should be a named list with names \"DF2\" and \"DF1\" referring",
            " to the data frame on this row and the one to normalize to,",
            " respectively",
            sep = "")
    )
  }

  if (!{ sapply(list_samples, is.character) |> all() }) {
    return(
      paste(message_text,
            " should contain exclusively character vectors",
            sep = "")
    )
  }

  if ({ list_samples |> unlist() |> is.na() |> any() }) {
    return(
      paste("character vectors in ",
            message_text,
            " should not contain NAs",
            sep = "")
    )
  }

  if (!all(list_samples$DF1 %in% project_1_all_samples)) {
    return(
      paste("not all SampleID in ",
            message_text,
            "$DF1 are present in \"project_1_df\"",
            sep = "")
    )
  }

  if (!all(list_samples$DF2 %in% project_2_all_samples)) {
    return(
      paste("not all SampleID in ",
            message_text,
            "$DF2 are present in \"project_2_df\"",
            sep = "")
    )
  }

  if (check_mode == "bridge") {
    if ({ sapply(list_samples, length) |> unique() |> length() } != 1L) {
      return(
        paste("character vectors in ",
              message_text,
              " should have the same length",
              sep = ""))
    }
  }

  return("TRUE")
}

#' An internal function to perform checks on the input project names in the
#' functions olink_normalization_bridge and olink_normalization_subset. The
#' function is expected to run all checks on project names to make sure that
#' normalization can be performed smoothly. It should work independently of the
#' function calling it.
#'
#' @param project_1_name Name of project 1 (required)
#' @param project_2_name Name of project 2 (required)
#' @param project_ref_name  Name of reference project (required)
#'
#' @return a character message. If the message is "TRUE" then all checks passed,
#' otherwise an error message will be printed.
#'
olink_normalization_project_name_check <- function(project_1_name,
                                                   project_2_name,
                                                   project_ref_name) {

  project_names <- c(project_1_name,
                     project_2_name,
                     project_ref_name)

  # check all project names together
  if (!is.character(project_names)) {
    return(
      paste("\"project_1_name\", \"project_2_name\" and \"project_ref_name\"",
            " should be a character vectors",
            sep = "")
    )
  }

  if (length(project_names) != 3L) {
    return(
      paste("\"project_1_name\", \"project_2_name\" and \"project_ref_name\"",
            " should be a character vectors of length 1 each",
            sep = "")
    )
  }

  if ({ is.na(project_names) |> any() }) {
    return(
      paste("\"project_1_name\", \"project_2_name\" or \"project_ref_name\"",
            " should not be NA",
            sep = "")
    )
  }

  # sanity check for individual project names
  if (project_1_name == project_2_name) {
    return(
      paste("\"project_1_name\" and \"project_2_name\" should differ from each",
            " other",
            sep = "")
    )
  }

  if (!(project_ref_name %in% c(project_1_name, project_2_name))) {
    return(
      paste("\"project_ref_name\" should be one of the \"project_1_name\" or",
            " \"project_2_name\"",
            sep = "")
    )
  }

  return("TRUE")
}

#' An internal function to perform checks on the input of the function
#' olink_normalization_n.
#'
#' @param norm_schema A tibble with more than 1 rows and (strictly) the
#' following columns: "order", "name", "data", "samples", "normalization_type",
#' "normalize_to". See above for details of the structure of the data frame. See
#' details in help for olink_normalization_n. (required)
#'
#' @return a character message. If the message is "TRUE" then all checks passed,
#' otherwise an error message will be printed.
#'
olink_normalization_n_check <- function(norm_schema) {

  # check input
  input_colnames <- c("order",
                      "name",
                      "data",
                      "samples",
                      "normalization_type",
                      "normalize_to")
  if (!all(input_colnames %in% colnames(norm_schema))) {
    miss_cols <- input_colnames[!input_colnames %in% colnames(norm_schema)]

    if (length(miss_cols == 1)) {
      miss_cols_print <- miss_cols
    } else {
      miss_cols_print <- miss_cols |>
        head(-1) |>
        paste(collapse = ", ") |>
        paste("and", { miss_cols |> tail(-1) })
    }

    return(
      paste("norm_schema input is missing columns ",
            miss_cols_print,
            sep = "")
    )
  }

  # Check order
  if (any(is.na(norm_schema$order)) ||
      any(is.infinite(norm_schema$order))) {
    return("order cannot contain NA and/or infinities!")
  }

  if (!is.numeric(norm_schema$order) &&
      !is.integer(norm_schema$order)) {
    return("order has to be numeric or integer vector!")
  }

  if (!identical(
    {
      norm_schema$order |>
        sort() |>
        as.integer()
    },
    1L:nrow(norm_schema))) {
    return(
      paste("order has to be a sequence of unique integer or numeric",
            " identifiers of each row starting from 1. The sequence has to",
            " increase by 1",
            sep = "")
    )
  }

  # Check data
  if (!is.list(norm_schema$data) ||
      !all(sapply(norm_schema$data, is.data.frame))) {
    return("data has to be a list of NPX data frames!")
  }

  if (any(sapply(norm_schema$data, function(x) "Project" %in% colnames(x)))) {
    check_project <- norm_schema$data |>
      sapply(function(x) "Project" %in% colnames(x)) |>
      names()

    if (length(check_project) == 1) {
      check_project_print <- check_project
    } else {
      check_project_print <- check_project |>
        head(-1) |>
        paste(collapse = ", ") |>
        paste("and", { check_project |> tail(-1) })
    }

    return(
      paste("datsets ",
            check_project_print,
            " contain the column \"Project\". Please remove and rerun!",
            sep = "")
    )
  }

  if (any(sapply(norm_schema$data, function(x) "Adj_factor" %in% colnames(x)))) {
    check_project <- norm_schema$data |>
      sapply(function(x) "Adj_factor" %in% colnames(x)) |>
      names()

    if (length(check_project) == 1) {
      check_project_print <- check_project
    } else {
      check_project_print <- check_project |>
        head(-1) |>
        paste(collapse = ", ") |>
        paste("and", { check_project |> tail(-1) })
    }

    return(
      paste("datsets ",
            check_project_print,
            " contain the column \"Adj_factor\". Please remove and rerun!",
            sep = "")
    )
  }

  # Check samples
  if (!is.list(norm_schema$samples)) {
    return("samples has to be a list of character vectors!")
  }

  # Check normalization_type
  if (!is.character(norm_schema$normalization_type)) {
    return("normalization_type has to be a character vector!")
  }

  if (!all(norm_schema$normalization_type %in% c("Subset",
                                                 "Bridge",
                                                 NA))) {
    return("normalization_type may contain Subset, Bridge or NA!")
  }

  # Check normalize_to
  if (!is.character(norm_schema$normalize_to)) {
    return("normalize_to has to be a character array!")
  }

  if (!all({
    strsplit(x = norm_schema$normalize_to,
             split = ",",
             fixed = TRUE) |>
      unlist() |>
      unique() |>
      as.integer() |>
      sort()
  } %in% {
    norm_schema$order |>
      sort() |>
      as.integer()
  })) {
    return("all elements from normalize_to have to be a present in \"order\"!")
  }

  order_and_norm_to <- lapply(1:nrow(norm_schema), function(i)
  {
    order_tmp <- norm_schema |>
      dplyr::slice(i) |>
      dplyr::pull(order)

    norm_to_tmp <- norm_schema |>
      dplyr::slice(i) |>
      dplyr::pull(normalize_to) |>
      strsplit(split = ",",
               fixed = TRUE) |>
      unlist()

    if (order_tmp %in% norm_to_tmp) {
      return(order_tmp)
    } else {
      return(NULL)
    }
  }) |>
    unlist()
  if (!is.null(order_and_norm_to) &&
      length(order_and_norm_to) > 0) {
    if (length(order_and_norm_to) == 1) {
      order_and_norm_to_print <- order_and_norm_to
    } else {
      order_and_norm_to_print <- order_and_norm_to |>
        head(-1) |>
        paste(collapse = ", ") |>
        paste("and", { order_and_norm_to |> tail(-1) })
    }

    return(
      paste("entries with order column identifiers ",
            order_and_norm_to_print,
            " contain their own identifier in the normalize_to column. This",
            " implies normalization to self. Remove self-references!",
            sep = "")
    )
  }

  return("TRUE")
}
