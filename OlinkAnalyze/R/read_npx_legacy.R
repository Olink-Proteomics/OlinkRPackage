read_npx_legacy_help <- function(file,
                                 out_df,
                                 olink_platform = NULL,
                                 data_type = NULL,
                                 data_type_no_accept = c("Ct")) {
  # check input ----

  check_is_character(string = data_type_no_accept,
                     error = TRUE)

  # check that file can be processed ----

  # send the input file to read_npx_format with legacy = TRUE so that it returns
  # information on the file format, the data type and olink platform, as well as
  # the data frame itself. This is done to utilize the tools we have already
  # developed to detect various features of the data and to read it in.

  list_format <- read_npx_format(file = file,
                                 out_df = out_df,
                                 sep = NULL,
                                 long_format = NULL,
                                 olink_platform = olink_platform,
                                 data_type = data_type,
                                 quiet = TRUE,
                                 legacy = TRUE)

  # checks ----

  # legacy wide files are supported only

  # only wide files are accepted
  if (list_format$long_format == TRUE) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only wide format files!",
        "i" = "Detected long format!"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

  # only Target 48 and Target 96 platforms are accepted
  accept_platforms <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["broader_platform"]] == "qPCR"
      & grepl("Target", .data[["name"]])
    )

  if (!(list_format$olink_platform %in% accept_platforms$name)) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only data from
        {.val {accept_platforms$name}}!",
        "i" = "Detected {.val {list_format$olink_platform}}!"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

  # only NPX and Quantified values
  accept_quant <- accepted_olink_platforms |>
    # keep only relevant quantification methods
    dplyr::filter(
      .data[["broader_platform"]] == "qPCR"
      & .data[["name"]] == list_format$olink_platform
    ) |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unlist() |>
    # remove Ct as is in not supported
    (\(.x) .x[!(.x %in% data_type_no_accept)])()
  if (!(list_format$data_type %in% accept_quant)) {

    cli::cli_abort(
      message = c(
        "x" = "{.fn read_npx_legacy} accepts only {.val {accept_quant}} data ran
        on {.val {list_format$olink_platform}}!",
        "i" = "Detected {.val {list_format$data_type}}!"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

  # split data frame into sub-matrices ----

  # format acceptable specifications
  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == list_format$data_type
    )

  # split data frame into header, top, middle and bottom matrices. Here we used
  # the tools we have already developed for read_npx_wide.
  df_split <- read_npx_wide_split_row(
    df = list_format$df,
    file = file,
    data_type = list_format$data_type,
    format_spec = format_spec
  )

  # get NPX software version ----

  # function was developed for read_npx_wide
  npxs_v <- read_npx_wide_npxs_version(
    df = df_split$df_head
  )

  # modify variables ----

  # combine df_mid with df_bottom
  df_na <- rep(NA_character_, ncol(df_split$df_mid)) |>
    t() |>
    dplyr::as_tibble(
      .name_repair = "minimal"
    )
  names(df_na) <- names(df_split$df_mid)

  df_split$df_data <- df_split$df_mid |>
    dplyr::bind_rows(
      df_na
    ) |>
    dplyr::bind_rows(
      df_split$df_bottom
    )

  # remove df_head from the list
  df_split <- df_split[-which(names(df_split) %in% c("df_head",
                                                     "df_mid",
                                                     "df_bottom"))]

  # return ----

  return(
    list(
      olink_platform = list_format$olink_platform,
      long_format = list_format$long_format,
      data_type = list_format$data_type,
      df_split = df_split,
      npxs_v = npxs_v,
      format_spec = format_spec,
      accept_platforms = accept_platforms |>
        dplyr::filter(
          .data[["name"]] == list_format$olink_platform
        )
    )
  )
}

read_npx_legacy <- function(file,
                            out_df = "tibble",
                            olink_platform = NULL,
                            data_type = NULL,
                            quiet = TRUE) {
  # check input ----

  check_is_scalar_boolean(bool = quiet,
                          error = TRUE)

  # the remaining variables are checked in read_npx_legacy_help

  # read_npx_legacy_help ----

  # call read_npx_legacy_help to get information about the input file as well as
  # import the dataset.
  # At the same time, read_npx_legacy_help will also check all the values of the
  # input.
  df_format_list <- read_npx_legacy_help(file = file,
                                         out_df = out_df,
                                         olink_platform = olink_platform,
                                         data_type = data_type,
                                         data_type_no_accept = c("Ct"))

  # Check if the data type is NPX or absolute concentration ----

  is_npx <- ifelse(df_format_list$data_type == "NPX",
                   TRUE,
                   FALSE)

  # Message of data type detected

  if (!quiet) {
    cli::cli_inform(
      message = "{df_format_list$olink_platform} data in wide form detected.",
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # help vars ----

  # in earlier versions of Olink wide files, the row Normalization on the bottom
  # matrix was absent. In more recent versions, it was included. This flag marks
  # its presence or absence.
  norm_flag <-  FALSE
  # number of assays per panel. 45 for T48 and 92 for T96.
  # Newer T48 panels might not work.
  base_index <- ifelse(df_format_list$olink_platform == "Target 96", 92L, 45L)

  # datasets ----

  # metadata: top and bottom matrices
  meta_dat <- df_format_list$df_split$df_top
  names(meta_dat) <- c("Name", 2L:ncol(meta_dat))

  # data: middle matrix
  dat <- df_format_list$df_split$df_data
  names(dat) <- c("Name", 2L:ncol(dat))

  # update datasets ----

  # locations of internal controls
  control_index <- stringr::str_detect(
    string = meta_dat[2L, ],
    pattern = "Det Ctrl|Inc Ctrl|Inc Ctrl 1|Inc Ctrl 2|Ext Ctrl"
  )

  # update meta_dat
  meta_dat[4L, 1L] <- "SampleID"
  meta_dat[4L, control_index] <- meta_dat[2L, control_index]
  meta_dat[3L, control_index] <- "-"

  # total number of internal controls
  nr_controls <- sum(control_index)

  # number of deviations from internal controls
  nr_deviations <- stringr::str_detect(
    string = meta_dat[2L, ],
    pattern = "QC Deviation from median"
  ) |>
    sum()

  # number of panels
  nr_panel <- (ncol(meta_dat)
               - 1L
               - nr_deviations
               - nr_controls) / (base_index + 2L)

  # df of plate cols and number of plates
  plates <- dat[, ((base_index * nr_panel)
                   + nr_controls
                   + 2L):(ncol(dat)
                          - nr_panel
                          - nr_deviations)] |>
    dplyr::distinct() |>
    tidyr::drop_na()

  nr_plates <- plates |>
    dplyr::pull(
      dplyr::all_of(1L)
    ) |>
    unique() |>
    length()

  # update base_index
  base_index <- ifelse(nr_controls > 0L,
                       base_index + nr_controls / nr_panel,
                       base_index)

  # add bottom matrix to top matrix `meta_dat` ----

  # missing frequency
  meta_dat <- meta_dat |>
    dplyr::bind_rows(
      dat |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["Name"]],
            pattern = "Missing Data freq."
          )
        )
    )

  # if it is quantified data
  if (is_npx == FALSE) {
    meta_dat <- meta_dat |>
      # lower limit of quantification
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "LLOQ"
            )
          )
      ) |>
      # upper limit of quantification
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "ULOQ"
            )
          )
      ) |>
      # assay warning
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Assay warning"
            )
          )
      ) |>
      # plate lower quantifiable level
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Lowest quantifiable level"
            )
          )
      ) |>
      # plate LOD
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "Plate LOD|PlateLOD|Plate_LOD"
            )
          )
      )
  } else { # if it is NPX data
    # LOD
    meta_dat <- meta_dat |>
      dplyr::bind_rows(
        dat |>
          dplyr::filter(
            stringr::str_detect(
              string = .data[["Name"]],
              pattern = "LOD"
            )
          )
      )
  }

  # normalization
  meta_dat <- meta_dat |>
    dplyr::bind_rows(
      dat |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["Name"]],
            pattern = "Normalization"
          )
        )
    )

  # remove the meta data from dat ----

  norm_flag <- grepl(pattern = "Normalization", x = dat$Name) |>
    any()

  nbr_meta_data_rows_bottom <- ifelse(is_npx == TRUE,
                                      3L,
                                      4L + (3L * nr_plates))
  nbr_meta_data_rows_bottom <- ifelse(norm_flag == TRUE,
                                      nbr_meta_data_rows_bottom,
                                      nbr_meta_data_rows_bottom - 1L)

  dat <- dat[c(-1L * (nrow(dat) - nbr_meta_data_rows_bottom):nrow(dat)), ]

  # sample id and index vector
  sample_id <- dat$Name
  index_nr <- seq_len(length(sample_id))

  # initialize list for outputs ----

  # Initiate lists for later use
  panel_data <- list() ##NPX values to be stored
  qc_list <- list()    ##QC data
  meta_data_list <- list() ## meta data
  panel_list <- list()  ## combination of panel data and QC
  assay_name_list <- list()
  panel_list_long <- list()

  # Construct a list of tibbles that match the long format
  for (i in seq_len(nr_panel)) {

    panel_data[[i]] <- dat[, (2L + ((i - 1L) * base_index)):
                             ((base_index + 1L) + ((i - 1L) * base_index))]

    if (nr_deviations == 0L) {

      qc_list[[i]] <- dat[, c(
        (2L + (nr_panel * base_index) + (i - 1L)),
        (2L + (nr_panel * base_index) + (i - 1L)) + nr_panel
      )]

      meta_data_list[[i]] <- meta_dat[, c(
        (2L + ((i - 1L) * base_index)):
          ((base_index + 1L) + ((i - 1L) * base_index)),
        (2L + (nr_panel * base_index) + (i - 1L)),
        (2L + (nr_panel * base_index) + (i - 1L)) + nr_panel
      )]

    } else {

      qc_list[[i]] <- dat[, c(
        2L + (nr_panel * base_index) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + nr_panel,
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel) + i
      )]

      meta_data_list[[i]] <- meta_dat[, c(
        (2L + (i - 1L) * base_index):(base_index + 1L + (i - 1L) * base_index),
        2L + (nr_panel * base_index) + (i - 1L),
        2L + (nr_panel * base_index) + (i - 1L) + nr_panel,
        2L + (nr_panel * base_index) + (i - 1L) + (2L * nr_panel),
        2L + (nr_panel * base_index) + (i - 1L) + (3L * nr_panel)
      )]

      meta_data_list[[i]][4L, (base_index + 3L)] <- "QC Deviation Inc Ctrl"
      meta_data_list[[i]][4L, (base_index + 4L)] <- "QC Deviation Det Ctrl"

    }

    meta_data_list[[i]][4L, (base_index + 1L)] <-
      meta_data_list[[i]][2L, (base_index + 1L)]
    meta_data_list[[i]][4L, (base_index + 2L)] <-
      meta_data_list[[i]][2L, (base_index + 2L)]

    panel_list[[i]] <- dplyr::bind_cols(panel_data[[i]], qc_list[[i]])
    colnames(panel_list[[i]]) <- unlist(meta_data_list[[i]][4L, ])

    panel_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))] <-
      lapply(panel_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))],
             function(.x) {
               stringr::str_replace_all(
                 string = .x,
                 pattern = c("#" = "",
                             "," = ".",
                             "No Data" = NA_character_,
                             "> ULOQ" = NA_character_,
                             "< LLOQ" = NA_character_)
               ) |>
                 as.numeric()
             })

    # Remove the last two columns since they contain redundant meta data and
    # will only cause warnings
    meta_data_list[[i]] <-
      meta_data_list[[i]][, c(-(base_index + 1L), -(base_index + 2L))]

    assay_name_list[[i]] <- tidyr::tibble(
      ID = meta_data_list[[i]][4L, ] |> as.character(),
      Name = meta_data_list[[i]][2L, ] |> as.character(),
      UniProt = meta_data_list[[i]][3L, ] |> as.character(),
      Panel = meta_data_list[[i]][1L, ] |> as.character()
    )

    if (is_npx == TRUE) {

      assay_name_list[[i]] <- assay_name_list[[i]] |>
        dplyr::bind_cols(
          dplyr::tibble(
            MissingFreq = meta_data_list[[i]][5L, ] |> as.character(),
            LOD = meta_data_list[[i]][6L, ] |> as.numeric()
          )
        )

      if (norm_flag == TRUE) {

        assay_name_list[[i]] <- assay_name_list[[i]] |>
          dplyr::bind_cols(
            dplyr::tibble(
              Normalization = meta_data_list[[i]][7L, ] |> as.character()
            )
          )
      }

      panel_list_long[[i]] <- panel_list[[i]] |>
        dplyr::mutate(
          SampleID = sample_id,
          Index = index_nr
        ) |>
        tidyr::pivot_longer(
          -dplyr::any_of(
            c("SampleID", "QC Warning", "Plate ID", "Index",
              "QC Deviation Inc Ctrl", "QC Deviation Det Ctrl")
          ),
          names_to = "Assay",
          values_to = "NPX"
        ) |>
        dplyr::left_join(
          assay_name_list[[i]],
          by = c("Assay" = "ID")
        ) |>
        dplyr::select(
          dplyr::all_of(
            c("SampleID", "Index", "OlinkID" = "Assay", "UniProt",
              "Assay" = "Name", "MissingFreq", "Panel", "PlateID" = "Plate ID",
              "QC_Warning" = "QC Warning", "LOD", "NPX")
          ),
          dplyr::any_of(
            c("Assay_Warning", "Normalization", "QC Deviation Inc Ctrl",
              "QC Deviation Det Ctrl")
          )
        )

    } else {
      for (j in 1:nr_plates) {

        assay_name_by_plate <- assay_name_list[[i]] |>
          dplyr::bind_cols(
            dplyr::tibble(
              Unit = meta_data_list[[i]][5L, ] |> as.character(),
              MissingFreq = meta_data_list[[i]][6L, ] |> as.character(),
              LLOQ = meta_data_list[[i]][7L, ] |> as.numeric(),
              ULOQ = meta_data_list[[i]][8L, ] |> as.numeric(),
              Assay_Warning = meta_data_list[[i]][(9L + j - 1L), ] |>
                as.character(),
              Plate_LQL = meta_data_list[[i]][(9L + nr_plates + j - 1L), ] |>
                as.numeric(),
              LOD = meta_data_list[[i]][(9L + (2L * nr_plates) + j - 1L), ] |>
                as.numeric()
            )
          )

        if (norm_flag == TRUE) {

          assay_name_by_plate <- assay_name_by_plate |>
            dplyr::bind_cols(
              dplyr::tibble(
                Normalization = meta_data_list[[i]][9L + (3L * nr_plates), ] |>
                  as.character()
              )
            )

        }

        panel_lst_long_idx <- ifelse(nr_plates > 1L,
                                     (i - 1L) * 2L + j,
                                     i)

        panel_list_long[[panel_lst_long_idx]] <- panel_list[[i]] |>
          dplyr::mutate(
            SampleID = sample_id,
            Index = index_nr
          ) |>
          tidyr::pivot_longer(
            -dplyr::any_of(
              c("SampleID", "QC Warning", "Plate ID", "Index",
                "QC Deviation Inc Ctrl", "QC Deviation Det Ctrl")
            ),
            names_to = "Assay",
            values_to = "NPX"
          ) |>
          dplyr::filter(
            .data[["Plate ID"]] == as.character(plates[j, i])
          ) |>
          dplyr::left_join(
            assay_name_by_plate,
            by = c("Assay" = "ID")
          ) |>
          dplyr::select(
            dplyr::all_of(
              c("SampleID", "Index", "OlinkID" = "Assay", "UniProt",
                "Assay" = "Name", "MissingFreq", "Panel",
                "PlateID" = "Plate ID", "QC_Warning" = "QC Warning", "LOD",
                "NPX", "Unit", "ULOQ", "LLOQ", "Plate_LQL", "Assay_Warning")
            ),
            dplyr::any_of(
              c("Normalization", "QC Deviation Inc Ctrl",
                "QC Deviation Det Ctrl")
            )
          )
      }
    }
  }

  # reshape and cleanup final dataset ----

  # rename columns LOD and NPX to Plate_LOD and Quantified_value, respectively.
  # if data is absolute quantification
  if (is_npx == FALSE) {
    panel_list_long <- lapply(
      panel_list_long,
      function(.x) {
        lookup_rename <- c("Plate_LOD" = "LOD",
                           "Quantified_value" = "NPX")
        .x |>
          dplyr::rename(
            dplyr::all_of(lookup_rename)
          )
      }
    )
  }

  df_long <- panel_list_long |>
    dplyr::bind_rows() |>
    # remove any row with SampleID = NA
    dplyr::filter(
      !is.na(.data[["SampleID"]])
    ) |>
    # extract Panel_Version
    dplyr::mutate(
      Panel_Version = stringr::str_replace_all(string = .data[["Panel"]],
                                               pattern = ".*\\(",
                                               replacement = "") |>
        stringr::str_replace_all(pattern = "\\)",
                                 replacement = "")
    ) |>
    # adjust Panel
    dplyr::mutate(
      Panel = stringr::str_replace_all(string = .data[["Panel"]],
                                       pattern = "\\(.*\\)",
                                       replacement = "") |>
        stringr::str_to_title() |>
        stringr::str_replace_all(pattern = "Target 96",
                                 replacement = "") |>
        stringr::str_replace_all(pattern = "Target 48",
                                 replacement = "") |>
        stringr::str_replace_all(pattern = "Olink",
                                 replacement = "") |>
        trimws(which = "left")
    ) |>
    # split panel into two columns for further adjustment
    tidyr::separate(
      col = .data[["Panel"]],
      sep = " ",
      into = c("Panel_Start", "Panel_End"),
      fill = "right"
    ) |>
    # cleanup Panel_End and recombine Panel_Start with Panel_End to Panel
    dplyr::mutate(
      Panel_End = dplyr::if_else(grepl("Ii", .data[["Panel_End"]]),
                                 stringr::str_to_upper(.data[["Panel_End"]]),
                                 .data[["Panel_End"]]),
      Panel_End = dplyr::if_else(is.na(.data[["Panel_End"]]),
                                 " ",
                                 .data[["Panel_End"]]),
      Panel = paste("Olink", .data[["Panel_Start"]], .data[["Panel_End"]]) |>
        trimws(which = "right")
    ) |>
    # keep relevant columns
    dplyr::select(
      -dplyr::all_of(c("Panel_Start", "Panel_End"))
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("SampleID", "Index", "OlinkID", "UniProt", "Assay", "MissingFreq",
          "Panel", "Panel_Version", "PlateID", "QC_Warning")
      ),
      dplyr::any_of(
        c("Plate_LQL", "LOD", "Plat_LOD", "LLOQ", "ULOQ", "NPX",
          "Quantified_value", "Unit", "Assay_Warning", "Normalization")
      ),
      dplyr::matches(c("*Inc Ctrl*", "*Det Ctrl*"))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("NPX", "Quantified_value")),
        ~ as.numeric(.x)
      )
    )

  return(df_long)
}
