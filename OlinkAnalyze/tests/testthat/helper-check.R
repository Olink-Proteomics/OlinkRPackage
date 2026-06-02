# check if snapshot exists. if not skip the test.
check_snap_exist <- function(test_dir_name, snap_name) {
  # check that "_snaps" exist, otherwise skip
  base_test_dir <- test_path("_snaps")
  skip_if_not(dir.exists(base_test_dir))

  # check that test-specific snaps directory exist. if not skip.
  test_dir <- test_path("_snaps", test_dir_name)
  skip_if_not(dir.exists(test_dir))

  # check that snapshot exist. if not skip
  stopifnot(rlang::is_scalar_character(snap_name))
  snap_name_clean <- gsub("[^a-z0-9]", "-", tolower(snap_name))
  snap_name_clean <- gsub(paste0("-", "-", "+"), "-", snap_name_clean)
  snap_name_clean <- gsub(paste0("^", "-", "|", "-", "$"), "", snap_name_clean)
  snap_name_clean <- paste0(snap_name_clean, ".svg")

  snap_path <- test_path("_snaps", test_dir_name, snap_name_clean)
  skip_if_not(file.exists(snap_path))

  return(invisible(NULL))
}

# check if ggplot objects are equal by comparing the built ggplot objects, the
# mapping, the geom and stat of each layer, and the labels. This is preferred
# over comparing the ggplot objects directly, which can fail due to differences
# in attributes or other non-essential components of the ggplot objects.
expect_equal_ggplot <- function(object, expected) {
  testthat::expect_s3_class(object, "ggplot")
  testthat::expect_s3_class(expected, "ggplot")

  object_built   <- ggplot2::ggplot_build(object)
  expected_built <- ggplot2::ggplot_build(expected)

  testthat::expect_equal(
    object_built$data,
    expected_built$data,
    ignore_attr = TRUE
  )

  testthat::expect_equal(
    object$mapping,
    expected$mapping
  )

  testthat::expect_equal(
    lapply(object$layers, function(x) class(x$geom)[1L]),
    lapply(expected$layers, function(x) class(x$geom)[1L])
  )

  testthat::expect_equal(
    lapply(object$layers, function(x) class(x$stat)[1L]),
    lapply(expected$layers, function(x) class(x$stat)[1L])
  )

  testthat::expect_equal(
    object$labels,
    expected$labels
  )
}
