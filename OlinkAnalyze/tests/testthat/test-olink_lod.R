# Test check_ht_fixed_lod_version ----

test_that("no message when darid_invalid is empty", {
  expect_no_message(
    check_ht_fixed_lod_version(
      check_log = list(darid_invalid = character(0)),
      lod_file  = data.frame(Panel = "Explore_HT")
    )
  )
})

test_that("warn when missing Version column", {
  msgs <- capture_messages(
    check_ht_fixed_lod_version(
      check_log = list(darid_invalid = "D10007"),
      lod_file  = data.frame(Panel = "Explore_HT")
    )
  )

  expect_true(
    any(stringr::str_detect(
      msgs,
      "Outdated version of Fixed LOD file detected"
    )),

    any(stringr::str_detect(
      msgs,
      "Please download the newest version from Olink.com"
    ))
  )
})

test_that("warn when multiple version values are presented", {
  msgs <- capture_messages(
    check_ht_fixed_lod_version(
      check_log = list(darid_invalid = "D10007"),
      lod_file = data.frame(
        Panel = rep("Explore_HT", 2),
        Version = c("6.0.0", "7.0.0"),
        stringsAsFactors = FALSE
      )
    )
  )

  expect_true(
    any(stringr::str_detect(
      msgs,
      "Multiple Fixed LOD versions detected in the file"
    ))
  )
})

test_that("warn when version lower than minimum", {
  msgs <- capture_messages(
    check_ht_fixed_lod_version(
      check_log = list(darid_invalid = "D10007"),
      lod_file = data.frame(
        Panel = "Explore_HT",
        Version = "5.9.0",
        stringsAsFactors = FALSE
      )
    )
  )

  expect_true(any(stringr::str_detect(
    msgs,
    "Outdated version of Fixed LOD file detected"
  )))

  expect_true(any(stringr::str_detect(
    msgs,
    stringr::fixed("Detected version: 5.9.0")
  )))

  expect_true(any(stringr::str_detect(
    msgs,
    stringr::fixed("Minimum required version: 6.0.0")
  )))

  expect_true(any(stringr::str_detect(
    msgs,
    stringr::fixed("Please download the newest version from Olink.com")
  )))

})

test_that("works - version >= min_version passes silently", {

  expect_no_message(
    check_ht_fixed_lod_version(
      check_log = list(darid_invalid = "D10007"),
      lod_file = data.frame(
        Panel = "Explore_HT",
        Version = "6.0.0",
        stringsAsFactors = FALSE
      )
    )
  )

})
