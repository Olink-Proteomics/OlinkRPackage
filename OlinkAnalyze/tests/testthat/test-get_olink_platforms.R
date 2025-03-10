# Test get_olink_platforms ----

test_that(
  "get_olink_platforms - works",
  {
    # all platforms ----

    expect_true(
      object = identical(
        x = accepted_olink_platforms$name |>
          unique() |>
          sort(),
        y = get_olink_platforms()
      )
    )

    # broader platforms ----

    lapply(
      accepted_olink_platforms$broader_platform |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["broader_platform"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(broad_platform = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # data type ----

    lapply(
      accepted_olink_platforms$quant_method |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method"))
            ) |>
            dplyr::filter(
              .data[["quant_method"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(data_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # quantification method ----

    lapply(
      accepted_olink_platforms$quant_type |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type"))
            ) |>
            dplyr::filter(
              .data[["quant_type"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(quant_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # broader platforms and data type ----

    bplat_dt <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "quant_method"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_dt)),
      function(i) {
        bplat_dt_i <- bplat_dt |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_dt_i$broader_platform
              & .data[["quant_method"]] == bplat_dt_i$quant_method
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(broad_platform = bplat_dt_i$broader_platform,
                                  data_type = bplat_dt_i$quant_method)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_dt)

    # broader platforms and quantification method ----

    bplat_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_qm)),
      function(i) {
        bplat_qm_i <- bplat_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_qm_i$broader_platform
              & .data[["quant_type"]] == bplat_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(broad_platform = bplat_qm_i$broader_platform,
                                  quant_type = bplat_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_qm)

    # data type and quantification method ----

    dt_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(dt_qm)),
      function(i) {
        dt_qm_i <- dt_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["quant_method"]] == dt_qm_i$quant_method
              & .data[["quant_type"]] == dt_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(data_type = dt_qm_i$quant_method,
                                  quant_type = dt_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(dt_qm)

    # broader platforms, data type and quantification method ----

    bplat_dt_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "quant_method", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_dt_qm)),
      function(i) {
        bplat_dt_qm_i <- bplat_dt_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_dt_qm_i$broader_platform
              & .data[["quant_method"]] == bplat_dt_qm_i$quant_method
              & .data[["quant_type"]] == bplat_dt_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["name"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_platforms(
            broad_platform = bplat_dt_qm_i$broader_platform,
            data_type = bplat_dt_qm_i$quant_method,
            quant_type = bplat_dt_qm_i$quant_type
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_dt_qm)
  }
)

# Test check_olink_platform ----

test_that(
  "check_olink_platform - works",
  {
    expect_no_condition(
      object = check_olink_platform(
        x = "Target 48",
        broad_platform = NULL
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore HT",
        broad_platform = NULL
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Target 96",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore HT",
        broad_platform = "NGS"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Flex",
        data_type = "Quantified"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore 3072",
        data_type = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Focus",
        quant_type = "absolute"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore 3072",
        quant_type = "relative"
      )
    )
  }
)

test_that(
  "check_olink_platform - error - unexpected platform",
  {
    # random platform name
    expect_error(
      object = check_olink_platform(
        x = "Not_An_Olink_Platform",
        broad_platform = NULL
      ),
      regexp = "Unexpected Olink platform"
    )

    # qPCR platform in NGS broader platform
    expect_error(
      object = check_olink_platform(
        x = "Target 48",
        broad_platform = "NGS"
      ),
      regexp = "Unexpected Olink platform"
    )

    # qPCR platform in NGS broader platform
    expect_error(
      object = check_olink_platform(
        x = "Explore HT",
        broad_platform = "qPCR"
      ),
      regexp = "Unexpected Olink platform"
    )

    # qPCR platform in NGS broader platform
    expect_error(
      object = check_olink_platform(
        x = "Explore HT",
        broad_platform = "NGS",
        data_type = "Ct"
      ),
      regexp = "No Olink platform detected"
    )
  }
)

# Test get_olink_broader_platforms ----

test_that(
  "get_olink_broader_platforms - works",
  {
    # all broad platforms ----

    expect_true(
      object = identical(
        x = accepted_olink_platforms$broader_platform |>
          unique() |>
          sort(),
        y = get_olink_broader_platforms()
      )
    )

    # platforms ----

    lapply(
      accepted_olink_platforms$name,
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["name"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(platform_name = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # data type ----

    lapply(
      accepted_olink_platforms$quant_method |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method"))
            ) |>
            dplyr::filter(
              .data[["quant_method"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(data_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # quantification method ----

    lapply(
      accepted_olink_platforms$quant_type |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type"))
            ) |>
            dplyr::filter(
              .data[["quant_type"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(quant_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # platforms and data type ----

    plat_dt <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_method"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(plat_dt)),
      function(i) {
        plat_dt_i <- plat_dt |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method"))
            ) |>
            dplyr::filter(
              .data[["name"]] == plat_dt_i$name
              & .data[["quant_method"]] == plat_dt_i$quant_method
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(platform_name = plat_dt_i$name,
                                          data_type = plat_dt_i$quant_method)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(plat_dt)

    # platforms and quantification method ----

    plat_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(plat_qm)),
      function(i) {
        plat_qm_i <- plat_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type"))
            ) |>
            dplyr::filter(
              .data[["name"]] == plat_qm_i$name
              & .data[["quant_type"]] == plat_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(platform_name = plat_qm_i$name,
                                          quant_type = plat_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(plat_qm)

    # data type and quantification method ----

    dt_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(dt_qm)),
      function(i) {
        dt_qm_i <- dt_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["quant_method"]] == dt_qm_i$quant_method
              & .data[["quant_type"]] == dt_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(data_type = dt_qm_i$quant_method,
                                          quant_type = dt_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(dt_qm)

    # platforms, data type and quantification method ----

    plat_dt_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method", "quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_method", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(plat_dt_qm)),
      function(i) {
        plat_dt_qm_i <- plat_dt_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["name"]] == plat_dt_qm_i$name
              & .data[["quant_method"]] == plat_dt_qm_i$quant_method
              & .data[["quant_type"]] == plat_dt_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["broader_platform"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_broader_platforms(platform_name = plat_dt_qm_i$name,
                                          data_type = plat_dt_qm_i$quant_method,
                                          quant_type = plat_dt_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(plat_dt_qm)
  }
)

# Test check_olink_broader_platform ----

test_that(
  "check_olink_broader_platform - works",
  {
    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        platform_name = "Target 96"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        platform_name = "Flex"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS",
        platform_name = "Explore HT"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS",
        data_type = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        data_type = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        data_type = "Quantified"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        quant_type = "absolute"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS",
        platform_name = "Explore 3072",
        data_type = "NPX",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR",
        platform_name = "Target 48",
        data_type = "Ct",
        quant_type = "relative"
      )
    )
  }
)

test_that(
  "check_olink_broader_platform - error - unexpected broader platform",
  {
    # random broader platform name
    expect_error(
      object = check_olink_broader_platform(
        x = "Not_An_Olink_Platform"
      ),
      regexp = "Unexpected Olink broader platform"
    )

    # non existent combo NGS
    expect_error(
      object = check_olink_broader_platform(
        x = "NGS",
        platform_name = "Target 96"
      ),
      regexp = "Unexpected Olink broader platform"
    )

    # non existent combo qPCR
    expect_error(
      object = check_olink_broader_platform(
        x = "qPCR",
        platform_name = "Explore HT"
      ),
      regexp = "Unexpected Olink broader platform"
    )

    # non existent combo qPCR v2
    expect_error(
      object = check_olink_broader_platform(
        x = "qPCR",
        platform_name = "Target 96",
        data_type = "NPX",
        quant_type = "absolute"
      ),
      regexp = "No Olink broad platform detected"
    )
  }
)

# Test get_olink_data_types ----

test_that(
  "get_olink_data_types - works",
  {
    # all data types ----

    expect_true(
      object = identical(
        x = accepted_olink_platforms$quant_method |>
          unlist() |>
          unique() |>
          sort(),
        y = get_olink_data_types()
      )
    )

    # broad platforms ----

    lapply(
      accepted_olink_platforms$broader_platform |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["broader_platform"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_data_types(broad_platform = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # platforms ----

    lapply(
      accepted_olink_platforms$name,
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["name"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_data_types(platform_name = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # quantification method ----

    lapply(
      accepted_olink_platforms$quant_type |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["quant_type"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_data_types(quant_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # broad platforms and platforms ----

    bplat_plat <- accepted_olink_platforms |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "name"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_plat)),
      function(i) {
        bplat_plat_i <- bplat_plat |>
          dplyr::slice(.env[["i"]])

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_plat_i$broader_platform
              & .data[["name"]] == bplat_plat_i$name
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_data_types(
            broad_platform = bplat_plat_i$broader_platform,
            platform_name = bplat_plat_i$name
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_plat)

    # broad platforms and quantification method ----

    bplat_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_qm)),
      function(i) {
        bplat_qm_i <- bplat_qm |>
          dplyr::slice(.env[["i"]])

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_qm_i$broader_platform
              & .data[["quant_type"]] == bplat_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_data_types(
            broad_platform = bplat_qm_i$broader_platform,
            quant_type = bplat_qm_i$quant_type
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_qm)

    # platforms and quantification method ----

    plat_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(plat_qm)),
      function(i) {
        plat_qm_i <- plat_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["name"]] == plat_qm_i$name
              & .data[["quant_type"]] == plat_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_data_types(platform_name = plat_qm_i$name,
                                   quant_type = plat_qm_i$quant_type)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(plat_qm)

    # broad platforms, platforms and quantification method ----

    bplat_plat_qm <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_type"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "name", "quant_type"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_plat_qm)),
      function(i) {
        bplat_plat_qm_i <- bplat_plat_qm |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_plat_qm_i$broader_platform
              & .data[["name"]] == bplat_plat_qm_i$name
              & .data[["quant_type"]] == bplat_plat_qm_i$quant_type
            ) |>
            dplyr::pull(
              .data[["quant_method"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_data_types(
            broad_platform = bplat_plat_qm_i$broader_platform,
            platform_name = bplat_plat_qm_i$name,
            quant_type = bplat_plat_qm_i$quant_type
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_plat_qm)
  }
)

# Test check_olink_data_type ----

test_that(
  "check_olink_data_type - works",
  {
    expect_no_condition(
      object = check_olink_data_type(
        x = "Ct"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Quantified"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Ct",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Quantified",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        broad_platform = "NGS"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        platform = "Explore 3072"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        platform = "Target 96"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Ct",
        platform = "Target 96"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Quantified",
        platform = "Focus"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Quantified",
        quant_type = "absolute"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Ct",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Quantified",
        broad_platform = "qPCR",
        platform_name = "Flex",
        quant_type = "absolute"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "NPX",
        broad_platform = "NGS",
        platform_name = "Explore HT",
        quant_type = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_data_type(
        x = "Ct",
        broad_platform = "qPCR",
        platform_name = "Focus",
        quant_type = "relative"
      )
    )
  }
)

test_that(
  "check_olink_data_type - error - unexpected data_type",
  {
    expect_error(
      object = check_olink_data_type(
        x = "Wrong_Data_Type"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Wrong_Data_Type",
        broad_platform = "qPCR"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Wrong_Data_Type",
        broad_platform = "NGS"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Ct",
        broad_platform = "NGS"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Ct",
        platform = "Explore HT"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Quantified",
        platform = "Target 96"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Quantified",
        platform = "Explore 3072"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Quantified",
        quant_type = "relative"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "NPX",
        quant_type = "absolute"
      ),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(
        x = "Quantified",
        broad_platform = "qPCR",
        platform_name = "Target 96",
        quant_type = "absolute"
      ),
      regexp = "No Olink data type detected"
    )

    expect_error(
      object = check_olink_data_type(
        x = "NPX",
        broad_platform = "NGS",
        platform_name = "Explore HT",
        quant_type = "absolute"
      ),
      regexp = "No Olink data type detected"
    )
  }
)

# Test get_olink_quant_types ----

test_that(
  "get_olink_quant_types - works",
  {
    # all quantificantion types ----

    expect_true(
      object = identical(
        x = accepted_olink_platforms$quant_type |>
          unlist() |>
          unique() |>
          sort(),
        y = get_olink_quant_types()
      )
    )

    # broad platforms ----

    lapply(
      accepted_olink_platforms$broader_platform |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["broader_platform"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_quant_types(broad_platform = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # platforms ----

    lapply(
      accepted_olink_platforms$name,
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["name"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_quant_types(platform_name = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # data type ----

    lapply(
      accepted_olink_platforms$quant_method |>
        unlist() |>
        unique(),
      function(x) {
        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["quant_method"]] == .env[["x"]]
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_quant_types(data_type = x)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()

    # broad platforms and platforms ----

    bplat_plat <- accepted_olink_platforms |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "name"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_plat)),
      function(i) {
        bplat_plat_i <- bplat_plat |>
          dplyr::slice(.env[["i"]])

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_plat_i$broader_platform
              & .data[["name"]] == bplat_plat_i$name
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unlist() |>
            unique() |>
            sort(),
          y = get_olink_quant_types(
            broad_platform = bplat_plat_i$broader_platform,
            platform_name = bplat_plat_i$name
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_plat)

    # broad platforms and data type ----

    bplat_dt <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "quant_method"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_dt)),
      function(i) {
        bplat_dt_i <- bplat_dt |>
          dplyr::slice(.env[["i"]])

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_dt_i$broader_platform
              & .data[["quant_method"]] == bplat_dt_i$quant_method
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_quant_types(
            broad_platform = bplat_dt_i$broader_platform,
            data_type = bplat_dt_i$quant_method
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_dt)

    # platforms and data type ----

    plat_dt <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_method"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(plat_dt)),
      function(i) {
        plat_dt_i <- plat_dt |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_type", "quant_method"))
            ) |>
            dplyr::filter(
              .data[["name"]] == plat_dt_i$name
              & .data[["quant_method"]] == plat_dt_i$quant_method
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_quant_types(platform_name = plat_dt_i$name,
                                    data_type = plat_dt_i$quant_method)
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(plat_dt)

    # broad platforms, platforms and data type ----

    bplat_plat_dt <- accepted_olink_platforms |>
      tidyr::unnest(
        cols = dplyr::all_of(c("quant_method"))
      ) |>
      dplyr::select(
        dplyr::all_of(c("broader_platform", "name", "quant_method"))
      ) |>
      dplyr::distinct()

    lapply(
      seq_len(nrow(bplat_plat_dt)),
      function(i) {
        bplat_plat_dt_i <- bplat_plat_dt |>
          dplyr::slice(i)

        identical( # nolint return_linter
          x = accepted_olink_platforms |>
            tidyr::unnest(
              cols = dplyr::all_of(c("quant_method", "quant_type"))
            ) |>
            dplyr::filter(
              .data[["broader_platform"]] == bplat_plat_dt_i$broader_platform
              & .data[["name"]] == bplat_plat_dt_i$name
              & .data[["quant_method"]] == bplat_plat_dt_i$quant_method
            ) |>
            dplyr::pull(
              .data[["quant_type"]]
            ) |>
            unique() |>
            sort(),
          y = get_olink_quant_types(
            broad_platform = bplat_plat_dt_i$broader_platform,
            platform_name = bplat_plat_dt_i$name,
            data_type = bplat_plat_dt_i$quant_method
          )
        )
      }
    ) |>
      unlist() |>
      all() |>
      expect_true()
    rm(bplat_plat_dt)
  }
)

# Test check_olink_quant_type ----

test_that(
  "check_olink_quant_type - works",
  {
    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute",
        broad_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        broad_platform = "NGS"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        platform = "Explore 3072"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        platform = "Target 96"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute",
        platform = "Target 48"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute",
        platform = "Focus"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute",
        data_type = "Quantified"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        data_type = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        data_type = "Ct"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "absolute",
        broad_platform = "qPCR",
        platform_name = "Focus",
        data_type = "Quantified"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        broad_platform = "NGS",
        platform_name = "Explore HT",
        data_type = "NPX"
      )
    )

    expect_no_condition(
      object = check_olink_quant_type(
        x = "relative",
        broad_platform = "qPCR",
        platform_name = "Flex",
        data_type = "Ct"
      )
    )
  }
)

test_that(
  "check_olink_quant_type - error - unexpected quant_type",
  {
    expect_error(
      object = check_olink_quant_type(
        x = "Wrong_Quantification_Type"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "Wrong_Data_Type",
        broad_platform = "qPCR"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "Wrong_Data_Type",
        broad_platform = "NGS"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        broad_platform = "NGS"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        platform = "Explore HT"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        platform = "Target 96"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "relative",
        data_type = "Quantified"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        data_type = "Ct"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        data_type = "NPX"
      ),
      regexp = "Unexpected Olink quantification type"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        broad_platform = "qPCR",
        platform_name = "Target 96",
        data_type = "Quantified"
      ),
      regexp = "No Olink quantification type detected"
    )

    expect_error(
      object = check_olink_quant_type(
        x = "absolute",
        broad_platform = "qPCR",
        platform_name = "Explore HT",
        data_type = "Ct"
      ),
      regexp = "No Olink quantification type detected"
    )
  }
)
