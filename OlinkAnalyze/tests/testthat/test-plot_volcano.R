test_that(
  "olink_volcano_plot - works",
  {
    # Load reference results
    ref_results <- get_example_data("reference_results.rds")

    skip_on_cran()
    skip_if_not_installed("vdiffr")

    # There's some randomness to how the labels are placed on the plot.
    # Setting the seed should avoid this
    set.seed(10)

    # plot v1 ----

    volcano_plot <- olink_volcano_plot(
      p.val_tbl = ref_results$t_test,
      olinkid_list = c("")
    )
    volcano_plot_name <- "volcano plot"
    check_snap_exist(test_dir_name = "plot_volcano",
                     snap_name = volcano_plot_name)
    vdiffr::expect_doppelganger(volcano_plot_name, volcano_plot)

    # plot v2 ----

    volcano_plot2 <- olink_volcano_plot(
      p.val_tbl = ref_results$t_test,
      olinkid_list = c(""),
      coloroption =  c("teal", "pink")
    )
    volcano_plot2_name <- "volcano plot with coloroption"
    check_snap_exist(test_dir_name = "plot_volcano",
                     snap_name = volcano_plot2_name)
    vdiffr::expect_doppelganger(volcano_plot2_name, volcano_plot2)

    # plot v3 ----

    volcano_plot_sig <- olink_volcano_plot(
      p.val_tbl = ref_results$t_test,
      coloroption =  c("teal", "pink")
    )
    volcano_plot3_name <- "volcano plot with labels"
    check_snap_exist(test_dir_name = "plot_volcano",
                     snap_name = volcano_plot3_name)
    vdiffr::expect_doppelganger(volcano_plot3_name, volcano_plot_sig)
  }
)

test_that(
  "olink_volcano_plot - error - ...",
  {
    # Load reference results
    ref_results <- get_example_data("reference_results.rds")

    expect_error(
      object = olink_volcano_plot(
        p.val_tbl = ref_results$t_test,
        fake_variable = 1
      ),
      regexp = paste("The `...` option only takes the coloroption argument.",
                     "`...` currently contains the variable",
                     "\"fake_variable\"."),
      fixed = TRUE
    )

    expect_error(
      object = olink_volcano_plot(
        p.val_tbl = ref_results$t_test,
        fake_variable1 = 1,
        fake_variable2 = 2
      ),
      regexp = paste("The `...` option only takes one argument. `...`",
                     "currently contains the variables \"fake_variable1\" and",
                     "\"fake_variable2\"."),
      fixed = TRUE
    )
  }
)
