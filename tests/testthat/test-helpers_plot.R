# Functions create_plot() and prepare_plot_data() still need tests
source("dummy-data.R")

# Function create_plot()
test_that("create_plot() returns a ggplot2 object with the right mapping" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$axis_handling$adaptive_x_text_size,
                   specs$plot_creation$axis_handling$adaptive_titles,
                   specs$plot_creation$axis_handling$no_zero,
                   specs$plot_creation$legend,
                   specs$plot_creation$colors,
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$app_creator_settings$step_size
                )
             ), {
   df_list <- get_plot_datasets(
      dm = dm_dummy,
      ae = ae_prepared_dummy,
      ds = ds_prepared_dummy,
      subjid_var = "USUBJID",
      grouping_var = "ARM",
      metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
      time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
      unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
      tooltip_decimal_places = 4L,
      show_ungrouped = TRUE
   )
   x_axis <- get_x_axis(
         dataset = df_list$axis_df,
         metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
         time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
         unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
         step_size_days = 50L, step_size_weeks = 2L, step_size_months = 1L
   )
   scale <- get_x_scale(
         time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
         x_axis = x_axis
      )

   legend <- get_legend_text(dataset = dm_dummy, grouping_var = "ARM")


   plt <- create_plot(dm = dm_dummy,
                      ds = ds_prepared_dummy,
                      ae = ae_prepared_dummy,
                      subjid_var = "USUBJID",
                      grouping_var = "ARM",
                      selected_levels = c("Drug 1", "Drug 2", "Placebo"),
                      metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                      time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                      unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                      tooltip_decimal_places = 4L,
                      x_step_size_list = list(days = 50L, weeks = 2L, months = 1),
                      color_palette = generate_palette(unique(dm_dummy$ARM)),
                      show_ungrouped = TRUE)

   expect_s3_class(plt, "ggplot")
   expect_s3_class(plt$scales$scales[[1]], class(scale)[1])
   expect_identical(plt$scales$scales[[1]]$breaks, scale$breaks)
   expect_identical(plt$scales$scales[[1]]$labels, scale$labels)
   expect_identical(plt$theme$axis.text.x$size, get_x_axis_text_size(x_axis$breaks))
   expect_identical(plt$labels$y, REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
   expect_identical(plt$labels$x, x_axis$title)
   expect_identical(plt$labels$colour, legend)
   expect_identical(plt$layers[[1]]$mapping$colour, "ungrouped") # Line layer of ungrouped data
   expect_identical(rlang::expr_text(plt$layers[[2]]$mapping$colour), '~.data[["ARM"]]') # Line layer of grouped data
})

test_that("create_plot() appends the point layers after the line layers." |>
             vdoc[["add_spec"]](specs$plot_creation$hovering), {
   plt <- create_plot(dm = dm_dummy,
                      ds = ds_prepared_dummy,
                      ae = ae_prepared_dummy,
                      subjid_var = "USUBJID",
                      grouping_var = "ARM",
                      selected_levels = c("Drug 1", "Drug 2", "Placebo"),
                      metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                      time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                      unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                      tooltip_decimal_places = 4L,
                      x_step_size_list = list(days = 50L, weeks = 2L, months = 1),
                      color_palette = generate_palette(unique(dm_dummy$ARM)),
                      show_ungrouped = TRUE)

   expect_identical(class(plt$layers[[1]]$geom)[1], "GeomInteractiveLine") # ungrouped line
   expect_identical(class(plt$layers[[2]]$geom)[1], "GeomInteractiveLine") # grouped line
   expect_identical(class(plt$layers[[3]]$geom)[1], "GeomInteractivePoint") # ungrouped points
   expect_identical(class(plt$layers[[4]]$geom)[1], "GeomInteractivePoint") # grouped points
})



# Function convert_to_girafe()
test_that("convert_to_girafe() appends the point layers after the line layers." |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$hovering,
                   specs$plot_creation$highlight,
                   specs$plot_creation$colors
                )
             ), {
   plt <- create_plot(dm = dm_dummy,
                      ds = ds_prepared_dummy,
                      ae = ae_prepared_dummy,
                      subjid_var = "USUBJID",
                      grouping_var = "ARM",
                      selected_levels = c("Drug 1", "Drug 2", "Placebo"),
                      metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                      time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                      unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                      tooltip_decimal_places = 4L,
                      x_step_size_list = list(days = 50L, weeks = 2L, months = 1),
                      color_palette = generate_palette(unique(dm_dummy$ARM)),
                      show_ungrouped = TRUE)

   res <- convert_to_girafe(ggplot_obj = plt, selected_levels = c("Drug 1", "Drug 2", "Placebo"))
   expect_s3_class(res, "girafe")
   expect_true(res$x$settings$tooltip$use_fill)
   expect_identical(res$x$settings$zoom$min, 1)
   expect_identical(res$x$settings$select$type, "single")

   expected_css_select_inv <- paste0(
      ".select_inv_SVGID_ { opacity:",
      get_opacity(length(unique(dm_dummy$ARM))),
      "; filter:saturate(40%); }"
   )
   expect_identical(res$x$settings$select_inv$css, expected_css_select_inv)
})


# Function prepare_plot_data()
test_that("prepare_plot_data() returns data frame with correct columns", {
   res <- prepare_plot_data(dm = dm_dummy,
                            ds = ds_prepared_dummy,
                            ae = ae_prepared_dummy,
                            subjid_var = "USUBJID",
                            grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                            time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                            metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                            unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                            tt_dec_places = 3L)

   expect_s3_class(res, "data.frame")
   expect_true(
      all(c("USUBJID", "m", "n", "m_cumsum", "n_cumsum", "ratio", "time", "time_for_plot", "hovertext") %in% names(res))
   )
})


# Function get_plot_datasets()
test_that("get_plot_datasets() returns correct structure for no grouping" |>
         vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$ungrouped), {
   res <- get_plot_datasets(dm = dm_dummy,
                            ae = ae_prepared_dummy,
                            ds = ds_prepared_dummy,
                            subjid_var = "USUBJID",
                            grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                            time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                            metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                            unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                            tooltip_decimal_places = 3L,
                            show_ungrouped = FALSE)
   expect_named(res, c("ungrouped", "grouped", "axis_df"))
   expect_true(is.data.frame(res$ungrouped))
   expect_null(res$grouped)
   expect_equal(res$ungrouped, res$axis_df)
})

test_that("get_plot_datasets() returns correct structure for grouping with show_ungrouped = TRUE" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$grouping_and_levels$ungrouped
                )
             ), {
   res <- get_plot_datasets(dm = dm_dummy,
                            ae = ae_prepared_dummy,
                            ds = ds_prepared_dummy,
                            subjid_var = "USUBJID",
                            grouping_var = "ARM",
                            time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                            metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                            unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                            tooltip_decimal_places = 3L,
                            show_ungrouped = TRUE)
   expect_named(res, c("ungrouped", "grouped", "axis_df"))
   expect_true(is.data.frame(res$ungrouped))
   expect_true(is.data.frame(res$grouped))
   expect_identical(res$ungrouped, res$axis_df) #if both datasets arent null, the ungrouped df should be used as axis_df
})

test_that("get_plot_datasets() returns correct structure for grouping with show_ungrouped = FALSE" |>
         vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$grouped), {
   res <- get_plot_datasets(dm = dm_dummy,
                            ae = ae_prepared_dummy,
                            ds = ds_prepared_dummy,
                            subjid_var = "USUBJID",
                            grouping_var = "ARM",
                            time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                            metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                            unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                            tooltip_decimal_places = 3L,
                            show_ungrouped = FALSE)
   expect_named(res, c("ungrouped", "grouped", "axis_df"))
   expect_null(res$ungrouped)
   expect_true(is.data.frame(res$grouped))
   expect_identical(res$grouped, res$axis_df)
})

# Function select_geom()
test_that("select_geom() returns the correct ggiraph::geom_..._interactive object" |>
         vdoc[["add_spec"]](specs$plot_creation$function_type), {
   res <- select_geom(REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
   expect_equal(res, ggiraph::geom_line_interactive)

   res_2 <- select_geom(REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2)
   expect_equal(res_2, ggiraph::geom_step_interactive)
})



# Function create_line_layers()
df_ungrouped <- data.frame(
   time_for_plot = c(1:4),
   ratio = c(1:4),
   hovertext = c("a", "xy", "z", "w")
)
df_grouped <- data.frame(
   time_for_plot = c(1:4),
   ratio = c(1:4),
   hovertext = c("a", "xy", "z", "w"),
   ARM = as.factor(c("a", "b", "a", "b"))
)
test_that("create_line_layers() returns exactly one layer with the right mapping for ungrouped data only" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$highlight,
                   specs$plot_creation$colors
                )
             ), {
   df_list <- list(ungrouped = df_ungrouped, grouped = NULL, axis_df = df_ungrouped)
   res <- create_line_layers(df_list = df_list,
                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                             selected_levels = c("a"),
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)

   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_equal(res[[1]]$mapping$data_id, "ungrouped")
   expect_equal(res[[1]]$mapping$colour, "ungrouped")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$x)), "~.data[[\"time_for_plot\"]]")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$y)), "~.data[[\"ratio\"]]")
})

test_that("create_line_layers() returns exactly one layer with the right mapping for grouped data only" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$highlight,
                   specs$plot_creation$colors
                )
             ), {
   df_list <- list(ungrouped = NULL, grouped = df_grouped, axis_df = df_grouped)
   res <- create_line_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c("a", "b"),
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2)
   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$x)), "~.data[[\"time_for_plot\"]]")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$y)), "~.data[[\"ratio\"]]")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$data_id)), "~.data[[\"ARM\"]]")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$colour)), "~.data[[\"ARM\"]]")

   # no levels in selected_levels:
   res <- create_line_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c(),
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2)
   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
})

test_that("create_line_layers() returns two layers when both datasets exist" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$grouping_and_levels$ungrouped,
                specs$plot_creation$grouping_and_levels$grouped
             )
          ), {
   df_list <- list(ungrouped = df_ungrouped, grouped = df_grouped, axis_df = df_ungrouped)
   res <- create_line_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c("a", "b"),
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
   expect_length(res, 2)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_s3_class(res[[2]], "LayerInstance")
})

test_that("create_line_layers() filters the dataset to the selected levels" |>
         vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$grouped), {
   df_list <- list(ungrouped = NULL, grouped = df_grouped, axis_df = df_grouped)
   res <- create_line_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c("a"),
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
   data <- res[[1]]$data
   expect_equal(as.character(unique(data$ARM)), c("a"))
   expect_equal(nrow(data), sum(df_grouped$ARM == "a"))
})



# Function create_point_layers()
test_that("create_point_layers() returns exactly one layer with the right mapping for ungrouped data only" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$hovering
                )
             ), {
   df_list <- list(ungrouped = df_ungrouped, grouped = NULL, axis_df = df_ungrouped)
   res <- create_point_layers(df_list = df_list,
                              grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                              selected_levels = c("a"))

   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_equal(res[[1]]$mapping$data_id, "ungrouped")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$tooltip)), "~.data[[\"hovertext\"]]")
})

test_that("create_point_layers() returns exactly one layer with the right mapping for grouped data only" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$hovering
                )
             ), {
   df_list <- list(ungrouped = NULL, grouped = df_grouped, axis_df = df_grouped)
   res <- create_point_layers(df_list = df_list,
                              grouping_var = "ARM",
                              selected_levels = c("a", "b"))

   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_equal(as.character(rlang::expr_text(res[[1]]$mapping$tooltip)), "~.data[[\"hovertext\"]]")

   # no levels in selected_levels:
   res <- create_point_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c())
   expect_length(res, 1)
   expect_s3_class(res[[1]], "LayerInstance")
})

test_that("create_point_layers() returns two layers when both datasets exist" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$grouping_and_levels$ungrouped,
                specs$plot_creation$grouping_and_levels$grouped
             )
          ), {
   df_list <- list(ungrouped = df_ungrouped, grouped = df_grouped, axis_df = df_ungrouped)
   res <- create_point_layers(df_list = df_list,
                             grouping_var = "ARM",
                             selected_levels = c("a", "b"))
   expect_length(res, 2)
   expect_s3_class(res[[1]], "LayerInstance")
   expect_s3_class(res[[2]], "LayerInstance")
})

test_that("create_point_layers() filters the dataset to the selected levels" |>
         vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$grouped), {
   df_list <- list(ungrouped = NULL, grouped = df_grouped, axis_df = df_grouped)
   res <- create_point_layers(df_list = df_list,
                              grouping_var = "ARM",
                              selected_levels = c("a"))
   data <- res[[1]]$data
   expect_equal(as.character(unique(data$ARM)), c("a"))
   expect_equal(nrow(data), sum(df_grouped$ARM == "a"))
})



# Function get_x_scale()
x_axis_date <- list(breaks = as.Date(c("2020-01-01", "2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01",
                                       "2022-07-01", "2023-01-01", "2023-07-01"),
                     title = "Date"))
x_axis_day <- list(breaks = c(-250, -200, -150, -100, -50, 0, 49, 99, 149, 199, 249, 299, 349),
                   ticktext = c(-250, -200, -150, -100,  -50, 1, 50, 100, 150, 200, 250, 300, 350),
                   title = "Days")
test_that("get_x_scale() returns a ggplot2::scale_x_... object of the correct class" |>
         vdoc[["add_spec"]](specs$metric_calculation$timetype_selection), {
   res <- get_x_scale(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, x_axis_date)
   expect_equal(class(res), class(ggplot2::scale_x_date(breaks = x_axis_date$breaks)))

   res_2 <- get_x_scale(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, x_axis_day)
   expect_equal(class(res_2), class(ggplot2::scale_x_continuous(breaks = x_axis_day$breaks,
                                                                labels = x_axis_day$ticktext)))
})

test_that("get_x_scale() returns a ggplot2::scale_x_... object with the correct breaks and labels" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$axis_handling$no_zero,
                specs$app_creator_settings$step_size,
                specs$metric_calculation$timetype_selection
             )
          ), {
   res <- get_x_scale(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, x_axis_date)
   expect_equal(res$breaks, x_axis_date$breaks)

   res_2 <- get_x_scale(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, x_axis_day)
   expect_equal(res_2$breaks, x_axis_day$breaks)
   expect_equal(res_2$labels, x_axis_day$ticktext)
})



# Function get_geom_point()
test_that("get_geom_point() always returns a ggiraph::geom_point_interactive object." |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$hovering,
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$grouping_and_levels$ungrouped
                )
             ), {
   df <- data.frame(
      time_for_plot = 1:3,
      ratio = c(0.1, 0.2, 0.4),
      hovertext = c("a", "b", "c"),
      ARM = c("A", "B", "C")
   )
   res_ungrouped <- get_geom_point(dataset = df) # ungrouped case
   res_grouped <- get_geom_point(dataset = df, grouping_var = "ARM") # grouped case

   expect_equal(class(res_ungrouped), class(ggiraph::geom_point_interactive()))
   expect_equal(class(res_grouped), class(ggiraph::geom_point_interactive()))
})

test_that("get_geom_point() returns the ggiraph::geom_point_interactive with the right mapping" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$hovering,
                   specs$plot_creation$colors,
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$grouping_and_levels$grouped
                )
             ), {
   df <- data.frame(
      time_for_plot = 1:3,
      ratio = c(0.1, 0.2, 0.4),
      hovertext = c("a", "b", "c"),
      ARM = c("A", "B", "C")
   )
   res_ungrouped <- get_geom_point(dataset = df) # ungrouped case
   expect_equal(as.character(rlang::expr_text(res_ungrouped$mapping$x)), "~.data[[\"time_for_plot\"]]")
   expect_equal(as.character(rlang::expr_text(res_ungrouped$mapping$y)), "~.data[[\"ratio\"]]")
   expect_equal(res_ungrouped$mapping$data_id, "ungrouped")
   expect_equal(res_ungrouped$mapping$colour, "ungrouped")
   expect_equal(as.character(rlang::expr_text(res_ungrouped$mapping$tooltip)), "~.data[[\"hovertext\"]]")

   res_grouped <- get_geom_point(dataset = df, grouping_var = "ARM") # grouped case
   expect_equal(as.character(rlang::expr_text(res_grouped$mapping$x)), "~.data[[\"time_for_plot\"]]")
   expect_equal(as.character(rlang::expr_text(res_grouped$mapping$y)), "~.data[[\"ratio\"]]")
   expect_equal(as.character(rlang::expr_text(res_grouped$mapping$data_id)), "~.data[[\"ARM\"]]")
   expect_equal(as.character(rlang::expr_text(res_grouped$mapping$colour)), "~.data[[\"ARM\"]]")
   expect_equal(as.character(rlang::expr_text(res_grouped$mapping$tooltip)), "~.data[[\"hovertext\"]]")
})



# Function generate_palette()
levels_okabe <- letters[1:8]
levels_extended <- letters[1:14]
levels_viridis <- letters[1:15]
levels_zero <- character(0)

test_that("generate_palette() returns correct length and names" |>
             vdoc[["add_spec"]](specs$plot_creation$colors), {
   palette <- generate_palette(levels_okabe)
   expect_equal(length(palette), 9) # 8 levels + "ungrouped"
   expect_equal(names(palette), c("ungrouped", "a", "b", "c", "d", "e", "f", "g", "h"))

   palette <- generate_palette(levels_extended)
   expect_equal(length(palette), 15) # 14 levels + "ungrouped"
   expect_equal(names(palette), c("ungrouped", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"))

   palette <- generate_palette(levels_viridis)
   expect_equal(length(palette), 16) # 15 levels + "ungrouped"
   expect_equal(names(palette), c("ungrouped", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
                                     "o"))

   palette <- generate_palette(levels_zero)
   expect_equal(length(palette), 1) # 0 levels + "ungrouped"
   expect_equal(names(palette), c("ungrouped"))
})

test_that("generate_palette() uses the right color palette depending on the number of levels" |>
         vdoc[["add_spec"]](specs$plot_creation$colors), {
   palette_okabe <- generate_palette(levels_okabe)
   expected <- grDevices::palette.colors(length(palette_okabe), palette = "Okabe-Ito", recycle = FALSE)
   expect_equal(unname(palette_okabe), expected)

   palette_okabe <- generate_palette(levels_zero)
   expected <- grDevices::palette.colors(length(palette_okabe), palette = "Okabe-Ito", recycle = FALSE)
   expect_equal(unname(palette_okabe), expected)

   palette_extended <- generate_palette(levels_extended)
   expected <- rep(REPORT_RATES$PALETTES$EXTENDED, length.out = length(palette_extended))
   expect_equal(unname(palette_extended), unname(expected))

   palette_viridis <- generate_palette(levels_viridis)
   expected <- viridis::viridis(n = length(palette_viridis))
   expected[1] <- "#000000" # because viridis doesn't automatically use "#000000" for 1st level like the other palettes
   expect_equal(unname(palette_viridis), expected)

   many_levels <- letters[1:50]
   palette_many <- generate_palette(many_levels)
   expected <- viridis::viridis(n = length(palette_many))
   expected[1] <- "#000000"
   expect_equal(unname(palette_many), expected)
})

test_that("generate_palette() returns the names sorted (except of ungrouped as the first element)" |>
             vdoc[["add_spec"]](specs$plot_creation$colors), {
   levels_unsorted <- c("b", "a", "d", "x", "f", "y")
   palette <- generate_palette(levels_unsorted)
   expect_equal(names(palette), c("ungrouped", "a", "b", "d", "f", "x", "y"))
})

test_that("generate_palette() always returns a palette with black assigned to ungrouped." |>
          vdoc[["add_spec"]](specs$plot_creation$colors), {
   palette_okabe <- generate_palette(levels_okabe)
   expect_equal(palette_okabe[["ungrouped"]], "#000000")

   palette_okabe <- generate_palette(levels_zero)
   expect_equal(palette_okabe[["ungrouped"]], "#000000")

   palette_extended <- generate_palette(levels_extended)
   expect_equal(palette_extended[["ungrouped"]], "#000000")

   palette_viridis <- generate_palette(levels_viridis)
   expect_equal(palette_viridis[["ungrouped"]], "#000000")

   many_levels <- letters[1:50]
   palette_many <- generate_palette(many_levels)
   expect_equal(palette_many[["ungrouped"]], "#000000")
})

# Function palette_refresh_check()
test_that("palette_refresh_check() returns the correct logical" |>
         vdoc[["add_spec"]](specs$plot_creation$colors), {
   res_null <- palette_refresh_check(current_palette = NULL, all_levels = levels_okabe)
   expect_true(res_null)

   # checking the case when new levels appear in the data
   palette_okabe <- generate_palette(levels_okabe)
   res_new_lvls <- palette_refresh_check(palette_okabe, c(levels_okabe, "z")) # adding new level "z"
   expect_true(res_new_lvls)

   # checking the case when the levels changed completely
   res_different_lvls <- palette_refresh_check(palette_okabe, c("1", "2", "3", "4", "5", "6", "7", "8"))
   expect_true(res_different_lvls)

   # checking the case when the switch from viridis to extended is possible
   palette_viridis <- generate_palette(levels_viridis)
   res_switch_to_extended <- palette_refresh_check(palette_viridis, levels_extended)
   expect_true(res_switch_to_extended)

   # checking the case when the switch from extended to okabeIto is possible
   palette_extended <- generate_palette(levels_extended)
   res_switch_to_okabe <- palette_refresh_check(palette_extended, levels_okabe)
   expect_true(res_switch_to_okabe)

   # checking the case when some levels disappeared but the switch to another palette isn't possible
   levels_extended_without_a <- letters[2:14]
   res_no_switch <- palette_refresh_check(palette_extended, levels_extended_without_a)
   expect_false(res_no_switch)

   # checking the case when the levels remain exact the same
   res_same_lvls <- palette_refresh_check(palette_extended, levels_extended)
   expect_false(res_same_lvls)

   # checking the case when there are zero levels
   res_zero_lvls <- palette_refresh_check(palette_okabe, levels_zero)
   expect_false(res_zero_lvls)
})



# Function "get_x_axis()"
test_that("get_x_axis() returns a list with just the elements 'title' and 'breaks' if 'By study date' is selected as
          the time type" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$axis_handling$adaptive_titles,
                specs$metric_calculation$timetype_selection
             )
          ), {
   df <- data.frame(time = as.Date(c("2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")),
                    time_for_plot = as.Date(c("2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")))

   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )

   expect_equal(names(res), c("title", "breaks"))
   expect_equal(res$breaks, as.Date(c("2025-01-01", "2025-07-01")))
})

test_that("get_x_axis() returns a res$breaks with two dates, even if the dates are inside the same 6 month interval", {
   df <- data.frame(time = as.Date(c("2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")),
                    time_for_plot = as.Date(c("2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05")))
   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )

   df_2 <- data.frame(time = as.Date(c("2025-01-03")),
                      time_for_plot = as.Date(c("2025-01-03")))
   res_2 <- get_x_axis(dataset = df_2,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                       step_size_days = 50L,
                       step_size_weeks = 2L,
                       step_size_months = 1L
   )

   expect_equal(res$breaks, as.Date(c("2025-01-01", "2025-07-01")))
   expect_equal(res_2$breaks, as.Date(c("2025-01-01", "2025-07-01")))
})

test_that("get_x_axis() returns the correct interval for the breaks for the time selection 'By study date'" |>
             vdoc[["add_spec"]](specs$metric_calculation$timetype_selection), {
   df <- data.frame(time = as.Date(c("2020-06-02", "2020-09-03", "2020-09-15", "2023-01-05")),
                    time_for_plot = as.Date(c("2020-06-02", "2020-09-03", "2020-09-15", "2023-01-05")))

   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )

   df_2 <- data.frame(time = as.Date(c("2020-07-02", "2020-09-03", "2020-09-15", "2023-07-02")),
                    time_for_plot = as.Date(c("2020-07-02", "2020-09-03", "2020-09-15", "2023-07-02")))
   res_2 <- get_x_axis(dataset = df_2,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )

   # not sorted after time
   df_3 <- data.frame(time = as.Date(c("2020-09-03", "2023-07-02", "2020-09-15", "2020-07-02", "2021-04-04")),
                      time_for_plot = as.Date(c("2020-09-03", "2023-07-02", "2020-09-15", "2020-07-02", "2021-04-04")))
   res_3 <- get_x_axis(dataset = df_3,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                       step_size_days = 50L,
                       step_size_weeks = 2L,
                       step_size_months = 1L
   )

   expect_equal(res$breaks, as.Date(c("2020-01-01", "2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01",
                                      "2022-07-01", "2023-01-01", "2023-07-01")))
   expect_equal(res_2$breaks, as.Date(c("2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01",
                                      "2023-01-01", "2023-07-01", "2024-01-01")))
   expect_equal(res_3$breaks, as.Date(c("2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01",
                                        "2023-01-01", "2023-07-01", "2024-01-01")))
})

test_that("get_x_axis() returns a list with the elements 'title', 'breaks' and 'ticktext' if 'By study day' is
           selected as the time type" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$axis_handling$adaptive_titles,
                specs$plot_creation$axis_handling$no_zero,
                specs$app_creator_settings$step_size,
                specs$metric_calculation$timetype_selection
             )
          ), {
   df <- data.frame(time = c(-2, -1, 1, 2), time_for_plot = c(-2, -1, 0, 1))

   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )
   expect_equal(names(res), c("title", "breaks", "ticktext"))
})

test_that("The breaks and ticktext in the list that is returned by get_x_axis() are of the same length" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$axis_handling$no_zero,
                specs$app_creator_settings$step_size
             )
          ), {
   df <- data.frame(time = c(-2, -1, 1, 2), time_for_plot = c(-2, -1, 0, 1))

   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )
   expect_equal(length(res$breaks), length(res$ticktext))
})

test_that("The ticktext in the list that is returned by get_x_axis() should not contain a 0" |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$no_zero), {
   df <- data.frame(time = c(-2, -1, 1, 2), time_for_plot = c(-2, -1, 0, 1))

   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 2L,
                     step_size_months = 1L
   )
   expect_true(!any(res$ticktext == 0))
})

test_that("get_x_axis() returns the correct values for the ticktext if 'By study date' is selected" |>
         vdoc[["add_spec"]](specs$app_creator_settings$step_size), {
   df <- data.frame(time = c(-108:-1, 1:112), time_for_plot = -108:111)
   # metric = cumulative rate --> step_size_days will be used. Checking even stepsize
   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # == cumulative rate
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 3L,
                     step_size_months = 1L)
   expect_true(all(res$ticktext %% 50 == 0 | res$ticktext == 1))

   # metric = reporting rate per active patients; unit = weeks --> step_size_weeks will be used. Checking odd stepsize
   res_2 <- get_x_axis(dataset = df,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1, # == weeks
                       step_size_days = 50L,
                       step_size_weeks = 3L,
                       step_size_months = 1L)
   expect_true(all(res_2$ticktext %% 3 == 0 | res_2$ticktext == 1))

   # metric = reporting rate per active patients; unit = months --> step_size_months will be used. Checking stepsize = 1
   res_3 <- get_x_axis(dataset = df,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2, # == months
                       step_size_days = 50L,
                       step_size_weeks = 3L,
                       step_size_months = 1L)
   expect_equal(res_3$ticktext, df$time)
   expect_equal(res_3$breaks, df$time_for_plot)
})

test_that("get_x_axis() uses the right call of get_indices_for_tickval() depending on the metric & unit selection." |>
          vdoc[["add_spec"]](
             c(
                specs$app_creator_settings$step_size,
                specs$metric_calculation$binsize_selection
             )
          ), {
   df <- data.frame(time = c(-2, -1, 1, 2), time_for_plot = c(-2, -1, 0, 1))

   # metric = cumulative rate --> step_size_days will be used
   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # == cumulative rate
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = NULL,
                     step_size_months = NULL)

   # metric = reporting rate per active patients; unit = weeks --> step_size_weeks will be used
   res_2 <- get_x_axis(dataset = df,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1, # == weeks
                       step_size_days = NULL,
                       step_size_weeks = 2L,
                       step_size_months = NULL)

   # metric = reporting rate per active patients; unit = months --> step_size_months will be used
   res_3 <- get_x_axis(dataset = df,
                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2, # == months
                       step_size_days = NULL,
                       step_size_weeks = NULL,
                       step_size_months = 1L)

   expect_equal(res$ticktext, c(1))
   expect_equal(res_2$ticktext, c(-2, 1, 2))
   expect_equal(res_3$ticktext, c(-2, -1, 1, 2))

   # metric = reporting rate per active patients; unit = months --> step_size_months will be used but it is NULL here
   res_wrong <- get_x_axis(dataset = df,
                           metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                           time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                           unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2, # == months
                           step_size_days = NULL,
                           step_size_weeks = 2L,
                           step_size_months = NULL)
   expect_equal(res_wrong$ticktext, integer(0))
})

test_that("get_x_axis() returns empty breaks and ticktext if the input dataset was empty", {
   df <- data.frame(time = numeric(), time_for_plot = numeric())
   res <- get_x_axis(dataset = df,
                     metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # == cumulative rate
                     time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == 'By study day'
                     unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                     step_size_days = 50L,
                     step_size_weeks = 3L,
                     step_size_months = 1L)
   expect_equal(res$breaks, numeric(0))
   expect_equal(res$ticktext, numeric(0))

   df <- data.frame(time = as.Date(c(NA, NA, NA)), time_for_plot = as.Date(c(NA, NA, NA)))
   expect_error(
      get_x_axis(dataset = df,
                 metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # == cumulative rate
                 time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == 'By study date'
                 unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                 step_size_days = 50L,
                 step_size_weeks = 3L,
                 step_size_months = 1L)
   )
})



# Function "get_x_axis_label()"
test_that("get_x_axis_label() returns the correct Time label for the x-Axis" |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$adaptive_titles), {
   expect_equal(get_x_axis_label("Cumulative rate per total exposure time", "By study date", "Week"), "Date")
   expect_equal(get_x_axis_label("Cumulative rate per total exposure time", "By study date", "Month"), "Date")
   expect_equal(get_x_axis_label("Reporting rate per active patients", "By study date", "Week"), "Date")
   expect_equal(get_x_axis_label("Reporting rate per active patients", "By study date", "Month"), "Date")

   expect_equal(get_x_axis_label("Cumulative rate per total exposure time", "By study day", "Month"), "Days")
   expect_equal(get_x_axis_label("Cumulative rate per total exposure time", "By study day", "Week"), "Days")

   expect_equal(get_x_axis_label("Reporting rate per active patients", "By study day", "Week"), "Weeks")
   expect_equal(get_x_axis_label("Reporting rate per active patients", "By study day", "Month"), "Months")
})



# Function "get_indices_for_tickvals()"
test_that("get_indices_for_tickval() returns the correct indices when a step size is passed to the function" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$axis_handling$no_zero,
                   specs$app_creator_settings$step_size
                )
             ), {
   tick_text <- c(-108:-1, 1:112) # without 0
   res <- get_indices_for_tickvals(
      step_size = 20L,
      tick_text = tick_text
   )

   tick_text <- 1:100
   res_2 <- get_indices_for_tickvals(
      step_size = 20L,
      tick_text = tick_text
   )

   tick_text <- rep(c(-10:-1, 1:11), each = 3) # case if there was for example a grouping_var with 3 levels to it
   res_3 <- get_indices_for_tickvals(
      step_size = 3L,
      tick_text = tick_text
   )

   expect_equal(res, c(9, 29, 49, 69, 89, 109, 128, 148, 168, 188, 208))
   expect_equal(res_2, c(1, 20, 40, 60, 80, 100))
   expect_equal(res_3, c(4, 5, 6, 13, 14, 15, 22, 23, 24, 31, 32, 33, 37, 38, 39, 46, 47, 48, 55, 56, 57))
})

test_that("get_indices_for_tickval() throws an error if the passed step size is a string", {
   tick_text <- c(-108:-1, 1:112)
   df <- data.frame()

   expect_error(
      get_indices_for_tickvals(
         step_size = "3",
         tick_text = tick_text
      ),
      class = "simpleError"
   )
})



# Function "get_time_for_plot()"
test_that("get_time_for_plot() creates original time column as time_for_plot column when 'By study date' is
          selected" |>
         vdoc[["add_spec"]](specs$metric_calculation$timetype_selection), {
   df <- data.frame(time = as.Date(c("2025-01-01", "2025-01-02")))
   res <- get_time_for_plot(dataset = df, time_selection = "By study date")
   expect_equal(res$time_for_plot, df$time)
})

test_that("get_time_for_plot() adjusts time correctly when 'By study day' is selected" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$axis_handling$no_zero,
                specs$metric_calculation$timetype_selection
             )
          ), {
   df <- data.frame(time = as.Date(c(-2, -1, 0, 1, 2)))
   res <- get_time_for_plot(dataset = df, time_selection = "By study day")
   expect_equal(res$time_for_plot, c(-2, -1, 0, 1))
   expect_equal(res$time, c(-2, -1, 1, 2))
})

test_that("get_time_for_plot() adjusts time correctly when 'By study day' is selected for negative values only" |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$no_zero), {
   df <- data.frame(time = as.Date(c(-5, -4, -3, -2, -1)))
   res <- get_time_for_plot(dataset = df, time_selection = "By study day")
   expect_equal(res$time_for_plot, df$time)
})

test_that("get_time_for_plot() is filtering out the rows with time = 0 correctly." |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$no_zero), {
   df <- data.frame(time = c(0, 0, 0))
   res <- get_time_for_plot(dataset = df, time_selection = "By study day")
   expect_equal(nrow(res), 0)
})

test_that("get_time_for_plot() doesn't return the column 'time' with a 0 included." |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$no_zero), {
   df <- data.frame(time = c(-1, 0, 1))
   res <- get_time_for_plot(df, "By study day")

   df <- data.frame(time = c(-3, -2, -1))
   res_2 <- get_time_for_plot(dataset = df, time_selection = "By study day")

   df <- data.frame(time = c(0, 0, 0))
   res_3 <- get_time_for_plot(dataset = df, time_selection = "By study day")

   df <- data.frame(time = c(1, 2, 3))
   res_4 <- get_time_for_plot(dataset = df, time_selection = "By study day")

   expect_false(any(res$time == 0))
   expect_false(any(res_2$time == 0))
   expect_false(any(res_3$time == 0))
   expect_false(any(res_4$time == 0))
})



# function get_x_axis_text_size()
test_that("get_x_axis_text_size() returns the correct numeric" |>
         vdoc[["add_spec"]](specs$plot_creation$axis_handling$adaptive_x_text_size), {
   breaks <- 1:14
   res <- get_x_axis_text_size(breaks)
   expect_equal(res, 8)

   breaks <- 1:24
   res_2 <- get_x_axis_text_size(breaks)
   expect_equal(res_2, 7)

   breaks <- 1:34
   res_3 <- get_x_axis_text_size(breaks)
   expect_equal(res_3, 6)

   breaks <- 1:35
   res_4 <- get_x_axis_text_size(breaks)
   expect_equal(res_4, 4)

   expect_false(res == res_4)
})



# function get_opacity()
test_that("get_opacity() returns 1 when amount_levels is 0" |>
         vdoc[["add_spec"]](specs$plot_creation$highlight), {
   expect_equal(get_opacity(0), 1)
})

test_that("get_opacity() returns 0.5 when amount_levels is 1" |>
         vdoc[["add_spec"]](specs$plot_creation$highlight), {
   expect_equal(get_opacity(1), 0.5)
})

test_that("get_opacity() returns correct fractional value for level amount > 1" |>
         vdoc[["add_spec"]](specs$plot_creation$highlight), {
   expect_equal(get_opacity(2), 0.5)
   expect_equal(get_opacity(3), 1 / 3)
   expect_equal(get_opacity(10), 0.1)
   expect_equal(get_opacity(100), 0.01)
})



# function get_legend_text()
df_legend <- data.frame(
   ARM = as.factor(c("a", "b")),
   COUNTRY = c(1, 2),
   SITEID = c(1, 2)
)
attr(df_legend[["ARM"]], "label") <- "Description of planned Arm"
attr(df_legend[["COUNTRY"]], "label") <- 10

test_that("get_legend_text() returns the label of the grouping_var if there is one" |>
         vdoc[["add_spec"]](specs$plot_creation$legend), {
   res <- get_legend_text(df_legend, "ARM")
   expect_equal(res, "Description of planned Arm")

   # works also for non character labels:
   res_non_character <- get_legend_text(df_legend, "COUNTRY")
   expect_equal(res_non_character, "10")
})


test_that("get_legend_text() returns the name of the grouping variable itself it there is no label" |>
         vdoc[["add_spec"]](specs$plot_creation$legend), {
   # no label was assigned to the column SITEID
   res <- get_legend_text(df_legend, "SITEID")
   expect_equal(res, "SITEID")

   # assigning NULL
   attr(df_legend[["SITEID"]], "label") <- NULL
   res_null <- get_legend_text(df_legend, "SITEID")
   expect_equal(res_null, "SITEID")

   # assigning NA
   attr(df_legend[["SITEID"]], "label") <- NA
   res_na <- get_legend_text(df_legend, "SITEID")
   expect_equal(res_na, "SITEID")

   # assigning empty string
   attr(df_legend[["SITEID"]], "label") <- ""
   res_empty_string <- get_legend_text(df_legend, "SITEID")
   expect_equal(res_empty_string, "SITEID")
})



# Function get_columns_with_labels()
test_that("get_columns_with_labels returns column names with and without labels correctly" |>
             vdoc[["add_spec"]](specs$plot_creation$legend), {
   dataset <- data.frame(
      age = 1:3,
      sex = c("m", "f", "m"),
      stringsAsFactors = FALSE
   )

   attr(dataset$age, "label") <- "Age in years"
   cols <- c("age", "sex")
   result <- get_columns_with_labels(dataset, cols)
   expect_equal(names(result), c("age  [Age in years]", ""))

   # columnnames didnt change
   expect_equal(unname(result), cols)
})
