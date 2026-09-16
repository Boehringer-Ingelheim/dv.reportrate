# Initialize test app
app_dir <- test_path("apps/bookmarking_app")



test_that("The app's state is restored correctly after bookmarking" |>
                       vdoc[["add_spec"]](specs$framework_specs$bookmarking), {
   app_bmk <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "bookmarking_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app_bmk$wait_for_idle()

   # setting the inputs
   app_bmk$set_inputs(`reportrate-metric_id` = "Interval rate per active patient")
   app_bmk$set_inputs(`reportrate-type_id` = "By study day")
   app_bmk$set_inputs(`reportrate-unit_id` = "Month")
   app_bmk$set_inputs(`reportrate-grouping-group_var` = "SEX")
   app_bmk$wait_for_idle()
   app_bmk$set_inputs(`reportrate-selected_levels` = c("F"))
   app_bmk$set_inputs(`reportrate-ungrouped` = TRUE)



   # Bookmark
   app_bmk$set_inputs(!!"._bookmark_" := "click")

   # Initialize bookmarked app
   val <- app_bmk$get_value(export = "url")
   app_rst <- shinytest2::AppDriver$new(app_dir = val, name = "test_restoring")
   app_rst$wait_for_idle()

   # Get values and test
   actual <- app_rst$get_values(input = c("reportrate-metric_id", "reportrate-type_id", "reportrate-unit_id",
                                          "reportrate-grouping-group_var", "reportrate-selected_levels",
                                          "reportrate-ungrouped"))
   expected <- list(
      input = list(
         `reportrate-grouping-group_var` = "SEX",
         `reportrate-metric_id` = "Interval rate per active patient",
         `reportrate-selected_levels` = c("F"),
         `reportrate-type_id` = "By study day",
         `reportrate-ungrouped` = TRUE,
         `reportrate-unit_id` = "Month"
      )
   )

   expect_identical(actual, expected)
   app_bmk$stop()
   app_rst$stop()
})
