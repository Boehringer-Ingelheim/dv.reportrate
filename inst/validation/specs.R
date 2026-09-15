# Use a list to declare the specs
specs_list <- list

metric_calculation <- specs_list(
   rate_per_active_patients = "The module can display the reporting rates per active patients.",
   cum_rate_per_total_exp_time = "The module can display the cumulative rate per total exposure time.",
   metric_selection = "The module allows switching from one metric to the other.",
   timetype_selection = "The module allows switching between the time types 'By study date' and 'By study day'.",
   binsize_selection = "The module allows switching between the binsizes 'week' and 'month' for the metric reporting rate per active patients." #nolint
)

plot_creation <- specs_list(
   axis_handling = specs_list(
      no_zero = "The x-axis does not display zero because there is no study day/week/month zero.",
      adaptive_x_text_size = "The x-axis text size adapts to the number of breaks.",
      adaptive_titles = "The titles of the y and x axis are dynamic. The y-title changes depending on the selected metric and the x-title changes depending on the selected metric and time type." #nolint
   ),
   legend = "The legend uses the label of the grouping variable as the title if available, otherwise the variable name.", #nolint
   function_type = "The plot displays the correct function type (line function or step function) according to the selected metric.", #nolint
   grouping_and_levels = specs_list(
      ungrouped = "The plot displays ungrouped data when there is no selected grouping variable or when the show ungrouped checkbox is active.", #nolint
      grouped = "The plot displays the data of the selected levels in the selected grouping variable."
   ),
   hovering = "Hovering over plotted points shows a tooltip displaying additional information.",
   highlight = "Clicking a line dims the other lines.",
   colors = "The plot uses a colorblind friendly palette if possible."
)

app_creator_settings <- specs_list(
   tt_decimal_places = "The app creator can define the number of decimal places shown for ratios in the hovertext.",
   grouping = "The app creator can define, which columns should be available for grouping and whether a default grouping variable should be preselected when the app starts.", #nolint
   step_size = "App creators can specify the step size on the x-axis."
)

framework_specs <- specs_list(
   bookmarking = "The app's state gets restored correctly after bookmarking.",
   filter_and_datasets = "Users can use the module manager's filtering and dataset selection functionality. The module keeps the grouping variable and selected levels consistent and resolves invalid selections." #nolint
)


specs <- specs_list(
   metric_calculation = metric_calculation,
   plot_creation = plot_creation,
   app_creator_settings = app_creator_settings,
   framework_specs = framework_specs
)
