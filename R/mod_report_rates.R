REPORT_RATES <- poc(
  ID = poc(
    METRIC_BUTTONS = "metric_id",
    GROUP_VAR = "grouping-group_var",
    LEVELS_DROPDOWN = "selected_levels",
    TIMETYPE_BUTTONS = "type_id",
    BINSIZE_BUTTONS = "unit_id",
    PLOT = "plot_id",
    UNGROUPED_CHECKBOX = "ungrouped",
    X_AXIS_DATE_MIN = "x_axis_date_min",
    X_AXIS_DATE_MAX = "x_axis_date_max",
    X_AXIS_NUMERIC_MIN = "x_axis_numeric_min",
    X_AXIS_NUMERIC_MAX = "x_axis_numeric_max",
    Y_AXIS_MIN = "y_axis_min",
    Y_AXIS_MAX = "y_axis_max",
    RESET_AXIS_LIMITS = "reset_axis_limits"
  ),
  LBL = poc(
    METRIC_BUTTONS = "Metric type:",
    GROUP_DROPDOWN = "Group by:",
    TIMETYPE_BUTTONS = "Time alignment:",
    BINSIZE_BUTTONS = "Bin size:",
    UNGROUPED_CHECKBOX = "Show ungrouped data",
    AXIS_LIMITS = "Plot zoom",
    X_AXIS_MIN = "X-axis minimum:",
    X_AXIS_MAX = "X-axis maximum:",
    Y_AXIS_MIN = "Y-axis minimum:",
    Y_AXIS_MAX = "Y-axis maximum:",
    RESET_AXIS_LIMITS = "Reset zoom"
  ),
  CHOICES = poc(
    METRIC_BUTTONS_OPT1 = "Cumulative rate per total exposure time",
    METRIC_BUTTONS_OPT2 = "Interval rate per active patient",
    TIMETYPE_BUTTONS_OPT1 = "By study date",
    TIMETYPE_BUTTONS_OPT2 = "By study day",
    BINSIZE_BUTTONS_OPT1 = "Week",
    BINSIZE_BUTTONS_OPT2 = "Month",
    GROUP_NO_SELECTION = "-None-",
    LEVELS_AGGR_SELECTION = "Ungrouped"
  ),
  HEADLINE = poc(
    CALC_SETTINGS = "Calculation settings",
    TIME_SETTINGS = "Time settings",
    GROUP_SETTINGS = "Grouping settings",
    AXIS_LIMITS = "Plot zoom"
  ),
  INFO = poc(
    METRIC_BUTTONS_OPT1 = paste(
      "Indicates the total number of events observed across all subjects, divided by the sum of their individual",
      "observation times up to a given time point. This metric reflects the overall event frequency relative to",
      "cumulative exposure. \n"),
    METRIC_BUTTONS_OPT2 = paste(
      "Measures the number of reported events within a defined time interval (e.g., week or month), relative to the",
      "number of patients actively under observation at the end of that interval. This metric reflects the event",
      "frequency among active patients during the selected interval. The interval can be adjusted by selecting this",
      "rate type and specifying a bin size.\n"),
    TIMETYPE_BUTTONS_OPT1 = paste(
      "This view is calendar based. It aligns all events by their actual calendar dates.\n It",
      "allows you to view trends and rates over real-world time, regardless of when each subject entered the study."),
    TIMETYPE_BUTTONS_OPT2 = paste0(
      "This view is subject based. It aligns events relative to each subject's individual study timeline.\n",
      "It allows you to view trends and rates based on study progression (e.g. Day 1, Day 30) independent of calendar",
      "time. \n")
  ),
  DEFAULTS = poc(
    SUBJID = "USUBJID",
    TT_DECIMAL_PLACES = 3L,
    DISPO_VAR = "DSDECOD",
    DISPO_DATE_VAR = "DSSTDTC",
    DISPO_DAY_VAR = "DSSTDY",
    DISPO_ENTRY_VALS = c("RANDOMIZED"),
    DISPO_EXIT_VALS = c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH"),
    AE_DATE_VAR = "AESTDTC",
    AE_DAY_VAR = "AESTDY",
    STEPSIZE_DAYS = 50L,
    STEPSIZE_WEEKS = 2L,
    STEPSIZE_MONTHS = 1L
  ),
  PALETTES = poc(
    EXTENDED = c(
      "#000000",
      "#004949",
      "#009292",
      "#FF6DB6",
      "#FFB6DB",
      "#490092",
      "#006DDB",
      "#B66DFF",
      "#6DB6FF",
      "#B6DBFF",
      "#920000",
      "#924900",
      "#DB6D00",
      "#24FF24",
      "#FFFF6D"
    )
  )
)



#' Create User interface for \pkg{dv.reportrates} module
#'
#' @param module_id `[character(1)]`
#'
#' A unique ID string to create a namespace. Must match the ID provided to \code{report_rates_server()}
#'
#' @return A shiny UI.
#'
#' @keywords developers
#'
#' @export
report_rates_ui <- function(module_id) {
  checkmate::assert_string(module_id, min.chars = 1)

  ns <- shiny::NS(module_id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::tags$h4(REPORT_RATES$HEADLINE$CALC_SETTINGS),
      shiny::radioButtons(
        inputId = ns(REPORT_RATES$ID$METRIC_BUTTONS),
        label = REPORT_RATES$LBL$METRIC_BUTTONS,
        choiceNames = list(
          shiny::tagList(
            REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
            shiny::icon("circle-info",
                        title = paste0(REPORT_RATES$INFO$METRIC_BUTTONS_OPT1),
                        style = "color: grey")
          ),
          shiny::tagList(
            REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
            shiny::icon("circle-info",
                        title = paste0(REPORT_RATES$INFO$METRIC_BUTTONS_OPT2),
                        style = "color: grey")
          )
        ),
        choiceValues = c(REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2),
        selected = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1
      ),
      shiny::tags$hr(style = "border-top: 1px solid #aaa; margin: 15px 0;"),
      shiny::tags$h4(REPORT_RATES$HEADLINE$GROUP_SETTINGS),
      shiny::selectInput(
        inputId = ns(REPORT_RATES$ID$GROUP_VAR),
        label = REPORT_RATES$LBL$GROUP_DROPDOWN,
        choices = NULL,
        selected = NULL
      ),
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] != '%s'", ns(REPORT_RATES$ID$GROUP_VAR), REPORT_RATES$CHOICES$GROUP_NO_SELECTION
        ),
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("selected_levels"),
            label = "Select groups to display in the plot:",
            choices = NULL,
            selected = NULL,
            multiple = TRUE
          ),
          shiny::checkboxInput(
            inputId = ns(REPORT_RATES$ID$UNGROUPED_CHECKBOX),
            label = REPORT_RATES$LBL$UNGROUPED_CHECKBOX,
            value = FALSE
          )
        )
      ),
      shiny::tags$hr(style = "border-top: 1px solid #aaa; margin: 15px 0;"),
      shiny::tags$h4(REPORT_RATES$HEADLINE$TIME_SETTINGS),


      shiny::radioButtons(
        inputId = ns(REPORT_RATES$ID$TIMETYPE_BUTTONS),
        label = REPORT_RATES$LBL$TIMETYPE_BUTTONS,
        choiceNames = list(
          shiny::tagList(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                         shiny::icon("circle-info",
                                     title = REPORT_RATES$INFO$TIMETYPE_BUTTONS_OPT1,
                                     style = "color: grey")),
          shiny::tagList(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                         shiny::icon("circle-info",
                                     title = REPORT_RATES$INFO$TIMETYPE_BUTTONS_OPT2,
                                     style = "color: grey"))
        ),
        choiceValues = c(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                         REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2)
      )
      ,
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == '%s'",
          ns(REPORT_RATES$ID$METRIC_BUTTONS),
          REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2
        ),
        shiny::column(
          width = 6,
          shiny::radioButtons(
            inputId = ns(REPORT_RATES$ID$BINSIZE_BUTTONS),
            label = REPORT_RATES$LBL$BINSIZE_BUTTONS,
            choices = c(REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1, REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
          )
        )
      )

      ,

      shiny::tags$hr(style = "border-top: 1px solid #aaa; margin: 15px 0;"),
      shiny::tags$h4(REPORT_RATES$HEADLINE$AXIS_LIMITS),

      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == '%s'",
          ns(REPORT_RATES$ID$TIMETYPE_BUTTONS),
          REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyWidgets::airDatepickerInput(
              inputId = ns(REPORT_RATES$ID$X_AXIS_DATE_MIN),
              label = REPORT_RATES$LBL$X_AXIS_MIN,
              value = NULL,
              dateFormat = "yyyy-MM-dd",
              clearButton = TRUE
            )
          ),
          shiny::column(
            width = 6,
            shinyWidgets::airDatepickerInput(
              inputId = ns(REPORT_RATES$ID$X_AXIS_DATE_MAX),
              label = REPORT_RATES$LBL$X_AXIS_MAX,
              value = NULL,
              dateFormat = "yyyy-MM-dd",
              clearButton = TRUE
            )
          )
        )
      ),

      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == '%s'",
          ns(REPORT_RATES$ID$TIMETYPE_BUTTONS),
          REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              inputId = ns(REPORT_RATES$ID$X_AXIS_NUMERIC_MIN),
              label = REPORT_RATES$LBL$X_AXIS_MIN,
              value = NULL
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              inputId = ns(REPORT_RATES$ID$X_AXIS_NUMERIC_MAX),
              label = REPORT_RATES$LBL$X_AXIS_MAX,
              value = NULL
            )
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns(REPORT_RATES$ID$Y_AXIS_MIN),
            label = REPORT_RATES$LBL$Y_AXIS_MIN,
            value = NULL
          )
        ),
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns(REPORT_RATES$ID$Y_AXIS_MAX),
            label = REPORT_RATES$LBL$Y_AXIS_MAX,
            value = NULL
          )
        )
      ),

      shiny::actionButton(
        inputId = ns(REPORT_RATES$ID$RESET_AXIS_LIMITS),
        label = REPORT_RATES$LBL$RESET_AXIS_LIMITS,
        icon = shiny::icon("rotate-left"),
        width = "100%"
      )

    ),
    shiny::mainPanel(
      shiny::div(
        style = "height:20vh;",
        ggiraph::girafeOutput(outputId = ns(REPORT_RATES$ID$PLOT), width = "100%", height = "80%")
      )
    )
  )
}




#' Create server for \pkg{dv.reportrate} module
#'
#' @param module_id `[character(1)]`
#'
#' A unique ID string to create a namespace. Must match the ID provided to \code{report_rates_ui()}
#'
#' @param dataset_list `[shiny::reactive(list(data.frame))]`
#'
#' A reactive list of named datasets. Usually obtained from module manager.
#'
#' @inheritParams mod_report_rates
#'
#' @keywords developers
#'
#' @export
report_rates_server <- function(module_id,
                                dataset_list,
                                subjid_var = REPORT_RATES$DEFAULTS$SUBJID,
                                tooltip_decimal_places = REPORT_RATES$DEFAULTS$TT_DECIMAL_PLACES,
                                disposition_events,
                                adverse_events,
                                grouping_vars = list(choices = NULL, default_choice = NULL),
                                x_step_size_list = list(days = REPORT_RATES$DEFAULTS$STEPSIZE_DAYS,
                                                        weeks = REPORT_RATES$DEFAULTS$STEPSIZE_WEEKS,
                                                        months = REPORT_RATES$DEFAULTS$STEPSIZE_MONTHS)
) {

  input_args <- list(module_id = module_id,
                     dataset_list = dataset_list,
                     subjid_var = subjid_var,
                     tooltip_decimal_places = tooltip_decimal_places,
                     disposition_events = disposition_events,
                     adverse_events = adverse_events,
                     grouping_vars = grouping_vars,
                     x_step_size_list = x_step_size_list)
  input_args <- validate_and_fill(input_args = input_args)

  shiny::moduleServer(module_id, function(input, output, session) {

    # Helper to reset all user-defined axis limits.
    reset_axis_limits <- function() {

      shinyWidgets::updateAirDateInput(
        session,
        REPORT_RATES$ID$X_AXIS_DATE_MIN,
        value = NA
      )

      shinyWidgets::updateAirDateInput(
        session,
        REPORT_RATES$ID$X_AXIS_DATE_MAX,
        value = NA
      )

      shiny::updateNumericInput(
        session,
        REPORT_RATES$ID$X_AXIS_NUMERIC_MIN,
        value = NA
      )

      shiny::updateNumericInput(
        session,
        REPORT_RATES$ID$X_AXIS_NUMERIC_MAX,
        value = NA
      )

      shiny::updateNumericInput(
        session,
        REPORT_RATES$ID$Y_AXIS_MIN,
        value = NA
      )

      shiny::updateNumericInput(
        session,
        REPORT_RATES$ID$Y_AXIS_MAX,
        value = NA
      )
    }

    # Reset all user-defined axis limits
    # - at app startup
    # - when changing time alignment
    # - when changing metric type
    # - when clicking the reset button
    shiny::observe({
      reset_axis_limits()
    }) |>
      shiny::bindEvent(
        input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
        input[[REPORT_RATES$ID$METRIC_BUTTONS]],
        input[[REPORT_RATES$ID$RESET_AXIS_LIMITS]],
        ignoreInit = FALSE
      )

    subjid_var <- input_args$subjid_var

    dispo_event_col <- input_args$disposition_events[["event_var"]]
    dispo_date_col  <- input_args$disposition_events[["date_var"]]
    dispo_day_col <- input_args$disposition_events[["day_var"]]
    dispo_entry_vals <- input_args$disposition_events[["entry_vals"]]
    dispo_exit_vals <- input_args$disposition_events[["exit_vals"]]

    ae_date_col <- input_args$adverse_events[["date_var"]]
    ae_day_col <- input_args$adverse_events[["day_var"]]

    grouping <- input_args$grouping_vars
    # storing the default choice in a reactiveVal because it needs to be updated to NULL inside the observer for the
    # grouping dropdown after it was used once.
    group_default_choice <- shiny::reactiveVal(input_args$grouping$default_choice)

    # ------------ BOOKMARKING -------------
    # Store the selected group and sel. levels as reactive values in order to restore them correctly after bookmarking
    r_values <- shiny::reactiveValues(
      # using ...$restoring flags to only update the UI with the bookmarked values once
      grouping = list(val = REPORT_RATES$CHOICES$GROUP_NO_SELECTION, restoring = FALSE),
      levels = list(val = NULL, restoring = FALSE),
    )

    # To make bookmarking work also for r_values
    shiny::onRestore(function(state) {
      if (length(state$input) > 0) { # makes sure that the default_vars are displayed at app launch with SSO
        r_values$grouping$val <- state$input[[REPORT_RATES$ID$GROUP_VAR]]
        r_values$grouping$restoring <- TRUE
        r_values$levels$val <- state$input[[REPORT_RATES$ID$LEVELS_DROPDOWN]]
        r_values$levels$restoring <- TRUE
      }
    })
    # -------------------------------------


    # Dataset validation
    v_dataset_list <- shiny::reactive({
      shiny::req(dataset_list())
      checkmate::assert_list(dataset_list(), types = "data.frame", names = "named", len = 3)
      validate_dm_dataset(dataset = dataset_list()$dm,
                          subjid_var = subjid_var,
                          grouping_choices = grouping$choices,
                          default_choice = shiny::isolate(group_default_choice()))
      validate_ds_dataset(dataset = dataset_list()$ds, disposition_events = disposition_events)
      validate_ae_dataset(dataset = dataset_list()$ae, adverse_events = adverse_events)
      dataset_list()
    })


    dm <- shiny::reactive({
      shiny::req(v_dataset_list())
      v_dataset_list()$dm
    })

    ae_prepared <- shiny::reactive({
      shiny::req(v_dataset_list())
      ae <- v_dataset_list()$ae
      prepare_ae_data(dataset = ae,
                      subjid_var = subjid_var,
                      date_var = ae_date_col,
                      day_var = ae_day_col)
    })

    ds_prepared <- shiny::reactive({
      shiny::req(v_dataset_list())
      ds <- v_dataset_list()$ds
      prepare_ds_data(dataset = ds,
                      subjid_var = subjid_var,
                      event_var = dispo_event_col,
                      entry_terms = dispo_entry_vals,
                      exit_terms = dispo_exit_vals,
                      date_var = dispo_date_col,
                      day_var = dispo_day_col)
    })


    available_grouping_choices <- shiny::reactive({
      shiny::req(dm())
      dm <- dm()
      valid_vars <- names(dm)[sapply(dm, function(col) is.factor(col))]
      if (!is.null(grouping$choices)) { # pre selection of choices if specified by the app creator
        valid_vars <- intersect(valid_vars, grouping$choices)
      }
      valid_vars <- c(REPORT_RATES$CHOICES$GROUP_NO_SELECTION, valid_vars)
      valid_names <- get_columns_with_labels(dataset = dm, columns = valid_vars)
      valid_names
    })



    # observe for updating the grouping dropdown
    trigger_grouping <- shiny::reactiveVal(0L)
    current_sel_grouping_var <- shiny::reactiveVal(NULL)
    shiny::observe({
      shiny::req(available_grouping_choices())
      sel <- current_sel_grouping_var()

      if (!is.null(group_default_choice())) { # using default choice, if there was one specified by app creator
        sel <- group_default_choice()
        group_default_choice(NULL) # so that the default choice is only set when first starting the app
      }
      sel <- validate_grouping_selection(dm = shiny::isolate(dm()), selected_group = sel)

      # bookmarking case
      if (r_values$grouping$restoring == TRUE) {
        sel <- r_values$grouping$val
        r_values$grouping$restoring <- FALSE
      }

      if (identical(current_sel_grouping_var(), sel)) { # if it's still valid...
        # ... trigger the selected_grouping_var reactive anyways so that the plot is redrawn. This is important for
        # the case when the dm / available choices get updated but it doesnt lead to a change of the grouping var.
        # With the trigger_grouping the reactive chain is still being continued anyways in such cases.
        trigger_grouping(shiny::isolate(trigger_grouping() + 1L))
      }

      shiny::updateSelectInput(
        session,
        inputId = REPORT_RATES$ID$GROUP_VAR,
        choices = available_grouping_choices(),
        selected = sel
      )
    }) |> shiny::bindEvent(available_grouping_choices())



    selected_grouping_var <- shiny::reactive({
      trigger_grouping()
      val <- shiny::req(input[[REPORT_RATES$ID$GROUP_VAR]])
      val <- validate_grouping_selection(shiny::isolate(dm()), val)
      current_sel_grouping_var(val)
      val
    }) |> shiny::bindEvent(list(trigger_grouping(), input[[REPORT_RATES$ID$GROUP_VAR]]), ignoreInit = TRUE)



    available_lvls <- shiny::reactive({
      dm_df <- shiny::req(dm())
      grouping_var <- shiny::req(selected_grouping_var())
      if (grouping_var == REPORT_RATES$CHOICES$GROUP_NO_SELECTION) {
        vals <- character(0)
      } else {
        vals <- as.character(unique(dm_df[[grouping_var]]))
      }
      vals
    })



    # observe for updating the levels dropdown and keeping the available choices for selection valid
    old_group <- shiny::reactiveVal(character(0)) # to check if the grouping variable has changed.
    current_sel_lvls <- shiny::reactiveVal(NULL)
    shiny::observe({
      if (shiny::isolate(selected_grouping_var()) != REPORT_RATES$CHOICES$GROUP_NO_SELECTION &&
          nrow(shiny::isolate(dm())) > 0) {
        shiny::req(available_lvls())
      }

      new_sel <- NULL
      # if the grouping variable has changed update the selection to all current available levels
      if (is.null(old_group()) || !identical(old_group(), shiny::isolate(selected_grouping_var()))) {
        new_sel <- available_lvls()
        old_group(shiny::isolate(selected_grouping_var()))
      } else {
        # some levels may have dissapeared through a change in dm. remove them and use the remaining selected levels
        new_sel <- intersect(current_sel_lvls(), available_lvls())
      }

      # bookmarking case
      if (r_values$levels$restoring == TRUE) {
        new_sel <- r_values$levels$val
        r_values$levels$restoring <- FALSE
      }

      if (setequal(current_sel_lvls(), new_sel)) { # if the new selection is the same as the old
        # trigger the selected_levels reactive even if the levels selection of the UI itself didnt change
        # This is important in the cases:
        #  - just the dm dataset changed through global filtering
        #  -  the selected grouping variable changed (e.g. ARM -> ACTARM because it has the same level names)
        # without leading to a change in the levels selection. The trigger should only be activated in those cases
        # and not when a change in the dm or the selected grouping variable led to a change in the levels selection.
        # Like that the create_plot function only gets called once in every scenario.
        trigger_levels(shiny::isolate(trigger_levels() + 1L))
      }

      shiny::updateSelectInput(
        session = session,
        inputId = REPORT_RATES$ID$LEVELS_DROPDOWN,
        choices = available_lvls(),
        selected = new_sel
      )
    }) |> shiny::bindEvent(available_lvls())



    trigger_levels <- shiny::reactiveVal(0L)
    sel_levels <- shiny::reactive({
      trigger_levels()

      inp <- input[[REPORT_RATES$ID$LEVELS_DROPDOWN]]
      if (is.null(inp)) {
        inp <- character(0)
      }
      current_sel_lvls(inp)
      inp
    }) |> shiny::bindEvent(list(trigger_levels(), input[[REPORT_RATES$ID$LEVELS_DROPDOWN]]), ignoreInit = TRUE)



    ungrouped_checkbox <- shiny::reactive({
      val <- input[[REPORT_RATES$ID$UNGROUPED_CHECKBOX]]
    })



    # #color palette for the levels in the plot. A new palette will only be created if a switch to a better
    # (more colorblind friendly) palette is possible and also if there are new levels in available_lvls()
    palette_stored <- shiny::reactiveVal(NULL)
    color_palette <- shiny::reactive({
      if (palette_refresh_check(shiny::isolate(palette_stored()), available_lvls())) {
        new_palette <- generate_palette(available_lvls())
        palette_stored(new_palette)
      }
      shiny::isolate(palette_stored())
    }) |> shiny::bindEvent(available_lvls())



    # observe for updating the ungrouped_checkbox if the grouping variable was set to -None-
    shiny::observe({
      if (shiny::isolate(selected_grouping_var()) == REPORT_RATES$CHOICES$GROUP_NO_SELECTION) {
        shiny::updateCheckboxInput(
          session,
          REPORT_RATES$ID$UNGROUPED_CHECKBOX,
          value = FALSE
        )
      }
    }) |> shiny::bindEvent(selected_grouping_var())



    output[[REPORT_RATES$ID$PLOT]] <- ggiraph::renderGirafe({
      #show message "No data available" if one of the df's has 0 rows
      # or when no levels are selected and the checkbox for the ungrouped data is FALSE
      validate_df_rows(dataset_list = v_dataset_list(),
                       selected_levels = sel_levels(),
                       ungrouped_checkbox = ungrouped_checkbox(),
                       selected_group = selected_grouping_var())


      # validate axis limits
      shiny::validate(
        validate_axis_limits(
          min_value = if (identical(
            input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
            REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1
          )) {
            input[[REPORT_RATES$ID$X_AXIS_DATE_MIN]]
          } else {
            input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MIN]]
          },
          max_value = if (identical(
            input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
            REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1
          )) {
            input[[REPORT_RATES$ID$X_AXIS_DATE_MAX]]
          } else {
            input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MAX]]
          },
          axis_name = "X"
        ),
        validate_axis_limits(
          min_value = input[[REPORT_RATES$ID$Y_AXIS_MIN]],
          max_value = input[[REPORT_RATES$ID$Y_AXIS_MAX]],
          axis_name = "Y"
        )
      )




      plt <- create_plot(dm = dm(),
                         ae = ae_prepared(),
                         ds = ds_prepared(),
                         subjid_var = subjid_var,
                         grouping_var = selected_grouping_var(),
                         selected_levels = sel_levels(),
                         metric_selection = input[[REPORT_RATES$ID$METRIC_BUTTONS]],
                         time_selection = input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
                         unit_selection = input[[REPORT_RATES$ID$BINSIZE_BUTTONS]],
                         tooltip_decimal_places = input_args$tooltip_decimal_places,
                         x_step_size_list = input_args$x_step_size_list,
                         color_palette = color_palette(),
                         show_ungrouped = ungrouped_checkbox())



      # zoom via coord_cartesian
      plt <- apply_axis_limits(
        plt = plt,
        x_min = if (identical(
          input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
          REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1
        )) {
          input[[REPORT_RATES$ID$X_AXIS_DATE_MIN]]
        } else {
          input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MIN]]
        },
        x_max = if (identical(
          input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
          REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1
        )) {
          input[[REPORT_RATES$ID$X_AXIS_DATE_MAX]]
        } else {
          input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MAX]]
        },
        y_min = input[[REPORT_RATES$ID$Y_AXIS_MIN]],
        y_max = input[[REPORT_RATES$ID$Y_AXIS_MAX]],
        time_selection = input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]]
      )






      plt <- convert_to_girafe(plt, sel_levels())
      plt
    }) |>
      # Upstream changes in dm(), ae_prepared(), ds_prepared(), color_palette() and selected_grouping_var() always
      # flow through the reactive chain and invalidate sel_levels(), so renderGirafe only binds to sel_levels() and
      # recomputes exactly once per full update.
      shiny::bindEvent(
        sel_levels(),
        input[[REPORT_RATES$ID$METRIC_BUTTONS]],
        input[[REPORT_RATES$ID$TIMETYPE_BUTTONS]],
        input[[REPORT_RATES$ID$BINSIZE_BUTTONS]],
        ungrouped_checkbox(),
        input[[REPORT_RATES$ID$X_AXIS_DATE_MIN]],
        input[[REPORT_RATES$ID$X_AXIS_DATE_MAX]],
        input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MIN]],
        input[[REPORT_RATES$ID$X_AXIS_NUMERIC_MAX]],
        input[[REPORT_RATES$ID$Y_AXIS_MIN]],
        input[[REPORT_RATES$ID$Y_AXIS_MAX]]
      )



    # export for app tests
    shiny::exportTestValues(
      dm = { dm() },
      available_grouping_choices = { available_grouping_choices() },
      selected_group = { selected_grouping_var() },
      available_lvls = { available_lvls() },
      sel_levels = { sel_levels() },
      ungrouped_checkbox = { ungrouped_checkbox() },
      color_palette = { color_palette() },
      group_default_choice = { group_default_choice() }
    )


  })
}



#' Combining UI and server within DaVinci module
#'
#' This module shows plots of reporting rates of Adverse Events in clinical trials over time. There are 2 different
#' metrics that can be selected:
#' * cumulative rate per total exposure time: Indicates the total number of events observed across all subjects,
#' divided by the sum of their individual observation times up to a given time point. This metric reflects the
#' overall event frequency relative to cumulative exposure.
#' * reporting rates per active patients: Measures the number of reported events within a defined time interval
#' (e.g., week or month), relative to the number of patients actively under observation at the end of that interval.
#' The interval can be adjusted by selecting this rate type and specifying a bin size.
#'
#' @param module_id `[character(1)]`
#'
#' A unique module ID to create a namespace.
#' @param dm_dataset_name `[character(1)]`
#'
#' Name of the Demographics dataset. Defaults to "dm".
#' @param ds_dataset_name `[character(1)]`
#'
#' Name of the Disposition dataset. Defaults to "ds".
#' @param ae_dataset_name `[character(1)]`
#'
#' Name of the Adverse Event dataset. Defaults to "ae".
#' @param subjid_var `[character(1)]`
#'
#' Character name of the unique subject identifier column in all datasets
#' (default is USUBJID). Must be a single value.
#' @param tooltip_decimal_places `[integer(1)]`
#'
#' Number of decimal places to round the value shown in the hovertext. Defaults to 3L.
#' @param disposition_events `[list]`
#'
#' Named list specifying variables and values used to define entry and exit events in the disposition dataset.
#' Composed of:
#' * event_var `[character(1)]`
#' Name of the variable that contains the description of the event. Defaults to "DSDECOD".
#' * date_var `[character(1)]`
#' Name of the variable containing the date of the event. Defaults to "DSSTDTC".
#' * day_var `[character(1)]`
#' Name of the variable containing the study day of the event. Defaults to "DSSTDY".
#' * entry_vals `[character(1+)]`
#' Character vector of values defining which values out of var should be entry events. Defaults to c("RANDOMIZED").
#' * exit_vals `[character(1+)]`
#' Character vector of values defining which values out of var should be exit events.
#' Defaults to c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH").
#' @param adverse_events `[list]`
#'
#' Named list specifying the variables that define the date and study day of adverse event occurence.
#' Composed of:
#' * date_var `[character(1)]`
#' Name of the variable containing the date of the event. Defaults to "AESTDTC".
#' * day_var `[character(1)]`
#' Name of the variable containing the study day of the event. Defaults to "AESTDY".
#' @param grouping_vars `[list]`
#'
#' Named list specifying grouping options for the plot.
#' Composed of:
#' * choices `[character(1+) | NULL]`
#' Character vector of variable names out of the demographics dataset, specifying which variables are available for
#' grouping. If provided, only these variables can be selected. Defaults to \code{NULL}, meaning all factor variables
#' from the demographics dataset will be available for grouping.
#' * default_choice `[character(1) | NULL]`
#' Character specifying the default grouping variable to be used when starting the app. Defaults to \code{NULL}, meaning
#' no variable is selected for grouping by default.
#' @param x_step_size_list `[list]`
#'
#' Named list specifying the tick interval (step size) on the x-Axis depending on the time unit.
#' Composed of:
#' * days `[integer(1) | NULL]`
#' Integer value specifying the tick interval in days when the x-axis is displayed in days. If \code{NULL} it defaults
#' to a step size of 50L.
#' * weeks `[integer(1) | NULL]`
#' Integer value specifying the tick interval in weeks when the x-axis is displayed in weeks. If \code{NULL} it defaults
#' to a step size of 2L.
#' * months `[integer(1) | NULL]`
#' Integer value specifying the tick interval in months when the x-axis is displayed in months. If \code{NULL} it
#' defaults to a step size of 1L.
#' @returns A list containing the following elements to be used by the \pkg{modulemanager}:
#' * \code{ui}: A UI function of the \pkg{dv.reportrate} module.
#' * \code{server}: A server function of the \pkg{dv.reportrate} module.
#' * \code{module_id}: A unique identifier.
#' * \code{meta}: metadata.
#' @usage mod_report_rates(
#'    module_id,
#'    dm_dataset_name = "dm",
#'    ds_dataset_name = "ds",
#'    ae_dataset_name = "ae",
#'    subjid_var = "USUBJID",
#'    tooltip_decimal_places = 3L,
#'    disposition_events = list(event_var = "DSDECOD",
#'                              date_var = "DSSTDTC",
#'                              day_var = "DSSTDY",
#'                              entry_vals = c("RANDOMIZED"),
#'                              exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH")),
#'    adverse_events = list(date_var = "AESTDTC",
#'                          day_var = "AESTDY"),
#'    grouping_vars = list(choices = c("ARM", "ACTARM", "SEX", "SITEID"),
#'                         default_choice = NULL),
#'    x_step_size_list = list(days = 50L,
#'                            weeks = 2L,
#'                            months = 1L))
#'
#' @keywords main
#'
#' @export
mod_report_rates <- function(module_id,
                             dm_dataset_name = "dm",
                             ds_dataset_name = "ds",
                             ae_dataset_name = "ae",
                             subjid_var = REPORT_RATES$DEFAULTS$SUBJID,
                             tooltip_decimal_places = REPORT_RATES$DEFAULTS$TT_DECIMAL_PLACES,
                             disposition_events = list(event_var = REPORT_RATES$DEFAULTS$DISPO_VAR,
                                                       date_var = REPORT_RATES$DEFAULTS$DISPO_DATE_VAR,
                                                       day_var = REPORT_RATES$DEFAULTS$DISPO_DAY_VAR,
                                                       entry_vals = REPORT_RATES$DEFAULTS$DISPO_ENTRY_VALS,
                                                       exit_vals = REPORT_RATES$DEFAULTS$DISPO_EXIT_VALS),
                             adverse_events = list(date_var = REPORT_RATES$DEFAULTS$AE_DATE_VAR,
                                                   day_var = REPORT_RATES$DEFAULTS$AE_DAY_VAR),
                             grouping_vars = list(choices = NULL,
                                                  default_choice = NULL),
                             x_step_size_list = list(days = REPORT_RATES$DEFAULTS$STEPSIZE_DAYS,
                                                     weeks = REPORT_RATES$DEFAULTS$STEPSIZE_WEEKS,
                                                     months = REPORT_RATES$DEFAULTS$STEPSIZE_MONTHS)) {

  # Check validity of parameters
  checkmate::assert_character(dm_dataset_name, min.chars = 1)
  checkmate::assert_character(ds_dataset_name, min.chars = 1)
  checkmate::assert_character(ae_dataset_name, min.chars = 1)
  mod <- list(
    ui = function(module_id) {
      report_rates_ui(module_id = module_id)
    },
    server = function(afmm) {
      dataset_list <- shiny::reactive({
        afmm$filtered_dataset()[c(dm_dataset_name, ds_dataset_name, ae_dataset_name)]
      })
      report_rates_server(
        module_id = module_id,
        dataset_list = dataset_list,
        subjid_var = subjid_var,
        tooltip_decimal_places = tooltip_decimal_places,
        disposition_events = disposition_events,
        adverse_events = adverse_events,
        grouping_vars = grouping_vars,
        x_step_size_list = x_step_size_list
      )
    },
    module_id = module_id,
    meta = list(dataset_info = list(all = c(dm_dataset_name, ds_dataset_name, ae_dataset_name)))
  )
  return(mod)
 }
