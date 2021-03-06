#' profiling_output2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_output_ui <- function(id, id_input) {
  ns <- NS(id)
  ns_input  <- NS(id_input)
  tagList(
    conditionalPanel(
      condition = "input.y_var == 'risk'",
      ns = ns_input,
      highchartOutput(ns("risk_plot"))
    ),
    conditionalPanel(
      condition = "input.y_var == 'benefit'",
      ns = ns_input,
      highchartOutput(ns("benefit_plot"))
    ),
    conditionalPanel(
      condition = "input.y_var == 'vac'",
      ns = ns_input,
      highchartOutput(ns("vac_plot"))
    )
  )
}

#' profiling_output2 Server Functions
#'
#' @noRd
mod_profiling_output_server <- function(id, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get name of list that contains profile variable
    i_nm <- reactive(
      stringr::str_extract(inputs$vars$x(), "^[^_]+"),
      label = "i_nm()"
    )
    # Get name of profile variable in list
    j_nm <- reactive(
      stringr::str_remove(inputs$vars$x(), "^[^_]+_"),
      label = "j_nm()"
    )

    # Get profile variable
    x_arg <- reactive_x_arg(inputs, i_nm = i_nm, j_nm = j_nm)
    # Create x axis points
    x     <- reactive(seq_profile(x_arg()), label = "seq_profile()")

    # Transform dist_args to appropriate model input
    dist_args_t <- trans_dist_args(inputs$dist_args)
    # Transform x to appropriate model input
    x_t         <- reactive(trans_x(x(), j_nm()), label = "trans_x()")
    # Transform j_nm to match model input
    j_nm_t      <- reactive(trans_j_nm(j_nm()), label = "trans_j_nm()")

    data_risk <- reactive(profiling_prep_risk(
      x(), x_t = x_t(), n = inputs$n$org(), dist_args = dist_args_t,
      i_nm = i_nm(), j_nm = j_nm_t()
    )())

    output$risk_plot <- bindEvent(renderHighchart(
      profiling_plot_risk(data_risk(), i_nm = i_nm(), j_nm = j_nm_t())
    ), inputs$calc(), ignoreNULL = FALSE, label = "profiling_risk_plot()")

    data_benefit <- reactive(profiling_prep_benefit(data_risk()))

    output$benefit_plot <- bindEvent(renderHighchart(
      profiling_plot_benefit(data_benefit(), i_nm = i_nm(), j_nm = j_nm_t())
    ), inputs$calc(), ignoreNULL = FALSE, label = "profiling_benefit_plot()")

    data_vac <- reactive(profiling_prep_vac(
      x(), x_t = x_t(), n = inputs$n$org(), dist_args = dist_args_t,
      i_nm = i_nm(), j_nm = j_nm_t()
    )())

    output$vac_plot <- bindEvent(renderHighchart(
      profiling_plot_vac(data_vac(), i_nm = i_nm(), j_nm = j_nm_t())
    ), inputs$calc(), ignoreNULL = FALSE, label = "profiling_vac_plot()")
  })
}


reactive_x_arg <- function(inputs, i_nm, j_nm) {
  reactive({
    if (i_nm() == "n") {
      inputs$n$org()
    } else {
      inputs$dist_args[[i_nm()]]()[[j_nm()]]
    }
  }, label = "x_arg()")
}


trans_dist_args <- function(dist_args) {
  trans_args <- reactiveValues()

  trans_args$vac <- reactive(purrr::map(
    dist_args$vac(),
    ~ .x * 1e-2
  ), label = "trans_vac()")

  trans_args$inf <- reactive({
    new_nms <- stringr::str_replace(names(dist_args$inf()), "^r_", "p_")
    dist_args$inf() %>%
      purrr::map_if(startsWith(names(.), "r_"), ~ .x * 1e-5) %>%
      magrittr::set_names(new_nms)
  }, label = "trans_inf()")

  trans_args$symp <- reactive(purrr::map(
    dist_args$symp(),
    ~ .x * 1e-2
  ), label = "trans_symp()")

  trans_args$test <- reactive({
    new_nms <- stringr::str_replace(names(dist_args$test()), "^f_", "p_")
    dist_args$test() %>%
      purrr::imap(
        ~ (if (startsWith(.y, "f_")) correct_freq(1 / .x) else .x * 1e-2)
      ) %>%
      magrittr::set_names(new_nms)
  }, label = "trans_test()")

  trans_args$detect <- reactive(purrr::map(
    dist_args$detect(),
    ~ .x * 1e-2
  ), label = "trans_detect()")

  trans_args
}

trans_x <- function(x, j_nm) {
  if (startsWith(j_nm, "f_")) {
    correct_freq(1 / x)
  } else if (startsWith(j_nm, "r_")) {
    x * 1e-5
  } else if (startsWith(j_nm, "p_")) {
    x * 1e-2
  } else if (j_nm %in% c("eff", "sens", "spec")) {
    x * 1e-2
  } else {
    x
  }
}


trans_j_nm <- function(j_nm) {
  if (any(startsWith(j_nm, c("f_", "r_")))) {
    stringr::str_replace(j_nm, "^[a-z]_", "p_")
  } else {
    j_nm
  }
}


seq_profile <- function(x, n = 55, intervals = c(1, 2, 5, 10)) {
  if (NROW(x) == 1) return(x)
  # Ensure that x is length 2 numeric
  checkmate::assert_numeric(x, len = 2L, finite = TRUE, any.missing = FALSE)

  # Range
  r <- abs(diff(x))

  # Magnitude of intervals
  m <- floor(log10(r)) - floor(log10(n))

  # Scale candidate intervals
  intervals <- intervals * 10^m
  # Expand to +/- 1 order of magnitude
  intervals <- unique(c(intervals * 1e-1, intervals, intervals * 1e1))

  # Must yield `n` intervals or fewer; pick smallest from remaining
  by <- min(intervals[r / intervals <= n])

  # Create sequence; count down if x[[1]] is larger than x[[2]]
  s <- seq(x[[1]], x[[2]], by = if (x[[1]] > x[[2]]) -by else by)

  # Ensure endpoints are always included
  if (x[[2]] != s[[NROW(s)]]) c(s, x[[2]]) else s
}
