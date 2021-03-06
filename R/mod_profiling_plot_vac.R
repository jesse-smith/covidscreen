profiling_plot_vac <- function(data, i_nm, j_nm) {
  x_title <- profile_arg_labels[[i_nm]][[j_nm]]
  tt_decimals <- -floor(log10(max(data$n, na.rm = TRUE))) + 2
  data %>%
    hchart(
      "line",
      hcaes(
        name  = forcats::as_factor(.data$group),
        group = forcats::as_factor(.data$group),
        x = .data$x,
        y = .data$n,
        color = .data$color
      ),
      marker = list(enabled = FALSE, symbol = "circle")
    ) %>%
    hc_colors(c("#90caf9", "#90a4ae")) %>%
    hc_xAxis(title = list(text = x_title)) %>%
    hc_yAxis(title = list(text = "Cases Detected per 100 Tests"), min = 0) %>%
    hc_tooltip(
      shared = TRUE,
      valueDecimals = tt_decimals,
      headerFormat = paste0("<b>", x_title, ": {point.x}</b><br>"),
      pointFormat = paste0(
        "<span style='color: {point.color}; font-weight: bold'>",
        "{point.name}: {point.y}%</span> positive tests<br>"
      )
    )
}

profiling_prep_vac <- function(x, x_t, n, dist_args, i_nm, j_nm) {
  if (i_nm == "n") {
    d0 <- reactive_dist(const_testing(dist_args))
    d1 <- reactive_dist(const_testing(dist_args, 1, 1))


    s <- reactive(calc_vac_slopes(data_test0 = d0(), data_test1 = d1()))

    reactive(data.table(
      group   = rep(c("Vaccinated", "Unvaccinated"), times = NROW(x)),
      x = rep(x, each = 2),
      n = rep(s() * 100, times = NROW(x)),
      color = rep(c("#90caf9", "#90a4ae"), NROW(x))
    ))
  } else {
    s <- reactive_map_vac(x_t, dist_args = dist_args, i_nm = i_nm, j_nm = j_nm)

    reactive(data.table(
      group = rep(c("Vaccinated", "Unvaccinated"), times = NROW(x)),
      x = rep(x, each = 2),
      n = s() * 100,
      color = rep(c("#90caf9", "#90a4ae"), NROW(x))
    ))
  }
}


reactive_map_vac <- function(new_arg_seq, dist_args, i_nm, j_nm) {
  reactive(purrr::flatten_dbl(purrr::map(
    new_arg_seq,
    ~ reactive_vac_mapper(
      .x,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )()
  )))
}


reactive_vac_mapper <- function(new_arg, dist_args, i_nm, j_nm) {
  args <- insert_args(new_arg, dist_args = dist_args, i_nm = i_nm, j_nm = j_nm)
  d0 <- reactive_dist(const_testing(args))
  d1 <- reactive_dist(const_testing(args, 1, 1))

  reactive(calc_vac_slopes(data_test0 = d0(), data_test1 = d1()))
}
