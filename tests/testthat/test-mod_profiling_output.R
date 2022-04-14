n_org_point <- 2000
n_org_range <- c(1000, 3000)
vac_point <- list(p_comm = 60, p_org = 40, eff = 50)
vac_range <- list(p_comm = c(0, 100), p_org = c(0, 100), eff = c(0, 100))
inf_point <- list(r_incid = 500, t_symp = 7, t_presymp = 3)
inf_range <- list(r_incid = c(10, 1000), t_symp = c(1, 9), t_presymp = c(1, 9))
symp_point <- list(p_inf_vac = 40, p_inf_unvac = 60, p_uninf = 1)
symp_range <- list(p_inf_vac = c(0, 100), p_inf_unvac = c(0, 100), p_uninf = c(0, 100))
test_point <- list(p_symp = 90, f_asymp_vac = 0, f_asymp_unvac = 4)
test_range <- list(p_symp = c(0, 100), f_asymp_vac = c(0, 100), f_asymp_unvac = c(0, 100))
detect_point <- list(sens = 90, spec = 99)
detect_range <- list(sens = c(0, 100), spec = c(50, 100))

choose_pnt_rng <- function(pnt, rng, x, ref) {
  if (x == ref) reactive(rng) else reactive(pnt)
}

test_output <- function(x, y) {
  inputs <- list(
    n = reactiveValues(org = choose_pnt_rng(n_org_point, n_org_range, x, "n_org")),
    dist_args = reactiveValues(
      vac = reactive(vac_point),
      inf = reactive(inf_point),
      symp = reactive(symp_point),
      test = reactive(test_point),
      detect = reactive(detect_point)
    ),
    vars = reactiveValues(
      x = reactiveVal(x),
      y = reactiveVal(y)
    ),
    calc = reactiveVal(0L)
  )
  testServer(mod_profiling_output_server, args = list(inputs = inputs), {
    json <- jsonlite::parse_json(output$risk_plot)
    expect_snapshot(json$x$hc_opts$series)
  })
}


test_that("profiling output server returns expected outputs", {
  test_output(x = "n_org", y = "risk")
  test_output(x = "vac_p_comm", y = "risk")
})
