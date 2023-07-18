#' Simulate fisheries data to demonstrate FSAR figures

#' @param format Long or wide data frame?
#'
#' @return A data frame
#' @export
#'
#' @examples
#' sim_fsar_data()
sim_fsar_data <- function(format = c("wide", "long")) {
  format <- match.arg(format)
  categories <- c("Catch", "SSB", "Fishing", "Recruitment")
  years <- seq(1950, 2022)
  set.seed(1234)
  ts1 <- 500 + (arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = length(years)) * 100)
  ts1.2 <- rep(400, length(years))

  ts2 <- 2000 + (arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = length(years)) * 400)
  ts2.2 <- rep(1500, length(years))

  sim.df <-
    rbind(
      data.frame(
        panel.category = rep(categories[1], length(years) * 2),
        year = rep(years, 2),
        ts.name = rep(c("Catch-MT", "TAC-MT"), each = length(years)),
        ts.value = c(ts1, ts1.2)
      ),
      data.frame(
        panel.category = rep(categories[2], length(years) * 5),
        year = rep(years, 5),
        ts.name = rep(c("SSB-MT", "SSBlow-MT", "SSBhigh-MT", "SSBlrp-MT", "SSBusr-MT"), each = length(years)),
        ts.value = c(ts2, ts2 + 500, ts2 - 500, ts2.2, ts2.2 * 2)
      ),
      data.frame(
        panel.category = rep(categories[3], length(years) * 5),
        year = rep(years, 5),
        ts.name = rep(c("F-1/yr", "Flow-1/yr", "Fhigh-1/yr", "Flim-1/yr", "M-1/yr"), each = length(years)),
        ts.value = c(ts1 / 500, (ts1 + 100) / 500, (ts1 - 100) / 500, ts1.2 / 400, rep(0.2, length(years)))
      ),
      data.frame(
        panel.category = rep(categories[4], length(years) * 3),
        year = rep(years, 3),
        ts.name = rep(c("R-E06", "Rlow-E06", "Rhigh-E06"), each = length(years)),
        ts.value = c(ts1 / 1E-6, (ts1 / 1E-6) + 2E8, (ts1 / 1E-6) - 2E8) / 1E6
      )
    )

  sim.df$panel.category <- factor(sim.df$panel.category, levels = c("Catch", "SSB", "Fishing", "Recruitment"), ordered = TRUE)
  if (format == "long") {
    return(sim.df)
  } else {
    df <- tidyr::pivot_wider(sim.df,
      id_cols = year,
      names_from = ts.name, values_from = ts.value
    )
    return(df)
  }
}
