request <- list(
  dataset_short_name = "derived-era5-single-levels-daily-statistics",
  product_type = "reanalysis",
  variable = "total_precipitation",
  year = "2024",
  month = "01",
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  daily_statistic = "daily_sum",
  time_zone = "utc+00:00",
  frequency = "1_hourly",
  target = "test.nc",
  request$area <- c(
      52.503588,
      1.372544,
      52.003588,
      1.872544
  )
)
