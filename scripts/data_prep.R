pacman::p_load(tidyverse, sf, strayr, tidytransit, strayr, lubridate, duckdb, DBI)
# load custom functions
invisible(map(list.files("r/", full.names = TRUE, ), ~ source(.x)))

# get full schedule from gtfs
bne <- get_bne_pt_route_geom()
trip <- get_full_pt_schedule()
service_period <- trip$summary$service_period %>% strsplit(" - ") %>% flatten_chr()
bne_stops <- bne$pt_stops
bne_route_geom <- bne$pt_route_geom

# ref files
divider_cal <-
  expand_grid(
    tibble(
      dt = seq.Date(lubridate::ymd(min(service_period)), lubridate::ymd(max(service_period)), by = "1 day"),
      daytype = if_else(wday(dt, label = TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")
    ),
    hour = 0:23
  ) %>%
  mutate(time_bucket = case_match(
    hour, 6:8 ~ "0600To0859",
    9:15 ~ "0900To1559",
    16:18 ~ "1600To1859",
    c(19:23, 0:5) ~ "1900To0559"
  )) %>%
  group_by(daytype, time_bucket) %>%
  reframe(day_cnt = n_distinct(dt), hours_cnt = n())


# bne stop ids
bne_stop_ids <- bne_stops %>% filter(!is.na(mode)) %>% distinct(mode, stop_id, stop_name)


# aggregation
t <-
  trip$trip_schedule %>%
  filter(stop_id %in% bne_stop_ids$stop_id) %>%
  distinct(stop_id, service_date, dow, departure_time, route_short_name, trip_headsign) %>% # to remove some data duplication in gtfs deviating headway time
  mutate(
    daytype = if_else(dow %in% c("Sat", "Sun"), "weekend", "weekday"),
    hour = lubridate::hour(departure_time),
    time_bucket = case_when(
      between(hour, 7, 18) ~ "0700To1859",
      between(hour, 19, 21) ~ "1900To2159",
      TRUE ~ "2200To0659"
    )
  ) %>%
  group_by(stop_id, service_date) %>%
  arrange(stop_id, service_date, departure_time) %>%
  mutate(headway_in_seconds = lead(departure_time) - departure_time, stop_id = as.character(stop_id))
