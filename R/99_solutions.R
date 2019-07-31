#'02_data_manipulation
#'
#' With the tki_demo and tki_demo_complications data sets, for each participant,
#' find the first time the "Man Flu" complication was observed by participants
#' taking Drug 1 and a measurement greater than 15.0 was observed and return the
#' participants id, intervention, gender, trial day, and the measurement.
#' (note, only males can suffer from Man Flu virus)

left_join(tki_demo, tki_demo_complications, by = "id") %>%
  pivot_longer(starts_with("day"),
               names_to = "day",
               values_to = "measurement",
               names_prefix = "day") %>%
  filter(intervention == "Drug 1",
         measurement > 15.0,
         male) %>%
  select(id, intervention, male, day, measurement) %>%
  group_by(id) %>%
  top_n(1, desc(day))
