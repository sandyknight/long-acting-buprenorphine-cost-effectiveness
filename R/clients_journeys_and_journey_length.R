


f <- function(x) {
 (nrow(unique(x)))
}

#The total rows is the number of client-journeys
client_journeys <- f(id[, .(client_random_id, n_jy)])

clients <- f(id[, .(client_random_id)])

journeys_per_client <- client_journeys / clients

mean_length_of_journey <-
  id[, .(triaged, disd, jy_length = lubridate::interval(triaged, disd) %/% days(1))][, .(mean_jy_length = mean(jy_length, na.rm = T))]
