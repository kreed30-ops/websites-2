library(tidyverse)

billboard_long <- billboard |>
	pivot_longer(
		cols = starts_with("wk"),
		names_to = "week",
		names_prefix = "wk",
		values_to = "rank",
		values_drop_na = TRUE
	) |>
	mutate(week = as.integer(week))

featured_tracks <- billboard_long |>
	group_by(artist, track) |>
	summarise(
		weeks_in_top_10 = sum(rank <= 10),
		.groups = "drop"
	) |>
	slice_max(weeks_in_top_10, n = 6, with_ties = FALSE)

top_songs <- billboard_long |>
	semi_join(featured_tracks, by = c("artist", "track")) |>
	filter(week <= 20) |>
	mutate(song = str_c(track, " — ", artist))

write_rds(top_songs, file = "clean_data.rds")
