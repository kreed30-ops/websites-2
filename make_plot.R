library(tidyverse)

top_songs <- read_rds("clean_data.rds")

latest_positions <- top_songs |>
  group_by(song) |>
  slice_max(week, n = 1, with_ties = FALSE) |>
  mutate(label = str_trunc(track, width = 18)) |>
  ungroup()

billboard_plot <- ggplot(top_songs, aes(x = week, y = rank, group = song, color = song)) +
  geom_line(linewidth = 1.4, lineend = "round") +
  geom_point(size = 2.5, show.legend = FALSE) +
  geom_text(
    data = latest_positions,
    aes(label = label),
    hjust = -0.05,
    size = 3.2,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1, 20, by = 2),
    limits = c(1, 24),
    expand = c(0, 0)
  ) +
  scale_y_reverse(
    breaks = c(1, 5, 10, 25, 50, 75, 100),
    limits = c(100, 1),
    expand = c(0, 0)
  ) +
  scale_color_viridis_d(option = "mako", end = 0.9) +
  labs(
    title = "The climb to number one",
    subtitle = "Weekly Billboard rank during each song's first 20 weeks",
    x = "Week on the chart",
    y = "Billboard rank",
    caption = "Source: tidyr::billboard | Lower rank numbers are better"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 22, color = "#17202a"),
    plot.subtitle = element_text(color = "#52616b", margin = margin(b = 18)),
    axis.title = element_text(face = "bold", color = "#17202a"),
    axis.text = element_text(color = "#52616b"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(12, 90, 12, 12)
  )

ggsave("billboard.png", plot = billboard_plot, width = 10, height = 6)
