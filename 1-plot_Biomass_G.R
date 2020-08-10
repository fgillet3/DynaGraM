# Area plot of aboveground biomass of 7 species or groups
# F. Gillet 2020-01-03

biomass <- as_tibble(out) %>% select(date, year, 5:(n + 4)) %>% 
  gather(key = "PFT", value = "Biomass", -date, -year)
biomass$PFT <- factor(biomass$PFT, labels = c("A", "B" , "C", "D", "E", "R", "L"))
biomass$Biomass <- biomass$Biomass / 1000
biomass1 <- biomass %>% filter(year == 20)

g0 <-
  ggplot(biomass1, aes(date, Biomass, fill = PFT)) +
  geom_rect(
    aes(
      xmin = biomass1$date[gday$DOY[1]],
      xmax = biomass1$date[gday$DOY[nrow(gday)]],
      ymin = 0,
      ymax = 4.5
    ),
    fill = "lightyellow",
    alpha = 1
  ) +
  geom_area(position = "stack") +
  ylim(0, 4.5) +
  scale_fill_manual(values = col7) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = expression("Time [d]"),
       y = expression(paste("Green biomass [t DM ", ha ^ -1, "]"))) +
  geom_vline(xintercept = biomass$date[management$mdays == 1]) +
  geom_vline(xintercept = biomass$date[management$Nof > 0], 
             linetype = 1, colour = "cyan", size = 1) +
  geom_vline(xintercept = biomass$date[management$Nmf > 0], 
             linetype = "dashed", colour = "magenta") +
  theme_cowplot() + theme(legend.position = "right")

