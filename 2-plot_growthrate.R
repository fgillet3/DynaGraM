# Actual net growth
names(out)
netgr <- as_tibble(out) %>% select(date, 78:84, year) %>% 
  gather(key = "PFT", value = "ActualGrowth", -date, -year)
netgr$PFT <- factor(netgr$PFT, labels = c("A", "B", "C", "D", "E", "R", "L"))

netgr1 <- netgr %>% filter(year == 20)
management1 <- management %>% filter(year == 20)

## Area plot
g2 <- ggplot(netgr1, aes(date, ActualGrowth, fill = PFT)) +
  geom_area(position = "stack") +
  # ylim(0, 100) +
  scale_fill_manual(values = col7) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = expression("Time [d]"),
       y = expression(paste("Actual growth rate [kg DM ", ha ^ -1, d ^ -1, "]"))) +
  geom_vline(xintercept = netgr1$date[management1$mdays == 1]) +
  geom_vline(xintercept = netgr1$date[management1$Nof > 0], 
             linetype = 1, colour = "cyan", size = 1) +
  geom_vline(xintercept = netgr1$date[management1$Nmf > 0], 
             linetype = "dashed", colour = "magenta") +
  theme_cowplot() + theme(legend.position = "right")

if (sum(management1$SD) > 0) {
  g2 <- g2 + geom_rect(
    data = filter(grazD, year == 20),
    inherit.aes = FALSE,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = +Inf
    ),
    fill = "yellow",
    alpha = 0.2
  )
}

g2
