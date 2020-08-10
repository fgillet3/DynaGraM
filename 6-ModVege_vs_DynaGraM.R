## Plot DynaGraM LAI for the year 2004 ----

# Extract from output (choose the year)
outsel <- out %>% filter(year == yr)

g1 <- ggplot(outsel, aes(date, LAIt)) +
  geom_line(size = 1, colour = "deeppink3") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 10) +
  labs(title = "DynaGraM",
       x = expression("Time [d]"), 
       y = expression(paste("Leaf area index [", m^2, m^-2, "]"))) +
  theme_cowplot()


## Plot ModVege LAI for the year 2004 ----

ModVege$date <- outsel$date
g2 <- ggplot(ModVege, aes(date, LAI)) +
  geom_line(size = 1, colour = "deeppink3") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 10) +
  labs(title = "ModVege",
       x = expression("Time [d]"), 
       y = expression(paste("Leaf area index [", m^2, m^-2, "]"))) +
  theme_cowplot()


## Plot DynaGraM pot & act growth for year 2004 ----

outsel$gr <-
  outsel$gr1 + outsel$gr2 + outsel$gr3 + outsel$gr4 + 
  outsel$gr5 + outsel$gr6 + outsel$gr7

grow <- outsel %>% select(date, potgrowth, gr) %>% 
  gather(key = "Variable", value = "Rate", -date)
grow$Variable <- factor(grow$Variable, 
                        levels = c("potgrowth", "gr"),
                        labels = c("Potential growth", "Actual growth"),
                        ordered = TRUE)

g3 <- ggplot(grow, aes(date, Rate, colour = Variable)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 450) +
  labs(x = expression("Time [d]"), 
       y = expression(paste("Growth rate [kg DM ", ha^-1, d^-1, "]"))) +
  scale_colour_manual(name = NULL, values = c("tomato", "green3")) +
  theme_cowplot() + theme(legend.position = "top")


## Plot ModVege pot & act growth for year 2004 ----

grow <- ModVege %>% select(date, actGRO, potGRO) %>% 
  gather(key = "Variable", value = "Rate", -date)
grow$Variable <- factor(grow$Variable,
                        levels = c("potGRO", "actGRO"),
                        labels = c("Potential growth", "Actual growth"),
                        ordered = TRUE)

g4 <- ggplot(grow, aes(date, Rate, colour = Variable)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylim(0, 450) +
  labs(x = expression("Time [d]"), 
       y = expression(paste("Growth rate [kg DM ", ha^-1, d^-1, "]"))) +
  scale_colour_manual(name = NULL, values = c("tomato", "green3")) +
  theme_cowplot() + theme(legend.position = "top")
