# Function to plot reducers of biomass growth

ggRedB <- function(yr, each.sp = TRUE, weighted = TRUE) {
  
  redu <- as_tibble(out) %>% filter(year == yr) %>% 
    select(date, 5:11, 24:51, 92:98) %>% 
    rename(
      Ared1 = Reducer1,
      Ared2 = Reducer2,
      Ared3 = Reducer3,
      Ared4 = Reducer4,
      Ared5 = Reducer5,
      Ared6 = Reducer6,
      Ared7 = Reducer7) %>%
    pivot_longer(-c(1:8), 
                 names_to = c(".value", "Reducer"), 
                 names_pattern = "(.red)(.)") %>%
    pivot_longer(cols = starts_with("B"),
                 names_to = c(".value", "PFT"), 
                 names_pattern = "(.)(.)") %>% 
    filter(Reducer == PFT) %>% 
    select(-Reducer) %>% 
    pivot_longer(c(2:6), 
                 names_to = "Reducer",
                 values_to = "Value")
  redu$PFT <- factor(
    redu$PFT, 
    levels = as.character(1:7),
    labels = c("A", "B", "C", "D", "E", "R", "L"))
  redu$Reducer <- factor(
    redu$Reducer, 
    levels = c("Gred", "Tred", "Wred", "Nred", "Ared"),
    labels = c("Trampling", "Temperature", "Water", "Nutrients", "All"))
  
  management1 <- management %>% filter(year == yr)
  
  clines <-
    c(
      "Trampling" = "solid",
      "Temperature" = "solid",
      "Water" = "solid",
      "Nutrients" = "solid",
      "All" = "solid"
    )
  csizes <-
    c(
      "Trampling" = 1,
      "Temperature" = 1,
      "Water" = 1,
      "Nutrients" = 1,
      "All" = 0.5
    )
  calpha <-
    c(
      "Trampling" = 1,
      "Temperature" = 1,
      "Water" = 1,
      "Nutrients" = 1,
      "All" = 0.75
    )
  
  if (each.sp) {
    g1 <- ggplot(redu, aes(date, Value, group = Reducer, colour = Reducer)) + 
      geom_line(size = 1) + facet_wrap(~ PFT, nrow = 4) +
      scale_color_manual(values = c("orange", "red", "blue", "green3", "black")) +
      scale_linetype_manual(values = clines) +
      scale_size_manual(values = csizes) +
      scale_alpha_manual(values = calpha) +
      scale_x_date(name = "Time [d]", date_labels = "%b", 
                   date_breaks = "1 month") +
      scale_y_continuous(name = "Reducer value [-]") +
      geom_vline(xintercept = redu$date[management1$mdays == 1]) +
      geom_vline(xintercept = redu$date[management1$Nof > 0], 
                 linetype = 1, colour = "cyan", size = 1) +
      geom_vline(xintercept = redu$date[management1$Nmf > 0], 
                 linetype = "dashed", colour = "magenta") +
      theme_cowplot() + theme(legend.position = "top")
    
    if (sum(management1$SD) > 0) {
      g1 <- g1 + geom_rect(
        data = filter(grazD, year == yr),
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
    
  } else {
    if (weighted) {     # weighted means
      redu2 <- redu %>%
        group_by(date, Reducer) %>% 
        summarise(Value = weighted.mean(Value, B))
    } else {            # unweighted means
      redu2 <- redu %>% 
        group_by(date, Reducer) %>% 
        summarise(Value = mean(Value))
    }
    
    g1 <- ggplot(redu2, aes(date, Value, 
                            colour = Reducer, 
                            linetype = Reducer,
                            size = Reducer)) + 
      geom_line() +
      scale_color_manual(values = c("orange", "red", "blue", "green3", "black")) +
      scale_linetype_manual(values = clines) +
      scale_size_manual(values = csizes) +
      scale_alpha_manual(values = calpha) +
      scale_x_date(name = "Time [d]", date_labels = "%b", 
                   date_breaks = "1 month") +
      scale_y_continuous(name = "Average reducer value [-]") +
      geom_vline(xintercept = redu2[redu2$Reducer == "All", ]$date[management1$mdays == 1]) +
      geom_vline(xintercept = redu2[redu2$Reducer == "All", ]$date[management1$Nof > 0], 
                 linetype = 1, colour = "cyan", size = 1) +
      geom_vline(xintercept = redu2[redu2$Reducer == "All", ]$date[management1$Nmf > 0], 
                 linetype = "dashed", colour = "magenta") +
      theme_cowplot() + theme(legend.position = "top")
    
    if (sum(management1$SD) > 0) {
      g1 <- g1 + geom_rect(
        data = filter(grazD, year == yr),
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
  }
  g1
}
