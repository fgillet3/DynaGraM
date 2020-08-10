initBi <- data.frame(Assemblage = paste0("S", 1:8), 
                     A = c(150, rep(25, 6), 300 / 7),
                     B = c(25, 150, rep(25, 5), 300 / 7),
                     C = c(rep(25, 2), 150, rep(25, 4), 300 / 7),
                     D = c(rep(25, 3), 150, rep(25, 3), 300 / 7),
                     E = c(rep(25, 4), 150, rep(25, 2), 300 / 7),
                     R = c(rep(25, 5), 150, 25, 300 / 7),
                     L = c(rep(25, 6), 150, 300 / 7))
rowSums(initBi[, -1])

initBi <- initBi %>% gather(key = PFT, value = Biomass, -Assemblage)
initBi$PFT <-
  factor(initBi$PFT,
         levels = sp,
         ordered = TRUE)

p1 <- ggplot(initBi, aes(Assemblage, Biomass, fill = PFT)) + 
  geom_col(colour = 1) +
  scale_fill_manual(values = col7) +
  labs(title = "A",
       subtitle = "Initial composition",
       x = "Community structure",
       y = expression(paste("Green biomass [kg DM ", ha ^ -1, "]"))) +
  theme_cowplot()
