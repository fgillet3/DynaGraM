### R CODE FOR A NUMERICAL INTEGRATION OF THE DynaGraM MODEL
### DynaGraM: a process-based model to simulate multi-species plant community 
### dynamics in managed grasslands. Ecological Modelling.
### Thibault Moulin, Antoine Perasso, Pierluigi Calanca, François Gillet
### Last update by F. Gillet, 2020-08-10

### Required packages and general settings ----

library(deSolve)
library(tidyverse)
library(cowplot)
library(lubridate)
library(ggpubr)
library(vegan)

# Change locale parameters to international standard (for date labels)
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

# Fictitious dates for 120-year simulations
alldates <- seq(as.Date("0001-01-01"), as.Date("0120-12-31"), by = "days")
juld <- yday(alldates)
ficdates <- alldates[juld < 366]

# Define colours for seven PFTs
col7 <- c("#56B4E9", "orange", "forestgreen", "green", "red", "purple", "pink")


### Set up the model ----

# Load model equations and parameters
source("DynaGraM_equations.R")
source("DynaGraM_initparms.R")
load("Parameters120.RData")

# Species-specific parameters
(n <- nrow(parasel))
(sp <- rownames(parasel))

# Duration of the simulation
nyears <- 120    # here choose 120 years maximum!
times <- seq(1, nyears * 365, 1)
lengthsimu <- 365 * nyears

# Initialize parameters
parms <- initialize.parameters()

# Initialize state variables
# Equal proportions for green biomass
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = rep(parms$iniB / n, n)
)

# Load scenarios (120 years)
load("Scenarios120.RData")


### Run scenarios and save simulation results (NOT RUN) ----

source("RunScenarios120.R")

### END NOT RUN


### Load model outputs ----

load("Outputs120.RData")



### Fig. 3 - Biomass dynamics under 4 management scenarios ----

# Extensive grazing 
management <- G1M0F0
gday <- management %>% filter(year == 20, SD > 0)
out <- outEG120
source("1-plot_Biomass_G.R")
g1 <- g0 + labs(title = "Extensive Grazing")
B20EG <- biomass1

# Intensive grazing 
management <- G2M0F1
gday <- management %>% filter(year == 20, SD > 0)
out <- outIG120
source("1-plot_Biomass_G.R")
g2 <- g0 + labs(title = "Intensive Grazing")
B20IG <- biomass1

# Extensive mowing 
management <- G0M1F1
out <- outEM120
source("1-plot_Biomass_M.R")
g3 <- g0 + labs(title = "Extensive Mowing")
B20EM <- biomass1

# Intensive mowing 
management <- G0M2F2
out <- outIM120
source("1-plot_Biomass_M.R")
g4 <- g0 + labs(title = "Intensive Mowing")
B20IM <- biomass1

# Use a common legend for multiple plots
dev.new(width = 11,
        height = 7.5,
        noRStudioGD = TRUE)
ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2, labels = "AUTO", 
          common.legend = TRUE, legend = "right")
ggsave("Fig3.pdf")
ggsave("Fig3.png")

# Bind dataframes
B20EG <- B20EG %>% mutate(management = "EG")
B20IG <- B20IG %>% mutate(management = "IG")
B20EM <- B20EM %>% mutate(management = "EM")
B20IM <- B20IM %>% mutate(management = "IM")
Bi20EIGM <- bind_rows(B20EG, B20IG, B20EM, B20IM)
save(Bi20EIGM, file = "Bi20EIGM.RData")


### Fig. 4 - Reducers and growth rates ----

source("2-plot_reducers.R") # function

## Extensive grazing
management <- G1M0F0
out <- outEG120

# Grazing period
Gstart <- 141 # 21 May
Gend <- 274   # 1 Oct
grazD <- as_tibble(management) %>% 
  mutate(time = out$date) %>% 
  select(time, year, DOY) %>% 
  filter(DOY %in% c(Gstart, Gend)) %>% 
  select(-DOY) %>% 
  mutate(grazing = rep(c("start", "end"), nyears)) %>% 
  spread(grazing, time)

# Reducers of plant growth (daily resolution)
# Arithmetic mean
g1EG <- ggRedB(yr = 20, each.sp = FALSE, weighted = FALSE)
g1EG <- g1EG + labs(title = "Extensive Grazing")
# Weighted arithmetic mean
g1EGw <- ggRedB(yr = 20, each.sp = FALSE, weighted = TRUE)
g1EGw <- g1EGw + labs(title = "Extensive Grazing")
source("2-plot_growthrate.R")
g2EG <- g2 + labs(title = "Extensive Grazing")

## Intensive mowing
management <- G0M2F2
out <- outIM120
g1IM <- ggRedB(yr = 20, each.sp = FALSE, weighted = FALSE)
g1IM <- g1IM + labs(title = "Intensive Mowing")
g1IMw <- ggRedB(yr = 20, each.sp = FALSE, weighted = TRUE)
g1IMw <- g1IMw + labs(title = "Intensive Mowing")
source("2-plot_growthrate.R")
g2IM <- g2 + labs(title = "Intensive Mowing")

# Plot Fig. 4
dev.new(width = 11,
        height = 5,
        noRStudioGD = TRUE)
ggarrange(g1EGw, g1IMw, ncol = 2, labels = "AUTO", 
          common.legend = TRUE, legend = "bottom")
ggsave("Fig4ab.pdf")
ggsave("Fig4ab.png")

ggarrange(g2EG, g2IM, ncol = 2, labels = c("C", "D"), 
          common.legend = TRUE, legend = "bottom")
ggsave("Fig4cd.pdf")
ggsave("Fig4cd.png")



### Fig. 6 - Comparison with floristic records ----

# Import the data
load("Obs68.RData")

# Table of PFTs
pft <-
  data.frame(
    row.names = rownames(veg68),
    A = rep(0, nrow(veg68)),
    B = 0,
    C = 0,
    D = 0,
    E = 0,
    R = 0,
    L = 0
  )
pft$A <- apply(veg68[, spe68$PFT == "A"], 1, sum)
pft$B <- apply(veg68[, spe68$PFT == "B"], 1, sum)
pft$C <- apply(veg68[, spe68$PFT == "C"], 1, sum)
pft$D <- apply(veg68[, spe68$PFT == "D"], 1, sum)
pft$E <- apply(veg68[, spe68$PFT == "E"], 1, sum)
pft$R <- apply(veg68[, spe68$PFT == "R"], 1, sum)
pft$L <- apply(veg68[, spe68$PFT == "L"], 1, sum)

# Table of management * PFT
man <- pft[1:4, ] * 0
rownames(man) <- c("GE", "GI", "ME", "MI")
man[1, ] <- apply(pft[rel68$management == "GE", ], 2, mean)
man[2, ] <- apply(pft[rel68$management == "GI", ], 2, mean)
man[3, ] <- apply(pft[rel68$management == "ME", ], 2, mean)
man[4, ] <- apply(pft[rel68$management == "MI", ], 2, mean)
man

obs <- man %>% rownames_to_column(var = "Management") %>% 
  gather(key = PFT, value = Cover, -Management)
obs$PFT <-
  factor(obs$PFT,
         levels = sp,
         ordered = TRUE)
obs$Management <-
  factor(obs$Management,
         levels = c("GE", "GI", "ME", "MI"),
         labels = c("EG", "IG", "EM", "IM"),
         ordered = TRUE)
obs <- as_tibble(obs)
obs

# Plot observed composition
p1 <- ggplot(obs, aes(Management, Cover / 100, fill = PFT)) + 
  geom_col(colour = 1) +
  scale_fill_manual(values = col7) +
  labs(title = "C",
       subtitle = "Observed composition (195 species)",
       x = "Management derived from typology",
       y = "Relative cover [%]") +
  scale_y_continuous(labels = scales::percent) +
  theme_cowplot()
p1

# Compare with simulated data (even initB, year 20)
load("Bi20EIGM.RData")
Bi20EIGM <- Bi20EIGM %>% 
  mutate(DOY = rep(1:365, 4 * 7)) %>% 
  filter(DOY %in% 92:274) %>% 
  select(-date, -year, -DOY)
sim <- Bi20EIGM %>% 
  group_by(PFT, management) %>% 
  summarise(Cover = mean(Biomass))
sim$Scenario <-
  factor(
    sim$management,
    levels = c("EG", "IG", "EM", "IM"),
    ordered = TRUE
  )
sim <- sim %>% select(-management)
sim

# Plot simulated composition
p2 <- ggplot(sim, aes(Scenario, Cover, fill = PFT)) + 
  geom_col(colour = 1, position = "fill") +
  scale_fill_manual(values = col7) +
  labs(title = "A",
       subtitle = "Simulated composition after 20 years",
       x = "Management scenario",
       y = "Relative biomass [%]") +
  scale_y_continuous(labels = scales::percent) +
  theme_cowplot()
p2

# Select the 7 species of DynaGraM
veg7sp <-
  veg68[, c("Lolium perenne",
            "Poa trivialis",
            "Trisetum flavescens",
            "Deschampsia cespitosa",
            "Ranunculus acr. friesianus",
            "Taraxacum officinale",
            "Trifolium pratense"
  )]

# Mean relative cover of species by management
# Table of management * PFT
man7 <- pft[1:4, ] * 0
rownames(man7) <- c("GE", "GI", "ME", "MI")
man7[1, ] <- apply(veg7sp[rel68$management == "GE", ], 2, mean)
man7[2, ] <- apply(veg7sp[rel68$management == "GI", ], 2, mean)
man7[3, ] <- apply(veg7sp[rel68$management == "ME", ], 2, mean)
man7[4, ] <- apply(veg7sp[rel68$management == "MI", ], 2, mean)
obs7 <- man7 %>% rownames_to_column(var = "Management") %>% 
  gather(key = PFT, value = Cover, -Management)
obs7$PFT <-
  factor(obs7$PFT,
         levels = sp,
         ordered = TRUE)
obs7$Management <-
  factor(obs7$Management,
         levels = c("GE", "GI", "ME", "MI"),
         labels = c("EG", "IG", "EM", "IM"),
         ordered = TRUE)
obs7 <- as_tibble(obs7)
obs7

# Plot observed composition (7 species)
p3 <- ggplot(obs7, aes(Management, Cover / 100, fill = PFT)) + 
  geom_col(colour = 1, position = "fill") +
  scale_fill_manual(values = col7) +
  labs(title = "B",
       subtitle = "Observed composition (7 species)",
       x = "Management derived from typology",
       y = "Relative cover [%]") +
  scale_y_continuous(labels = scales::percent) +
  theme_cowplot()
p3

# Plot Fig. 6
dev.new(width = 11, height = 5, noRStudioGD = TRUE)
ggarrange(p2, p3, p1, nrow = 1, common.legend = TRUE, legend = "right")
ggsave("Fig6.pdf")
ggsave("Fig6.png")



### Fig. 7 - Sensitivity to initial conditions ----

# Initial composition
dev.new(width = 5, height = 4, noRStudioGD = TRUE)
source("3-plot_composition_init.R")
p1
ggsave("Fig7a.pdf")
ggsave("Fig7a.png")

## Compute simulations (NOT RUN)

# Composition at year 10 (mean of vegetation period)
nyears <- 10  
times <- seq(1, nyears * 365, 1)
lengthsimu <- 365 * nyears
# Extensive grazing
management <- G1M0F0
scenario <- "Extensive Grazing"
source("3-plot_composition_10yr.R")
Bi10EG <- Bi10
# Intensive grazing
management <- G2M0F1
scenario <- "Intensive Grazing"
source("3-plot_composition_10yr.R")
Bi10IG <- Bi10
# Extensive mowing
management <- G0M1F1
scenario <- "Extensive Mowing"
source("3-plot_composition_10yr.R")
Bi10EM <- Bi10
# Intensive mowing
management <- G0M2F2
scenario <- "Intensive Mowing"
source("3-plot_composition_10yr.R")
Bi10IM <- Bi10
# Bind dataframes
Bi10 <- bind_rows(Bi10EG, Bi10IG, Bi10EM, Bi10IM)
save(Bi10, file = "Bi10.RData")

# Composition at year 50 (mean of vegetation period)
nyears <- 50  
times <- seq(1, nyears * 365, 1)
lengthsimu <- 365 * nyears
# Extensive grazing
management <- G1M0F0
scenario <- "Extensive Grazing"
source("3-plot_composition_50yr.R")
Bi50EG <- Bi50
# Intensive grazing
management <- G2M0F1
scenario <- "Intensive Grazing"
source("3-plot_composition_50yr.R")
Bi50IG <- Bi50
# Extensive mowing
management <- G0M1F1
scenario <- "Extensive Mowing"
source("3-plot_composition_50yr.R")
Bi50EM <- Bi50
# Intensive mowing
management <- G0M2F2
scenario <- "Intensive Mowing"
source("3-plot_composition_50yr.R")
Bi50IM <- Bi50
# Bind dataframes
Bi50 <- bind_rows(Bi50EG, Bi50IG, Bi50EM, Bi50IM)
save(Bi50, file = "Bi50.RData")

## END NOT RUN

# Barplots 1 - composition at year 10 (mean of vegetation period)
load("Bi10.RData")
dev.new(width = 8, height = 7, noRStudioGD = TRUE)
ggplot(Bi10, aes(Assemblage, Biomass, fill = PFT)) + 
  geom_col(colour = 1) +
  scale_fill_manual(values = col7) +
  facet_wrap(~Scenario) +
  labs(title = "B",
       subtitle = "Composition after 10 years",
       x = "Community structure",
       y = expression(paste("Green biomass [kg DM ", ha ^ -1, "]"))) +
  theme_cowplot() + theme(legend.position = "none")
ggsave("Fig7b.pdf")
ggsave("Fig7b.png")

# Barplots 2 - composition at year 50 (mean of vegetation period)
load("Bi50.RData")
dev.new(width = 8, height = 7, noRStudioGD = TRUE)
ggplot(Bi50, aes(Assemblage, Biomass, fill = PFT)) + 
  geom_col(colour = 1) +
  scale_fill_manual(values = col7) +
  facet_wrap(~Scenario) +
  labs(title = "B",
       subtitle = "Composition after 50 years",
       x = "Community structure",
       y = expression(paste("Green biomass [kg DM ", ha ^ -1, "]"))) +
  theme_cowplot() + theme(legend.position = "none")
ggsave("Fig7c.pdf")
ggsave("Fig7c.png")



### Fig. 8 - Sensitivity to initial conditions - line plot of Simpson evenness ----

nyears <- 120  
times <- seq(1, nyears * 365, 1)
lengthsimu <- 365 * nyears

## Compute simulations (NOT RUN)

# Extensive grazing
management <- G1M0F0
source("4-compute_evenness.R") # long computation!
MatE1 <- as_tibble(MatE1) %>% 
  mutate(date = ficdates, year = rep(1:nyears, each = 365))
save(MatE1, file = "MatE1.RData")
# Intensive mowing
management <- G0M2F2
source("4-compute_evenness.R") # long computation!
MatE1 <- as_tibble(MatE1) %>% 
  mutate(date = ficdates, year = rep(1:nyears, each = 365))
save(MatE1, file = "MatE2.RData")
# Intensive grazing
management <- G2M0F1
source("4-compute_evenness.R") # long computation!
MatE1 <- as_tibble(MatE1) %>% 
  mutate(date = ficdates, year = rep(1:nyears, each = 365))
save(MatE1, file = "MatE3.RData")
# Extensive mowing
management <- G0M1F1
source("4-compute_evenness.R") # long computation!
MatE1 <- as_tibble(MatE1) %>% 
  mutate(date = ficdates, year = rep(1:nyears, each = 365))
save(MatE1, file = "MatE4.RData")

## END NOT RUN

## Compare mean and amplitude of evenness
load("MatE1.RData") # EG
MatE1$sd <- apply(MatE1[, 1:8], 1, sd)
Eeg <- MatE1 %>% 
  select(date, year, mean, sd, delta) %>% 
  mutate(Scenario = "Extensive Grazing")

load("MatE3.RData") # IG
MatE1$sd <- apply(MatE1[, 1:8], 1, sd)
Eig <- MatE1 %>% 
  select(date, year, mean, sd, delta) %>% 
  mutate(Scenario = "Intensive Grazing")

load("MatE4.RData") # EM
MatE1$sd <- apply(MatE1[, 1:8], 1, sd)
Eem <- MatE1 %>% 
  select(date, year, mean, sd, delta) %>% 
  mutate(Scenario = "Extensive Mowing")

load("MatE2.RData") # IM
MatE1$sd <- apply(MatE1[, 1:8], 1, sd)
Eim <- MatE1 %>% 
  select(date, year, mean, sd, delta) %>% 
  mutate(Scenario = "Intensive Mowing")

Ev <- bind_rows(Eeg, Eig, Eem, Eim) %>% 
  mutate(CV = sd / mean)
Ev$Scenario <- factor(Ev$Scenario,
    levels = c(
      "Extensive Grazing",
      "Intensive Grazing",
      "Extensive Mowing",
      "Intensive Mowing"),
    ordered = TRUE)

save(Ev, file = "Ev.RData")

## Plot the results
load("Ev.RData")
# Mean evenness over the whole year +- SD
Evm <- Ev %>% group_by(year, Scenario) %>% 
  summarise(Evenness = mean(mean), SD = mean(sd))
Evm100y <- Evm %>% filter(year < 101)
dev.new(width = 9, height = 6, noRStudioGD = TRUE)
ggplot(Evm100y, aes(year, Evenness, colour = Scenario, fill = Scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(
    ymin = Evenness - SD,
    ymax = Evenness + SD
  ), alpha = 0.3, linetype = 0) +
  scale_colour_manual(values = c("green2", "green4", "orange", "red")) +
  scale_fill_manual(values = c("green2", "green4", "orange", "red")) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  labs(x = expression("Time [yr]"),
       y = expression("Simpson evenness [-]")) +
  theme_cowplot() + theme(legend.position = "bottom")

# Mean evenness over the vegetation period +- SD
Evv <- Ev %>% mutate(DOY = rep(1:365, 4 * nyears)) %>% 
  filter(DOY %in% c(91:273))
Evm <- Evv %>% group_by(year, Scenario) %>% 
  summarise(Evenness = mean(mean), SD = mean(sd))
Evm100y <- Evm %>% filter(year < 101)
ggplot(Evm100y, aes(year, Evenness, colour = Scenario, fill = Scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(
    ymin = Evenness - SD,
    ymax = Evenness + SD
  ), alpha = 0.3, linetype = 0) +
  scale_colour_manual(values = c("green2", "green4", "orange", "red")) +
  scale_fill_manual(values = c("green2", "green4", "orange", "red")) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = expression("Time [yr]"),
       y = expression("Simpson evenness [-]")) +
  theme_cowplot() + theme(legend.position = "bottom")
ggsave("Fig8.pdf")
ggsave("Fig8.png")



### Online Appendices: Plot periodic changes ----

## Extensive grazing
management <- G1M0F0
out <- outEG120
source("5-compute_changes.R")
chalEG <- chal %>% mutate(Scenario = "Extensive Grazing")

## Intensive mowing
management <- G0M2F2
out <- outIM120
source("5-compute_changes.R")
chalIM <- chal %>% mutate(Scenario = "Intensive Mowing")

## Intensive grazing
management <- G2M0F1
out <- outIG120
source("5-compute_changes.R")
chalIG <- chal %>% mutate(Scenario = "Intensive Grazing")

## Extensive mowing
management <- G0M1F1
out <- outEM120
source("5-compute_changes.R")
chalEM <- chal %>% mutate(Scenario = "Extensive Mowing")

# Merge scenarios
chal <- bind_rows(chalEG, chalEM, chalIG, chalIM)

## Plot the results
dev.new(width = 10, height = 6, noRStudioGD = TRUE)

# Plot soil state variables
chalnw <- chal %>% filter(Year > 5, Year < 101, Variable %in% c("No", "Nm", "W"))
ggplot(data = chalnw, aes(x = Year, y = Rate, colour = Variable)) + 
  geom_point(size = 1) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 100, by = 10), 
                     minor_breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(name = "Rate of change [%]") +
  facet_wrap(~Scenario) +
  theme_cowplot()

# Plot vegetation state variables
# Rate of change for vegetation in relation to the total biomass (whole year)
dev.new(width = 10, height = 6, noRStudioGD = TRUE)
chalb <- chal %>% 
  filter(Year > 5, Year < 101, 
         Variable %in% c("Ba", "Bb", "Bc", "Bd", "Be", "Br", "Bl", "Bt"))
chalb$PFT <- factor(chalb$Variable, 
                    levels = c("Ba", "Bb", "Bc", "Bd", "Be", "Br", "Bl", "Bt"),
                    labels = c("A", "B", "C", "D", "E", "R", "L", "all"))
ggplot(data = chalb, aes(x = Year, y = Rate, colour = PFT)) + 
  geom_point(size = 1) + geom_line() +
  scale_color_manual(values = c(col7, "black")) + 
  scale_x_continuous(breaks = seq(0, 120, by = 10), 
                     minor_breaks = seq(0, 120, by = 5)) +
  scale_y_continuous(name = "Rate of change relative to total biomass [%]") +
  facet_wrap(~Scenario) +
  theme_cowplot()

# Plot evenness (rate of change over the whole year)
dev.new(width = 9, height = 6, noRStudioGD = TRUE)
chale <- chal %>% filter(Year > 5, Variable %in% c("Beve"))
chale$Scenario <- factor(
  chale$Scenario,
  levels = c(
    "Extensive Grazing",
    "Intensive Grazing",
    "Extensive Mowing",
    "Intensive Mowing"
  ),
  ordered = TRUE
)
ggplot(data = chale, aes(x = Year, y = Rate, colour = Scenario)) + 
  geom_point(size = 1) + geom_line(size = 1) +
  scale_colour_manual(values = c("green2", "green4", "orange", "red")) +
  scale_x_continuous(name = expression("Time [yr]"),
                     breaks = seq(0, 120, by = 10),
                     minor_breaks = seq(0, 120, by = 5),
                     limits = c(0, 100)) +
  scale_y_continuous(name = expression("Rate of change [%]"),
                     breaks = seq(0, 5, by = 0.5),
                     minor_breaks = seq(0, 5, by = 0.1)) +
  theme_cowplot() + theme(legend.position = "bottom")
ggsave("FigS4.pdf")
ggsave("FigS4.png")


# Restrict outputs to the vegetation period
# DOY 91 to 273 (1st April to 30th September)

## Extensive grazing
management <- G1M0F0
out <- outEG120 %>% filter(DOY %in% c(91:273))
source("5-compute_changes.R")
chalEG <- chal %>% mutate(Scenario = "Extensive Grazing")

## Intensive mowing
management <- G0M2F2
out <- outIM120 %>% filter(DOY %in% c(91:273))
source("5-compute_changes.R")
chalIM <- chal %>% mutate(Scenario = "Intensive Mowing")

## Intensive grazing
management <- G2M0F1
out <- outIG120 %>% filter(DOY %in% c(91:273))
source("5-compute_changes.R")
chalIG <- chal %>% mutate(Scenario = "Intensive Grazing")

## Extensive mowing
management <- G0M1F1
out <- outEM120 %>% filter(DOY %in% c(91:273))
source("5-compute_changes.R")
chalEM <- chal %>% mutate(Scenario = "Extensive Mowing")

# Merge scenarios
chal <- bind_rows(chalEG, chalEM, chalIG, chalIM)


## Plot the results
dev.new(width = 9, height = 6, noRStudioGD = TRUE)

# Plot soil state variables
chalnw <- chal %>% filter(Year > 9, Year < 101, Variable %in% c("No", "Nm", "W"))
ggplot(data = chalnw, aes(x = Year, y = Rate, colour = Variable)) + 
  geom_point(size = 1) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(name = "Rate of change [%]") +
  facet_wrap(~Scenario) +
  theme_cowplot()
ggsave("FigS2.pdf")
ggsave("FigS2.png")

# Plot vegetation state variables
chalb <- chal %>% 
  filter(Year > 9, 
         Year < 101,
         Variable %in% c("Ba", "Bb", "Bc", "Bd", "Be", "Br", "Bl", "Bt"))
chalb$PFT <- factor(chalb$Variable, 
                    levels = c("Ba", "Bb", "Bc", "Bd", "Be", "Br", "Bl", "Bt"),
                    labels = c("A", "B", "C", "D", "E", "R", "L", "all"))
ggplot(data = chalb, aes(x = Year, y = Rate, colour = PFT)) + 
  geom_point(size = 1) + geom_line() +
  scale_color_manual(values = c(col7, "black")) + 
  scale_x_continuous(breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(name = "Rate of change relative to total biomass [%]") +
  facet_wrap(~Scenario) +
  theme_cowplot()
ggsave("FigS3.pdf")
ggsave("FigS3.png")


# Fig. 5 - Rate of change for Simpson evenness (for the vegetation period) ----

dev.new(width = 9, height = 6, noRStudioGD = TRUE)
chale <- chal %>% filter(Year > 4, Year < 101, Variable %in% c("Beve"))
chale$Scenario <- factor(
  chale$Scenario,
  levels = c(
    "Extensive Grazing",
    "Intensive Grazing",
    "Extensive Mowing",
    "Intensive Mowing"
  ),
  ordered = TRUE
)
ggplot(data = chale, aes(x = Year, y = Rate, colour = Scenario)) + 
  geom_point(size = 1) + geom_line(size = 1) +
  scale_colour_manual(values = c("green2", "green4", "orange", "red")) +
  scale_x_continuous(name = expression("Time [yr]"),
                     breaks = seq(0, 120, by = 10),
                     minor_breaks = seq(0, 120, by = 5),
                     limits = c(0, 100)) +
  scale_y_continuous(name = expression("Rate of change [%]"),
                     breaks = seq(0, 5, by = 0.5),
                     minor_breaks = seq(0, 5, by = 0.1)) +
  theme_cowplot() + theme(legend.position = "bottom")
ggsave("Fig5.pdf")
ggsave("Fig5.png")



### Online Appendix - Comparison with ModVege (intensive mowing) ----

# Read ModeVege output
# With composition: 0.7 A, 0.1 B, 0.1 C, 0.1 D
load("ModVegeOuput.RData")
# Read DynaGraM output
management <- G0M2F2
out <- outIM120
# Choose the year
yr <- 20
# Plot the results
source("6-ModVege_vs_DynaGraM.R")
dev.new(width = 11, height = 7.5, noRStudioGD = TRUE)
ggarrange(g1, g2, g3, g4, nrow = 2, ncol = 2, labels = "AUTO")
ggsave("FigS5.pdf")
ggsave("FigS5.png")



### Fig. 2 - CSR triangle ----

library(ggtern)

# Import data
load("CSR7species.RData")

# Ternary plot using ggtern library
dev.new(width = 6.5,
        height = 5,
        noRStudioGD = TRUE)
ggtern(data = CSR7, aes(R, C, S, fill = PFT)) + 
  geom_point(size = 5, shape = 21, colour = "white") +
  theme_bw() + theme_showarrows() +
  theme_nomask() +
  scale_fill_manual(values = col7) +
  geom_text(aes(label = species), 
            hjust = -0.2) +
  labs(xarrow = "Disturbance", 
       yarrow = "Competition", 
       zarrow = "Stress")
ggsave("Fig2.pdf")
ggsave("Fig2.png")



# Restore locale parameters
Sys.setlocale("LC_TIME", lct)
