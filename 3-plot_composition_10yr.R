initBi <- data.frame(Assemblage = paste0("S", 1:8), 
                     A = c(150, rep(25, 6), 300 / 7),
                     B = c(25, 150, rep(25, 5), 300 / 7),
                     C = c(rep(25, 2), 150, rep(25, 4), 300 / 7),
                     D = c(rep(25, 3), 150, rep(25, 3), 300 / 7),
                     E = c(rep(25, 4), 150, rep(25, 2), 300 / 7),
                     R = c(rep(25, 5), 150, 25, 300 / 7),
                     L = c(rep(25, 6), 150, 300 / 7))

Bi10 <- tibble(A = rep(.0, 8), B = .0, C = .0, D = .0, E = .0, R = .0, L = .0)

# 1. Dominance of PFT A
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[1, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[1, ] <- outy10 %>% summarise_all(mean)

# 2. Dominance of PFT B
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[2, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[2, ] <- outy10 %>% summarise_all(mean)

# 3. Dominance of PFT C
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[3, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[3, ] <- outy10 %>% summarise_all(mean)

# 4. Dominance of PFT D
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[4, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[4, ] <- outy10 %>% summarise_all(mean)

# 5. Dominance of PFT E
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[5, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[5, ] <- outy10 %>% summarise_all(mean)

# 6. Dominance of PFT R
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[6, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[6, ] <- outy10 %>% summarise_all(mean)

# 7. Dominance of PFT L
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[7, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[7, ] <- outy10 %>% summarise_all(mean)

# 8. Codominance of all PFTs
state <- c(
  No = soil$Norg,
  Nm = 30,
  W = parms$WHC,
  B = as.numeric(initBi[8, -1])
)
out1 <- ode(
  y = state,
  times = times,
  func = DynaGraM,
  parms = parms,
  method = "euler"
)
out <- as.data.frame(out1)
# Bi in the vegetation period at year 10
outy10 <- out %>% select(5:11) %>% 
  mutate(year = rep(1:nyears, each = 365)) %>% 
  filter(year == 10) %>% 
  mutate(DOY = 1:365) %>% 
  filter(DOY %in% 91:273) %>% 
  select(-year, -DOY)
Bi10[8, ] <- outy10 %>% summarise_all(mean)

# Convert to long format
Bi10 <- Bi10 %>% mutate(Assemblage = paste0("S", 1:8)) %>% 
  gather(key = PFT, value = Biomass, -Assemblage) %>% 
  mutate(Scenario = scenario)
Bi10$PFT <-
  factor(Bi10$PFT,
         levels = sp,
         ordered = TRUE)
