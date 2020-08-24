initBi <- data.frame(Assemblage = paste0("S", 1:8), 
                     A = c(150, rep(25, 6), 300 / 7),
                     B = c(25, 150, rep(25, 5), 300 / 7),
                     C = c(rep(25, 2), 150, rep(25, 4), 300 / 7),
                     D = c(rep(25, 3), 150, rep(25, 3), 300 / 7),
                     E = c(rep(25, 4), 150, rep(25, 2), 300 / 7),
                     R = c(rep(25, 5), 150, 25, 300 / 7),
                     L = c(rep(25, 6), 150, 300 / 7))

# Create the matrix with all evenness values
MatE <- matrix(NA, nrow = lengthsimu, ncol = 8)
colnames(MatE) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 1] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 2] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 3] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 4] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 5] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 6] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 7] <- E

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
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
E <- N2 / n
MatE[, 8] <- E

MatE <- as.data.frame(MatE)

# Compute min, max, mean, delta and sd of evenness
MatE$min <- apply(MatE[, 1:8], 1, min)
MatE$max <- apply(MatE[, 1:8], 1, max)
MatE$mean <- apply(MatE[, 1:8], 1, mean)
MatE$delta <- MatE$max - MatE$min
MatE$sd <- apply(MatE[, 1:8], 1, sd)

MatE <- as_tibble(MatE) %>% 
  mutate(date = ficdates, year = rep(1:nyears, each = 365))
