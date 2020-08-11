## Extensive grazing G1M0F0 ----

management <- G1M0F0
system.time(out1 <-
              ode(
                y = state,
                times = times,
                func = DynaGraM,
                parms = parms,
                method = "euler"
              ))
out <- as.data.frame(out1)
out$DOY <- rep(1:365, nyears)
out$year <- rep(1:nyears, each = 365)
out$Bt <- rowSums(out[, 5:11])
out$date <- ficdates
out$Wred8 <- apply(out[, 24:30], 1, mean)
out$Tred8 <- apply(out[, 31:37], 1, mean)
out$Nred8 <- apply(out[, 38:44], 1, mean)
out$Gred8 <- apply(out[, 45:51], 1, mean)
out$Ared8 <- apply(out[, 92:98], 1, mean)
# Inverse Simpson diversity
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
# Simpson evenness
out$Beve <- N2 / n
# Number of surviving species
N0 <- rowSums(out[, 5:11] > parms$Bmin)
# Survival ratio S
out$S <- N0 / n
# Pielou evenness
out$J1 <- diversity(out[, 5:11]) / log(n)
outEG120 <- out


## Intensive grazing G2M0F1 ----

management <- G2M0F1
system.time(out1 <-
              ode(
                y = state,
                times = times,
                func = DynaGraM,
                parms = parms,
                method = "euler"
              ))
out <- as.data.frame(out1)
out$DOY <- rep(1:365, nyears)
out$year <- rep(1:nyears, each = 365)
out$Bt <- rowSums(out[, 5:11])
out$date <- ficdates
out$Wred8 <- apply(out[, 24:30], 1, mean)
out$Tred8 <- apply(out[, 31:37], 1, mean)
out$Nred8 <- apply(out[, 38:44], 1, mean)
out$Gred8 <- apply(out[, 45:51], 1, mean)
out$Ared8 <- apply(out[, 92:98], 1, mean)
# Inverse Simpson diversity
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
# Simpson evenness
out$Beve <- N2 / n
# Number of surviving species
N0 <- rowSums(out[, 5:11] > parms$Bmin)
# Survival ratio S
out$S <- N0 / n
# Pielou evenness
out$J1 <- diversity(out[, 5:11]) / log(n)
outIG120 <- out


## Extensive mowing G0M1F1 ----

management <- G0M1F1
system.time(out1 <-
              ode(
                y = state,
                times = times,
                func = DynaGraM,
                parms = parms,
                method = "euler"
              ))
out <- as.data.frame(out1)
out$DOY <- rep(1:365, nyears)
out$year <- rep(1:nyears, each = 365)
out$Bt <- rowSums(out[, 5:11])
out$date <- ficdates
out$Wred8 <- apply(out[, 24:30], 1, mean)
out$Tred8 <- apply(out[, 31:37], 1, mean)
out$Nred8 <- apply(out[, 38:44], 1, mean)
out$Gred8 <- apply(out[, 45:51], 1, mean)
out$Ared8 <- apply(out[, 92:98], 1, mean)
# Inverse Simpson diversity
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
# Simpson evenness
out$Beve <- N2 / n
# Number of surviving species
N0 <- rowSums(out[, 5:11] > parms$Bmin)
# Survival ratio S
out$S <- N0 / n
# Pielou evenness
out$J1 <- diversity(out[, 5:11]) / log(n)
outEM120 <- out


## Intensive mowing G0M2F2 ----

management <- G0M2F2
system.time(out1 <-
              ode(
                y = state,
                times = times,
                func = DynaGraM,
                parms = parms,
                method = "euler"
              ))
out <- as.data.frame(out1)
out$DOY <- rep(1:365, nyears)
out$year <- rep(1:nyears, each = 365)
out$Bt <- rowSums(out[, 5:11])
out$date <- ficdates
out$Wred8 <- apply(out[, 24:30], 1, mean)
out$Tred8 <- apply(out[, 31:37], 1, mean)
out$Nred8 <- apply(out[, 38:44], 1, mean)
out$Gred8 <- apply(out[, 45:51], 1, mean)
out$Ared8 <- apply(out[, 92:98], 1, mean)
# Inverse Simpson diversity
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
# Simpson evenness
out$Beve <- N2 / n
# Number of surviving species
N0 <- rowSums(out[, 5:11] > parms$Bmin)
# Survival ratio S
out$S <- N0 / n
# Pielou evenness
out$J1 <- diversity(out[, 5:11]) / log(n)
outIM120 <- out


## Abandonment G0M0F0 ----

management <- G0M0F0
system.time(out1 <-
              ode(
                y = state,
                times = times,
                func = DynaGraM,
                parms = parms,
                method = "euler"
              ))
out <- as.data.frame(out1)
out$DOY <- rep(1:365, nyears)
out$year <- rep(1:nyears, each = 365)
out$Bt <- rowSums(out[, 5:11])
out$date <- ficdates
out$Wred8 <- apply(out[, 24:30], 1, mean)
out$Tred8 <- apply(out[, 31:37], 1, mean)
out$Nred8 <- apply(out[, 38:44], 1, mean)
out$Gred8 <- apply(out[, 45:51], 1, mean)
out$Ared8 <- apply(out[, 92:98], 1, mean)
# Inverse Simpson diversity
N2 <- diversity(out[, 5:11], "inv")
N2[is.infinite(N2)] <- 0
# Simpson evenness
out$Beve <- N2 / n
# Number of surviving species
N0 <- rowSums(out[, 5:11] > parms$Bmin)
# Survival ratio S
out$S <- N0 / n
# Pielou evenness
out$J1 <- diversity(out[, 5:11]) / log(n)
outAb120 <- out


## Save model outputs (large file!) ----
save(outEG120, outIG120, outEM120, outIM120, outAb120, file = "Outputs120.RData")

# Save model outputs in separate files fo each scenario
save(outEG120, file = "Outputs120EG.RData")
save(outIG120, file = "Outputs120IG.RData")
save(outEM120, file = "Outputs120EM.RData")
save(outIM120, file = "Outputs120IM.RData")
