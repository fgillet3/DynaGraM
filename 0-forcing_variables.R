# Climate forcing variables
weather <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  Temp = rep(weather1y$Temp, nyears),
  PAR = rep(weather1y$PAR, nyears),
  P = rep(weather1y$P, nyears),
  PET = rep(weather1y$PET, nyears),
  SumTemp = rep(weather1y$SumTemp, nyears)
)

# Abandonment scenario
G0M0F0 <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  SD = rep(G0M0F0y1$SD, nyears),
  Nmf = rep(G0M0F0y1$Nmf, nyears),
  Nof = rep(G0M0F0y1$Nof, nyears),
  mdays = rep(G0M0F0y1$mdays, nyears)
)

# Extensive grazing scenario
G1M0F0 <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  SD = rep(G1M0F0y1$SD, nyears),
  Nmf = rep(G1M0F0y1$Nmf, nyears),
  Nof = rep(G1M0F0y1$Nof, nyears),
  mdays = rep(G1M0F0y1$mdays, nyears)
)

# Intensive grazing scenario
G2M0F1 <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  SD = rep(G2M0F1y1$SD, nyears),
  Nmf = rep(G2M0F1y1$Nmf, nyears),
  Nof = rep(G2M0F1y1$Nof, nyears),
  mdays = rep(G2M0F1y1$mdays, nyears)
)

# Extensive mowing scenario
G0M1F1 <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  SD = rep(G0M1F1y1$SD, nyears),
  Nmf = rep(G0M1F1y1$Nmf, nyears),
  Nof = rep(G0M1F1y1$Nof, nyears),
  mdays = rep(G0M1F1y1$mdays, nyears)
)

# Intensive mowing scenario
G0M2F2 <- data.frame(
  year = rep(1:nyears, each = 365),
  DOY = rep(1:365, nyears),
  DOS = times,
  day = ficdates,
  SD = rep(G0M2F2y1$SD, nyears),
  Nmf = rep(G0M2F2y1$Nmf, nyears),
  Nof = rep(G0M2F2y1$Nof, nyears),
  mdays = rep(G0M2F2y1$mdays, nyears)
)
