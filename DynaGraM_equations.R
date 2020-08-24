DynaGraM <- function(t, state, parms) {
  with(as.list(parms), {
    # Reconstruct state variables
    state <- unname(state)
    No <- state[1]
    Nm <- state[2]
    W  <- state[3]
    B <- state[4:(n + 3)]
    
    # Control variables
    Temp <- weather[, 5][t]
    PAR <- weather[, 6][t]
    P <- weather[, 7][t]
    PET <- weather[, 8][t]
    SumTemp <- weather[, 9][t]
    
    SD <- management[, "SD"][t]
    Nmf <- management[, "Nmf"][t]
    Nof <- management[, "Nof"][t]
    mdays <- management[, "mdays"][t]
  
    # Auxiliary variables
    Tmin <- exp(gT * (Temp - Tm1) / (Temp + Tm2))
    Wmin <- 1 / (gW1 + gW2 * exp(gW3 * (W - PWP) / (WHC - PWP)))
    Nr <- max(Nm - Nmmax, 0) / tau
    LAI <- SLA * (B / 10) * LAM
    LAIt <- sum(LAI)
    RUE <- RUEm * pmax(0, pmin(1, 1 - gamma1 * (PAR - gamma2)))
    Lintercept <- LAI / LAIt
    grm <- 10 * PAR * Lintercept * RUE * (1 - exp(-alpha * LAIt))
    
    # Nred: reducer of plant growth by N
    Nred <- pmin(pmax(( (Nm ^ nu) / ((k ^ nu) + (Nm ^ nu)) ), 0), 1)
    
    # AET: Transpiration and evaporation processes
    PTr <- PET * min(1, 1 * sum(LAI) / 3)
    PEv <- PET - PTr
    evapo <- pmax(0, pmin(1, PET / PETmax))
    WredPTr <- pmax(0, ((1 - exp(-(
      evapo * beta2 + (1 - evapo) * beta1
    ) * pmax(0, W - PWP) / (WHC - PWP))) / (1 - exp(-(
      evapo * beta2 + (1 - evapo) * beta1
    )))))
    ATr <- PTr * WredPTr
    AEv <- PEv * max(min(W / WHC, 1), 0)
    AET <- min(W, ATr + AEv)
    
    # Delta: drainage of exceeding water
    Wr <- max(W + P - AET - WHC, 0)
    
    # Wred: reducer of plant growth by water
    Wred <-
      pmax(0, ((1 - exp(-(
        evapo * beta2 + (1 - evapo) * beta1
      ) * pmax(0, W - PWP) / (WHC - PWP))) / (1 - exp(-(
        evapo * beta2 + (1 - evapo) * beta1
      )))) ^ wtol)

    # Restitutions by cattle
    Nd1 <- Ndun * SD
    Nu1 <- Nuri * SD
    
    # Species-specific functions
    Tred <- rep(1, n)
    Gred <- rep(1, n)
    graz <- rep(0, n)
    mown <- rep(0, n)

    for (i in 1:n) {
      Tred[i] <-
        ifelse(Temp <= T0, 0, ifelse(Temp <= T1[i],
                                     (Temp - T0) / (T1[i] - T0),
                                     ifelse(Temp <= T2, 1, ifelse(
                                       Temp <= T3, (T3 - Temp) / (T3 - T2), 0
                                     ))))
      graz[i] <-
        max(0, kappa * SD * (rho[i] * B[i] ^ ng) / (ksi + sum((rho * B ^ ng))))
      mown[i] <- mdays * lambda[i] * B[i]
      Gred[i] <-
        ((1 - sigma[i] / 9) * (SDm - SD) / SDm + sigma[i] / 9) ^ Ttol
    }
    
    # Seasonal effect (SEA) on plant growth and on senescence driven by sum of temperature
    SEA <- ifelse(SumTemp <= 200, minSEA,
                  ifelse(
                    SumTemp <= ST1 - 200,
                    ((SumTemp - 200) * (maxSEA - minSEA) / (ST1 - 400) + minSEA),
                    ifelse(SumTemp <= (ST1 - 100),
                           maxSEA,
                           ifelse(SumTemp <= ST2,
                                  ((SumTemp - ST2) * (minSEA - maxSEA) /
                                     (ST2 - (ST1 - 100)) + minSEA
                                  ),
                                  minSEA))
                  ))
    
    SEAsen <-  ifelse(SumTemp <= Psi1,
                      omega1,
                      ifelse(
                        SumTemp >= Psi2,
                        omega2,
                        omega1 + (omega2 - omega1) * (SumTemp - Psi1) / (Psi2 - Psi1)
                      ))
    
    # Balance of plant growth
    Reducer <- Nred * Tred * Wred * Gred
    gr <- grm * Reducer * SEA
    netgr <- sum(gr - mu * SEAsen * B)
    
    # Amount of N transfer to the soil from dead leaves
    NoSupply <- corg * sum(delta * mu * SEAsen * B)
    
    # Derivatives of state variables
    dNo <-
      max(corg * sum(delta * mu * SEAsen * B) + Nof + Nd1 - theta * Tmin * Wmin * No, -No)
    dNm <-
      max(theta * Tmin * Wmin * No + Nmf + Nu1 - Nr - cmin * sum(delta * gr), -Nm)
    dW <- max(P - AET - Wr, -W)
    dB <- pmax(gr - mu * SEAsen * B - graz - mown, -B + Bmin)
    
    netMin <- theta * Tmin * Wmin * No # Mineral N acquisition = amount of No mineralized per day
    ReducerNmin <- Tmin * Wmin # Reducer of N mineralization 
    Nuptake <- sum(delta * gr) # Mineral N uptake = amount of Nm consumed by plants per day
    grazed <- sum(graz)
    potgrowth <- 10 * PAR * 1 *RUEm * (1 - exp(-alpha * LAIt))
    potgrowthi <- 10 * PAR * Lintercept  *RUEm * (1 - exp(-alpha * LAIt))

    list(
      c(dNo, dNm, dW, dB),  # Order needs to be equal to "state"!
      graz = graz,
      Temp = Temp,
      P = P,
      PAR = PAR,
      PET = PET,
      AET = AET,
      Wred = Wred,
      Tred = Tred,
      Nred = Nred,
      Gred = Gred,
      Nmf = Nmf,
      Nu1 = Nu1,
      mown = mown,
      LAIt = LAIt,
      LAI = LAI,
      netMin = netMin,
      grazed = grazed,
      dB = dB,
      gr = gr,
      grm = grm,
      Reducer = Reducer,
      RUE = RUE,
      Nuptake = Nuptake,
      Tmin = Tmin,
      Wmin = Wmin,
      ReducerNmin = ReducerNmin,
      netgr = netgr,
      SEA = SEA,
      NoSupply = NoSupply,
      SEAsen = SEAsen,
      potgrowth = potgrowth,
      potgrowthi = potgrowthi
    )
  })
}