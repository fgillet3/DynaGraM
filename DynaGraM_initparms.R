initialize.parameters <- function(
  
  # ========= INITIAL VALUES ===========
  
  # Initial total biomass
  iniB = 300,       # kg ha-1
  
  # ========= PLANT SPECIES-SPECIFIC PARAMETERS ===========
  
  # Specific leaf area (m2 g-1)
  SLA = parasel$SLA,
  # Proportion of  biomass removed by mowing (%)
  lambda = parasel$lambda,
  # Absolute cattle grazing preferences (forage value 1-9)
  rho = parasel$rho,
  # Trampling tolerance indicator value (1-9)
  sigma = parasel$sigma,
  # Leaf nitrogen content (kg N kg-1 DM)
  delta = parasel$delta,
  # Half-saturation constant for N resource (kg N ha-1)
  k = parasel$k,
  # Leaf death rate (d-1)
  mu = parasel$mu,
  # Minimum temperature for growth (°C)
  T1 = parasel$T1,
  # Sensitivity to drought stress (-)
  wtol = parasel$wtol,
  
  # ========= PLANT GROWTH PROCESS PARAMETERS ===========
  
  # Base temperature for light utilization and development (°C)
  T0 = 3,
  # Higher limit of optimum of daily temperature for photosynthesis (°C)
  T2 = 20,
  # Maximal temperature for light utilization and development (°C)
  T3 = 35,     
  # Maximum radiation use efficiency (g DM MJ-1)
  RUEm = 3,
  # Proportion of laminae in green biomass [-]
  LAM = 0.62,
  # Extinction coefficient [-]
  alpha = 0.6,
  # Mineral nitrogen consumption Holling coefficient [-]
  nu = 2,
  # Holling coefficient for green biomass removal by grazing [-]
  ng = 2,       # Need to be >= 2
  # Amplification exponent of the control function f(X) of trampling [-]
  Ttol = 2,     # possibly 0.5
  # Seasonal effect on plant growth: threshold value - lower bound [°C d]
  ST1 = 625,
  # Seasonal effect on plant growth: threshold value - upper bound [°C d]
  ST2 = 1300,
  # Seasonal effect on plant growth: maximum of intensity [-]
  maxSEA = 1.33,
  # Seasonal effect on plant growth: minimum of intensity [-]
  minSEA = 0.67,
  # Adjustment coefficient to plant growth [-]
  corg = 0.75,
  # Adjustment coefficient to plant growth [-]
  cmin = 0.75,
  # Seasonal effect on biomass senescence: lower bound when senescence starts increasing
  Psi1 = 775,
  # Seasonal effect on biomass senescence: upper bound when senescence stops increasing
  Psi2 = 3000,
  # Seasonal effect on biomass senescence: minimal senescence coefficient at beginning of season
  omega1 = 1,
  # Seasonal effect on biomass senescence: maximal senescence coefficient at end of season
  omega2 = 3,
  # Minimal biomass remaining after any disturbance (kg DM ha-1)
  Bmin = 1.5,
  
  # ========= SOIL PARAMETERS ===========
  
  # Nitrogen mineralization rate (d-1)
  theta = soil$theta,
  # Maximal amount of Nm stored in the soil before lixiviation (kg N ha-1)
  Nmmax = soil$Nmmax, # Calanca, 2016, set Nred = 0.85
  # Delay to withdraw exceeding Nm after reaching the maximal amount
  tau = 2,
  # Volumetric soil water content at field capacity	(mm)
  WHC = soil$WHC, # Value from Calanca, 2016
  # Volumetric soil water content at permanent wilting point (mm)
  PWP = soil$PWP, # Value from Calanca, 2016
  # Critical temperature for maximal mineralization rate of No to Nm (°C)
  Tm1 = 40,
  # Empirical parameter describing effect of temperature on mineralization (°C)
  Tm2 = 31.79,
  # Empirical parameter describing effect of temperature on mineralization [-]
  gT = 3.36,
  # Empirical parameter describing effect of soil water on mineralization [-]
  gW1 = 1,
  # Empirical parameter describing effect of soil water on mineralization [-]
  gW2 = 6.63,
  # Empirical parameter describing effect of soil water on mineralization [-]
  gW3 = -5.69,
  # Maximal measured value of PET	 (mm d-1)
  PETmax = pmax(max(weather$PET), 8),
  # Empirical parameter for fitting the water reducer function [-]
  beta1 = 6.467,
  # Empirical parameter for fitting the water reducer function [-]
  beta2 = 7.623e-8,
  # Empirical parameter for reduction factor at high daily radiation [m2 d MJ-1]
  gamma1 = 0.0445,
  # Empirical parameter for reduction factor at high daily radiation [MJ m-2 d-1]
  gamma2 = 5,
  
  # ========= MANAGEMENT PARAMETERS ===========
  
  # Maximal possible livestock density (ABU ha-1)
  SDm = 2,
  # Parameter in the grazing function
  ksi = 1,
  # Herb biomass daily consumption of 1 adult bovine unit (kg DM ABU-1 d-1)
  kappa = 22,
  # Rate of mineral nitrogen restitution by cattle urine deposition (kg N ABU-1 d-1)
  Nuri = 0.315,
  # Rate of organic nitrogen restitution by cattle dung deposition (kg N ABU-1 d-1)
  Ndun = 0.236
  
) {
  return(
    list(
      RUEm = RUEm,
      LAM = LAM,
      alpha = alpha,
      nu = nu,
      T0 = T0,
      T2 = T2,
      T3 = T3,
      Tm1 = Tm1,
      Tm2 = Tm2,
      gT = gT,
      gW1 = gW1,
      gW2 = gW2,
      gW3 = gW3,
      beta1 = beta1,
      beta2 = beta2,
      gamma1 = gamma1,
      gamma2 = gamma2,
      theta = theta,
      Nmmax = Nmmax,
      WHC = WHC,
      PWP = PWP,
      PETmax = PETmax,
      Ndun = Ndun,
      Nuri = Nuri,
      SDm = SDm,
      ksi = ksi,
      Bmin = Bmin,
      SLA = SLA,
      k = k,
      delta = delta,
      mu = mu,
      T1 = T1,
      wtol = wtol,
      kappa = kappa,
      lambda = lambda,
      rho = rho,
      sigma = sigma,
      ng = ng,
      Ttol = Ttol,
      iniB = iniB,
      ST1 = ST1,
      ST2 = ST2,
      maxSEA = maxSEA,
      minSEA = minSEA,
      corg = corg,
      cmin = cmin,
      Psi1 = Psi1,
      Psi2 = Psi2,
      omega1 = omega1,
      omega2 = omega2,
      tau = tau
    )
  )
}