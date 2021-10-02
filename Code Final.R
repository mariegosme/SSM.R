# ---- function which returns the proportion of the daily N transfer from the leaves ------

#' Title : function which returns the proportion of the daily N transfer from the leaves 
#'
#' @param lai numeric vector of LAI values.
#' @param slns vector which contains values of the specific leaf nitrogen in senesced leaves(g N m-2).
#' @param sncs vector which contains values of the minimum stem N concentration (g N g−1).
#' @param TRLN vector calculated with fComputeNAvailableatBeginningseedFill.
#'
#' @return vector of the same length as lai which returns the the proportion of the daily N transfer from the leaves.
#' @export
#'
#' @examples 
#' 
fComputeDailyNTransferFromleaves <- function(lai, slng, sncs, TRLN){
  return (lai * (slng-sncs)/TRLN)
  # Equation 17.5 on the PDF, p228
}


# ---- function which returns the estimate of supply vegs -------

fEstimateOfSupplyVegs <- function(cBSGMobilizable,sBdFromSowingToTerminationLeaf,sBdFromSowingToSeedGrowth, bd,cDailySeedWeightIncrease,cUptakeNGrain,pGrainMinConcentrationN,pGrainMaxConcentrationN, cDemandNAccumulation){
  ## bd not defined in all-variables, but calculated previously : bd = ppfun*verfun*tempfun
  cEstimateSupplyVeg <- cBSGMobilizable/(sBdFromSowingToTerminationLeaf-sBdFromSowingToSeedGrowth)*bd
  SeedGrowthCond <- (cDailySeedWeightIncrease == 0)
  cEstimateSupplyVeg[SeedGrowthCond] <- 0
  cDailySeedsNDemands <- pmax(cDailySeedWeightIncrease*pGrainMinConcentrationN,pmin(cEstimateSupplyVeg + cDemandNAccumulation*cUptakeNGrain,cDailySeedWeightIncrease*pGrainMaxConcentrationN))
  
  ###   If INGRN < (SGR * GNCmin) Then
  ###     INGRN = SGR * GNCmin
  ###   ElseIf INGRN >= (SGR * GNCmax) Then
  ###     INGRN = SGR * GNCmax
  
  cEstimateSupplyVeg <- cDailySeedsNDemands - cDemandNAccumulation
  return (cEstimateSupplyVeg) 
}  



# ---- function which returns the Daily demand for N accumulation when the cumulative biological days or less than the biological day before N fixation----


fDailyNDemandBeforNFixation <- function(cDemandNAccumulation ,sTotalAvailableUptakeN){
  return (pmin(cDemandNAccumulation, sTotalAvailableUptakeN))
  # If sBiologicalDaysSinceSowing < sBdFromSowingToFixation , the Daily demand for N accumulation is limited by sTotalAvailableUptakeN)
  #----------------Filtre à appliquer dans le Excel--------------- #
  
  # DailyNDemandBeforNFixation.filter is.before('EMR',3,5)
}


# ---- function which apply during Vegetative Growth, which means between phase BNF (EMR + 3,5) and (R5) -----

#' Title function which apply during Vegetative Growth, which means between phase BNF (EMR + 3,5) and (R5)
#'

#' @param pSpecLeafNSenescenceLeaf (SLNS) : numeric vector which contains values of specific leaf nitrogen in senesced leaves (minimum)
#' @param pSpecLeafNGreenLeaf (SLNG) : numeric vector which contains values of the specific leaf nitrogen in green leaves (g N m-2).
#' @param pStemMinimumNconcentration (SNCS) : numeric vector which contains values of the minimum stem N concentration (g N g−1).
#' @param pSNCG (pSNCG) : numeric vector which contains N content per unit stem weight (g N g-1).
#' @param pNUPmax  (MXNUP) : numeric vector which contains values of NUP maximum rate (g N m-2 d-1)
#' @param cDailyStemWeightIncrease (GST) : numeric vector which contains values of Daily increase in stem mass (g m−2 day−1 )
#' @param cFTSWRrootZone (FTSWRZ) : numeric vector which contains values of the FTSW in the root zone ([0,1])
#' @param sAccumulatedStemDryMatter (WST) : numeric vector which contains values of the cumulative stem dry matter (g m-2)
#' @param sTotalAvailableUptakeN (SNAVL) : numeric vector which contains values of the Total amount of N that is accessible to plant for uptake in the soil (sum of each layer)(g m-2)
#' @param sAccumulatedVegetativeDryMatter (WVEG) : numeric vector which contains values of cumulative vegetative organs dry matter (g m−2 )
#' @param sLAI (LAI) : numeric vector which contains values of Leaf area index (m2 m-2)

#'
#' @return 4 numeric vectors which contains values of the accumulated nitrgen in stem, the accumulated nitrogen in leaves, the total accumulated nitrogen in stem and leaves and the accumulated nitrogen in grain (g N m-2)
#' @export
#'
#' @examples 
fNFixationDuringVegetativeGrowth <- function(sAccumulatedVegetativeDryMatter, sAccumulatedStemDryMatter,pSNCG, cGrowthLAI, pSpecLeafNGreenLeaf, pNUPmax, sTotalAvailableUptakeN,cDailyStemWeightIncrease,cFTSWRrootZone,pStemMinimumNconcentration, sLAI, pSpecLeafNSenescenceLeaf, cDryMatterProduction){
  
  #cDailyStemWeightIncrease <- rep(0.5, length(pNUPmax))
  
  WSXF <- rep(1,length(pNUPmax))
  # Waiting to have more information about WSXF (ICICICICICICI)
  sNitrogenAccumulation <- rep(1,length(pNUPmax))
  # Value initialized at 1 outside the function, don't know why this because used in a multiplication
  sAccumulatedLeafNitrogen <- sLAI*pSpecLeafNGreenLeaf
  # Initialization of NLF : NLF <- LAI * SLNG
  sAccumulatedNStem <- sAccumulatedStemDryMatter * pSNCG
  # Initialization of NST : NST <- WST * pSNCG
  sAccumulatedNGrain <- numeric(length(sAccumulatedStemDryMatter))
  # Initialization of NGRN : Vector of 0 of the length of sAccumulatedStemDryMatter
  cCoefBiologicalNFixation <- numeric(length(sAccumulatedStemDryMatter))
  # Arbitrary initialization of NFC cCoefBiologicalNFixation : Not done in the VBA code (ICICICICICICIC)
  # Choice to initialize at 0
  cCoefBiologicalNFixation <- numeric(length(sAccumulatedStemDryMatter))
  
  
  
  cDailySeedsNDemands <- numeric(length(sAccumulatedStemDryMatter))
  cDemandResultDeficiencies <- pmax((sAccumulatedStemDryMatter * pSNCG) - sAccumulatedNStem, 0)
  cDemandNAccumulation <- pmin((cDailyStemWeightIncrease * pSNCG) + (cGrowthLAI * pSpecLeafNGreenLeaf) + cDemandResultDeficiencies, pNUPmax)
  #cCoefBiologicalNFixation <- cCoefBiologicalNFixation*(3/4) + (cDemandNAccumulation/sAccumulatedVegetativeDryMatter)*(1/4) ### ICICICIICICIC pas de sens physique à cette équation
  ### 3/4 and 1/4 seem to be like ponderation coefficients
  cDemandNAccumulation <- pmax(cDemandNAccumulation, 0) 
  
  
  ConditionFTSWRZ <- cFTSWRrootZone > 1
  cDemandNAccumulation[ConditionFTSWRZ] <- cDemandNAccumulation[ConditionFTSWRZ]*WSXF[ConditionFTSWRZ]
  
  
  ConditionDDMP <- cDryMatterProduction == 0
  cDemandNAccumulation[ConditionDDMP] <- 0
  
  # cDemandNAccumulation <- pmin(cDemandNAccumulation,sTotalAvailableUptakeN) for non legume here, but we start our model on chickpea 
  
  
  ## cBiologicalNFixation : BNF, used to calculate the cumulative nitrogen fixation (BNF)
  ## cCoefBiologicalNFixation : NFC, but not coded here 
  
  
  cBiologicalNFixation <- numeric(length(sAccumulatedStemDryMatter))
  LimitationCropUptake <- cDemandNAccumulation > sTotalAvailableUptakeN
  # Condition 
  cBiologicalNFixation[LimitationCropUptake] <- pmax(cDemandNAccumulation[LimitationCropUptake]-sTotalAvailableUptakeN[LimitationCropUptake],0)
  
  # ------------------------- Beginning of translation of the Diagramm of the PDF which describes the repartition of nitrogen exchanges during seed growth ------------------- #
  
  
  
  
  
  ## Initialisation for the calcul of the 4 principal variables : 
  
  sDailyAccumulationStemN <- sDailyRateNFromStem <- sDailyAccumulationLeavesN <- sDailyRateNfromLeave <- rep(0, length(sAccumulatedStemDryMatter))
  
  # sDailyAccumulationStemN : Daily rate of nitrogen accumulation in stems (INST)
  # sDailyAccumulationLeavesN : Daily rate of nitrogen accumulation in leaves (INLF)
  # sDailyRateNFromLeave : Daily rate of nitrogen mobilized from leaves (XNLF)
  # sDailyRateNFromStem : Daily rate of nitrogen mobilized from stems (XNST)
  
  
  
  
  StemDryMatterCondition <- sAccumulatedNStem < sAccumulatedStemDryMatter*pStemMinimumNconcentration
  
  # StemDryMatterCondition = First Condition : NST < WST * SNCS
  
  sDailyAccumulationLeavesN[!StemDryMatterCondition] <- cGrowthLAI[!StemDryMatterCondition]*pSpecLeafNGreenLeaf[!StemDryMatterCondition]
  sDailyAccumulationStemN [StemDryMatterCondition] <- sAccumulatedStemDryMatter[StemDryMatterCondition]*pStemMinimumNconcentration[StemDryMatterCondition]-sAccumulatedNStem[StemDryMatterCondition]
  
  # NLeafGreaterThanNNeeded = 2nd Condition : INLF > NUP and the first condition is not satisfied
  # NLeafSmallerThanNNeeded = 3nd Condition : INLF < NUP and the first condition is not satisfied
  
  NLeafGreaterThanNNeeded <- !StemDryMatterCondition & sDailyAccumulationLeavesN > cDemandNAccumulation
  NLeafSmallerThanNNeeded <- !StemDryMatterCondition & sDailyAccumulationLeavesN <=cDemandNAccumulation
  sDailyAccumulationLeavesN [NLeafGreaterThanNNeeded]<- cDemandNAccumulation[NLeafGreaterThanNNeeded] + sDailyRateNFromStem[NLeafGreaterThanNNeeded]
  
  sDailyAccumulationStemN [NLeafSmallerThanNNeeded] <- cDemandNAccumulation[NLeafSmallerThanNNeeded]-sDailyAccumulationLeavesN [NLeafSmallerThanNNeeded]
  sDailyRateNFromStem[NLeafGreaterThanNNeeded] <- (min(sDailyAccumulationLeavesN  - cDemandNAccumulation, sAccumulatedNStem - sAccumulatedStemDryMatter * pStemMinimumNconcentration))[NLeafGreaterThanNNeeded]
  
  # NStemGreaterThanNNeeded = 4th condition : INST > NUP and the first condition is satisfied
  # NStemSmallerThanNNeeded = 5th condition : INST < NUP and the first condition is satisfied
  
  NStemGreaterThanNNeeded <- StemDryMatterCondition & sDailyAccumulationStemN >cDemandNAccumulation
  NStemSmallerThanNNeeded <- StemDryMatterCondition & sDailyAccumulationStemN <= cDemandNAccumulation
  sDailyAccumulationLeavesN [NStemSmallerThanNNeeded ]<-(min(cGrowthLAI*pSpecLeafNGreenLeaf, cDemandNAccumulation-sDailyAccumulationStemN))[NStemSmallerThanNNeeded ]
  sDailyRateNfromLeave[NStemGreaterThanNNeeded ]<-(sDailyAccumulationStemN -cDemandNAccumulation)[NStemGreaterThanNNeeded]
  sDailyAccumulationStemN [NStemSmallerThanNNeeded ]<-(cDemandNAccumulation-sDailyAccumulationLeavesN )[NStemSmallerThanNNeeded]
  
  
  
  
  return (data.frame(INST = sDailyAccumulationStemN ,XNST = sDailyRateNFromStem,INLF = sDailyAccumulationLeavesN,XNLF = sDailyRateNfromLeave, BNF = cBiologicalNFixation, INGRN = cDailySeedsNDemands, NUP = cDemandNAccumulation))
  # Return 6 vectors which contains the values of : INST (sDailyAccumulationStemN), XNST (sDailyRateNFromStem), INLF (sDailyAccumulationLeavesN), XNLF (sDailyRateNFromLeave), BNF (cBiologicalNFixation) and INGRN (cDailySeedsNDemands)
  # Values used to updated cumulated variables NST, NLF, NGRN and CUMBNF in the procedure
  
  #----------------Filtre à appliquer dans le Excel--------------- #
  
  #  is.after('EMR', 3.5) & is.before('R5')     
}


#---------------------- Function which occurs during seed growth, which means between phase R5 and R7 ---------------------#

#' Title function which returns the accumulated nitrogen in the stem, leaves and grain each day

#' @param pStemMinimumNconcentration (SNCS) : numeric vector which contains values of the minimum stem N concentration (g N g−1).
#' @param pGrainConversionCoefficient (GCC) : numeric vector which contains values of ratio of energy content of vegetative tissues to that of grain (g g−1 )
#' @param pSNCG (pSNCG) : numeric vector which contains N content per unit stem weight (g N g-1).
#' @param pSpecLeafNGreenLeaf (SLNG) : numeric vector which contains values of the specific leaf nitrogen in green leaves (g N m-2).
#' @param pGrainMaxConcentrationN (GNCmax) : numeric vector which contains valuesof grain max N concentration (g N g-1)
#' @param pGrainMinConcentrationN (GNCmin) : numeric vector which contains values of grain min N Concentration (g N g-1)
#' @param pNUPmax (MXNUP) : numeric vector which contains values of NUP maximum rate (g N m-2 d-1)
#' @param cGrowthLAI (GLAI) : numeric vector which contains values of the Daily increase (growth) in leaf area index (m2 m-2 d-1)
#' @param cDailySeedWeightIncrease (SGR) : numeric vector which contains values of the daily seed growth (g m−2 day−1)
#' @param cCoefBiologicalNFixation (NFC) : numeric vector which contains values of coefficient of biological nitrogen fixation per unit mass
#' @param cCoefWaterStressSaturation (WSXF) : numeric vector which contains values of Water Stress from soil saturation ([0,1])
#' @param cDailyStemWeightIncrease (GST) : numeric vector which contains values of Daily increase in stem mass (g m−2 day−1 )
#' @param cDryMatterProduction (DDMP) : numeric vector which contains values of Daily dry matter production (g m-2 d-1)
#' @param cFTSWrootZone (FTSWRZ) : numeric vector which contains values of the FTSW in the root zone ([0,1])
#' @param sTotalAvailableUptakeN (SNAVL) : numeric vector which contains values of the Total amount of N that is accessible to plant for uptake in the soil (sum of each layer)(g m-2)
#' @param sAccumulatedStemDryMatter (WST) : numeric vector which contains values of the cumulative stem dry matter (g m-2)
#' @param sLAI (LAI) : numeric vector which contains values of Leaf area index (m2 m-2)
#' @param sAccumulatedVegetativeDryMatter (WVEG) : numeric vector which contains values of cumulative vegetative organs dry matter (g m−2 )
#'
#' @return 4 numeric vectors which contains values of the accumulated nitrgen in stem, the accumulated nitrogen in leaves, the total accumulated nitrogen in stem and leaves and the accumulated nitrogen in grain (g N m-2)
#' @export
#'
#' @examples

fNFixationDuringSeedGrowth <- function(cDailySeedWeightIncrease, pGrainMaxConcentrationN,pSNCG, cGrowthLAI, pSpecLeafNGreenLeaf, pNUPmax, sCoefBiologicalNFixation,sAccumulatedVegetativeDryMatter,cCoefWaterStressSaturation,cDailyStemWeightIncrease,cDryMatterProduction,pGrainConversionCoefficient,cFTSWRrootZone,sTotalAvailableUptakeN,sAccumulatedStemDryMatter,pGrainMinConcentrationN,sLAI,pStemMinimumNconcentration, fxlf){
  
  sCumulativeNFixation <- numeric(length(cDailySeedWeightIncrease))
  # Initialization of the cumulative 
  sNitrogenAccumulation <- rep(1,length(cDailySeedWeightIncrease))
  # WSFN = 1 in the VBA code, but no explanations and only used in water-stress factors
  sAccumulatedLeafNitrogen <- sLAI*pSpecLeafNGreenLeaf
  # Initialization of NLF : NLF <- LAI * SLNG
  sAccumulatedNStem <- sAccumulatedStemDryMatter * pSNCG
  # Initialization of NST : NST <- WST * pSNCG
  sAccumulatedNGrain <- numeric(length(sAccumulatedStemDryMatter))
  # Initialization of NGRN : Vector of 0 of the length of sAccumulatedStemDryMatter
  
  
  
  cDailySeedsNDemands <- cDailySeedWeightIncrease*pGrainMaxConcentrationN
  # cDailySeedWeightIncrease : SGR, daily seed growth 
  # First estimate using maximum GNC, maybe we could have tried to use minimum GNC sDailyAccumulationStemN ead, the pdf has a single constant value for GNC
  cDemandNAccumulation <- pmin(pmax(cDailySeedsNDemands + (cDailyStemWeightIncrease * pSNCG) + (cGrowthLAI * pSpecLeafNGreenLeaf),0),pNUPmax)
  # Daily demand for N accumulation can't be negative and is limited to a maximum.
  # cDailyStemWeightIncrease (GST) : cDailyStemWeightIncrease
  cUptakeNGrain <- cDailySeedsNDemands / (cDemandNAccumulation + 0.00000001)
  # No really information about the variable cUptakeNGrain (nup_fr_grn) in the book, not defined
  # No idea of the mean of the value 0.00000001
  
  #fBeforeNFixationOrNonLegum <- function(cRateBiologicalNFix){
  #cRateBiologicalNFix <- numeric(length(cRateBiologicalNFix))
  # before BNF activation OR for non-legum crops
  # DNF :cRateBiologicalNFix, Actual rate of biological nitrogen fixation
  # Fonction which occurs beforce N Fixation
  # }
  
  # ElseIf CBD >= bdBNF in the VBA version, but we already are in that case
  
  sPotentielRateNitrogenFixation <- pmin(sCoefBiologicalNFixation * sAccumulatedVegetativeDryMatter, cDemandNAccumulation)
  # No value initialized for NFC (sCoefBiologicalNFixation) in the VBA code
  # PDNF : Potential rate of biological nitrogen fixation, which is limited to the daily demand for N accumulation
  # for legum
  cRateBiologicalNFix <- sPotentielRateNitrogenFixation * sNitrogenAccumulation 
  #for legum
  
  DryMatterCondition <- (cDryMatterProduction <= cDailySeedWeightIncrease*pGrainConversionCoefficient | cDryMatterProduction == 0)
  cRateBiologicalNFix[DryMatterCondition] <- 0
  # For Legum
  
  
  ConditionFTSWRZ <- cFTSWRrootZone > 1
  cDemandNAccumulation[ConditionFTSWRZ]<- cDemandNAccumulation[ConditionFTSWRZ]*cCoefWaterStressSaturation[ConditionFTSWRZ]
  
  
  SeedGrowthCondition <- cDryMatterProduction < cDailySeedWeightIncrease/pGrainConversionCoefficient
  cDemandNAccumulation[DryMatterCondition] <- 0
  ## cDemandNAccumulation is set equal to 0 when daily dry matter production (DDMP) by the crop does not exceed SGR (Daily Seed Growth)
  
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  
  cDemandNAccumulation <- pmin(cDemandNAccumulation,sTotalAvailableUptakeN + cRateBiologicalNFix)
  cBiologicalNFixation <- pmax(cDemandNAccumulation - sTotalAvailableUptakeN, 0)
  
  # Variable updated at each time step 
  
  
  
  #----------------- Computation of the Nitrogen dispersal in leaves and stems during the vegetative Growth ------------- #
  
  
  ## Initialisation at 0 for the calcul of the 4 principal variables : 
  
  sDailyAccumulationStemN <- sDailyRateNFromStem <- sDailyAccumulationLeavesN <- sDailyRateNfromLeave <- rep(0, length(sAccumulatedStemDryMatter))
  
  # INST = XNST = INLF = XNLF = 0
  
  NUP2 <- cDemandNAccumulation - cDailySeedWeightIncrease*(pGrainMinConcentrationN+pGrainMaxConcentrationN)/2
  # The demand in Nitrogen seems to be adjusted in that case (NUP2 is the adjusted value)
  
  # ICICICICI : the code source uses a GNC value which is not used in the book or in the excel, i chose to use the mean between GNCmin and GNCmax to replace this value.
  
  GrainCondition <- (cDemandNAccumulation > cDailySeedsNDemands)
  
  # First condition : NUP > INGRN
  
  # If this first condition is not satisfied : 
  
  sDailyAccumulationLeavesN[!GrainCondition] <- 0
  sDailyAccumulationStemN[!GrainCondition] <- 0
  sDailyRateNfromLeave[!GrainCondition] <- ((cDailySeedsNDemands - cDemandNAccumulation)*fxlf)[!GrainCondition]
  sDailyRateNFromStem[!GrainCondition] <- ((cDailySeedsNDemands -  cDemandNAccumulation)*(1 - fxlf))[!GrainCondition]
  
  # FXLF : proportion of the daily N transfer from the leaves
  # FXLF is calculated thanks to another function below
  
  StemDryMatterCondition <- (GrainCondition) & (sDailyAccumulationStemN <= sAccumulatedStemDryMatter*pStemMinimumNconcentration )
  NoStemDryMatterCondition <- (GrainCondition) & (sDailyAccumulationStemN > sAccumulatedStemDryMatter*pStemMinimumNconcentration )
  
  # StemDryMatterCondition = 2nd condition : first condition is satisfied and NST <= WST * SNCS
  # NoStemDryMatterCondition = 3rd condition : first condition is satisfied and NST > WST * SNCS
  
  sDailyAccumulationStemN[StemDryMatterCondition] <- sAccumulatedStemDryMatter[StemDryMatterCondition] * pStemMinimumNconcentration [StemDryMatterCondition] - sDailyAccumulationStemN[StemDryMatterCondition]
  sDailyRateNFromStem[StemDryMatterCondition] <- 0 ### not necessary, just to be clear
  sDailyAccumulationLeavesN[NoStemDryMatterCondition] <- cGrowthLAI[NoStemDryMatterCondition]*pSpecLeafNGreenLeaf[NoStemDryMatterCondition]
  sDailyRateNfromLeave[NoStemDryMatterCondition] <- 0 #### Not necessary, just to be clear
  
  NStemGreaterThanNNeeded <-StemDryMatterCondition &  sDailyAccumulationStemN >= NUP2
  NStemSmallerThanNNeeded <- StemDryMatterCondition &  sDailyAccumulationStemN < NUP2
  
  # NStemGreaterThanNNeeded = 4th condition : 2nd condition is satisfied and INST >= NUP2
  # NStemSmallerThanNNeeded = 5th condition : 2nd condition is satisfied and INST < NUP2
  
  sDailyRateNfromLeave[ NStemGreaterThanNNeeded] <-  sDailyAccumulationStemN[ NStemGreaterThanNNeeded] - NUP2[ NStemGreaterThanNNeeded]
  sDailyAccumulationLeavesN[ NStemGreaterThanNNeeded] <- 0 #### Not necessary, just to be clear
  sDailyAccumulationLeavesN[ NStemSmallerThanNNeeded] <- (pmin(cGrowthLAI*pSpecLeafNGreenLeaf, NUP2- sDailyAccumulationStemN))[ NStemSmallerThanNNeeded]
  sDailyRateNfromLeave[ NStemSmallerThanNNeeded] <- 0 #### Not necessary, just to be clear
  
  NLeafGreaterThanNeeded <- NoStemDryMatterCondition &  sDailyAccumulationLeavesN >= NUP2
  NLeafSmallerThanNeeded <- NoStemDryMatterCondition &  sDailyAccumulationLeavesN < NUP2
  
  # NLeafGreaterThanNeeded = 6th condition : 2nd condition is unsatisfied and INLF >= NUP2
  # NLeafSmallerThanNeeded = 7th condition : 2nd condition is unsatisfied and INLF < NUP2
  
  sDailyAccumulationStemN[NLeafGreaterThanNeeded] <- 0 #### Not necessary, just to be clear
  sDailyRateNFromStem[NLeafGreaterThanNeeded] <-(pmin( sDailyAccumulationLeavesN - NUP2, sDailyAccumulationStemN - sAccumulatedStemDryMatter*pStemMinimumNconcentration ))[NLeafGreaterThanNeeded]
  sDailyAccumulationLeavesN[NLeafGreaterThanNeeded] <- NUP2[ NLeafGreaterThanNeeded] + sDailyRateNFromStem[ NLeafGreaterThanNeeded]
  sDailyAccumulationStemN[NLeafSmallerThanNeeded] <- NUP2[ NLeafSmallerThanNeeded] +  sDailyAccumulationLeavesN[ NLeafSmallerThanNeeded]
  
  
  # ------------------------------------------ end of computation -----------------------
  
  return (data.frame(INST = sDailyAccumulationStemN ,XNST = sDailyRateNFromStem,INLF = sDailyAccumulationLeavesN,XNLF = sDailyRateNfromLeave, BNF = cBiologicalNFixation, INGRN = cDailySeedsNDemands, NUP = cDemandNAccumulation))
  # Return 6 vectors which contains the values of : INST (sDailyAccumulationStemN), XNST (sDailyRateNFromStem), INLF (sDailyAccumulationLeavesN), XNLF (sDailyRateNFromLeave), BNF (cBiologicalNFixation) and INGRN (cDailySeedsNDemands)
  # Values used to updated cumulated variables NST, NLF, NGRN and CUMBNF in the procedure
}


rUpdatePlantNitrogen <-function(){
  
  
  crop_presence <- !(is.na(ALLDAYDATA$sCrop))
  # The code only applies when LAI is not NA, so we need to check it
  
  if(any(crop_presence)){
    
    crops <- ALLDAYDATA$sCropCult
    
    ## Listing of all parameters and variables needed in the functions
    
    cDailySeedWeightIncrease <- ALLDAYDATA$cDailySeedWeightIncrease[crop_presence] # to get SGR
    pSpecLeafNGreenLeaf <- ALLCROPS[crops, "pSpecLeafNGreenLeaf"][crop_presence] # to get SNLG 
    pSpecLeafNSenescenceLeaf <- ALLCROPS[crops, "pSpecleafNSenescenceLeaf"][crop_presence] # to get SNLS 
    pStemMinimumNconcentration <-ALLCROPS[crops, "pStemMinimumNconcentration"][crop_presence] # to get SNCS 
    sLAI <- ALLDAYDATA$sLAI[crop_presence] # to get LAI
    sAccumulatedStemDryMatter <- ALLDAYDATA$sAccumulatedStemDryMatter[crop_presence] # to get WST
    pGrainConversionCoefficient <- ALLCROPS[crops, "pGrainConversionCoefficient"][crop_presence] # to get GCC 
    pSNCG <- ALLCROPS[crops, "pSNCG"][crop_presence] # to get pSNCG #### ICICIC NUULLLL
    pGrainMaxConcentrationN <- ALLCROPS[crops,"pGrainMaxConcentrationN"][crop_presence] # to get GNCmax 
    pGrainMinConcentrationN  <- ALLCROPS[crops, "pGrainMinConcentrationN"][crop_presence] # to get GNCmin 
    pNUPmax <- ALLCROPS[crops, "pNUPmax"][crop_presence] # to get MXNUP 
    cGrowthLAI <- ALLDAYDATA$cGrowthLAI[crop_presence] # to get GLAI
    sCoefBiologicalNFixation <- ALLDAYDATA$sCoefBiologicalNFixation[crop_presence] # to get NFC
    cCoefWaterStressSaturation <- ALLDAYDATA$cCoefWaterStressSaturation[crop_presence] # to get WSXF
    cDailyStemWeightIncrease <- ALLDAYDATA$cDailyStemWeightIncrease[crop_presence] # to get GST
    cDryMatterProduction <- ALLDAYDATA$cDryMatterProduction[crop_presence] # to get DDMP
    cFTSWRrootZone <- ALLDAYDATA$cFTSWRrootZone[crop_presence] # to get FTSWRZ
    sTotalAvailableUptakeN <- ALLDAYDATA$sTotalAvailableUptakeN[crop_presence] # to get SNAVL
    sAccumulatedStemDryMatter <- ALLDAYDATA$sAccumulatedStemDryMatter[crop_presence] # to get WST
    sAccumulatedVegetativeDryMatter <- ALLDAYDATA$sAccumulatedVegetativeDryMatter[crop_presence] # to get WVEG
    sBiologicalDay  <- ALLDAYDATA$sBiologicalDay[crop_presence] # to get biological days
    
    
    ## Listing of variables needed in the procedure to calcul cumulative variables
    
    sNitrogenContent <- ALLDAYDATA$sNitrogenContent[crop_presence] # to get CUMBNF
    sAccumulatedNStem <- ALLDAYDATA$sAccumulatedNStem[crop_presence] # to get NST
    sAccumulatedLeafNitrogen <- ALLDAYDATA$sAccumulatedLeafNitrogen[crop_presence] # to get NLF
    sAccumulatedNGrain <- ALLDAYDATA$sAccumulatedNGrain[crop_presence] # to get NGRN
    cTotalAccumulatedNitrogen <- ALLDAYDATA$cTotalAccumulatedNitrogen[crop_presence] # to get NVEG
    
    ## Creation of the filters 
    
    VegetativeGrowthFilter <- applyfilters("VegetativeGrowthPhase")
    SeedGrowthFilter <- applyfilters("SeedGrowthPhase")
    BeforeBNF <- applyfilters("BeforeNFixation")
    
    
    ## Calculation of intermediate variables for the 2 main functions
    
    TRLN <- fComputeNAvailableatBeginningseedFill(lai = sLAI,
                                                  slns = pSpecLeafNSenescenceLeaf, 
                                                  slng =  pSpecLeafNGreenLeaf, 
                                                  nst = sAccumulatedNStem , 
                                                  wst = sAccumulatedStemDryMatter, 
                                                  sncs = pStemMinimumNconcentration)
    
    # TRLNB is the Total mobilizable N available in the plant  and is used in the 2nd function
    
    FXLF <- fComputeDailyNTransferFromleaves(lai = sLAI,
                                             slng = pSpecLeafNGreenLeaf, 
                                             sncs = pStemMinimumNconcentration, 
                                             TRLN = TRLN)
    
    # FXLF is proportion of the daily N transfer from the leaves
    
    
    # Initialization of cDemandNAccumulation (NUP)
    cDemandNAccumulation <- rep(0,5)
    
    # 
    
    if (any(BeforeBNF)){
      cDemandNAccumulation[BeforeBNF] <- 0
      sNitrogenContent[BeforeBNF] <- 0
      sAccumulatedNStem[BeforeBNF] <- 0
      sAccumulatedLeafNitrogen[BeforeBNF] <- 0
      sAccumulatedNGrain[BeforeBNF] <- 0
      cTotalAccumulatedNitrogen[BeforeBNF] <- 0
      
    }
    
    
    
    
    ## Application of the filters on the 2 main functions 
    
    if (any(VegetativeGrowthFilter)){
      
      FixationVegetativeGrowth <- data.frame(INST <- rep(0,5), XNST <- rep(0,5), INLF <- rep(0,5), XNLF <- rep(0,5), BNF <- rep(0,5), INGRN <- rep(0,5), NUP <- rep(0,5))
      FixationVegetativeGrowth[VegetativeGrowthFilter] <- fNFixationDuringVegetativeGrowth(sAccumulatedStemDryMatter = sAccumulatedStemDryMatter, 
                                                                                           pSNCG = pSNCG, 
                                                                                           cGrowthLAI = cGrowthLAI, 
                                                                                           pSpecLeafNGreenLeaf = pSpecLeafNGreenLeaf, 
                                                                                           pNUPmax = pNUPmax, 
                                                                                           sTotalAvailableUptakeN = sTotalAvailableUptakeN,
                                                                                           cDailyStemWeightIncrease = cDailyStemWeightIncrease, 
                                                                                           sAccumulatedVegetativeDryMatter = sAccumulatedVegetativeDryMatter,
                                                                                           cFTSWRrootZone = cFTSWRrootZone,
                                                                                           pStemMinimumNconcentration = pStemMinimumNconcentration,
                                                                                           sLAI = sLAI,
                                                                                           pSpecLeafNSenescenceLeaf = pSpecLeafNSenescenceLeaf,
                                                                                           cDryMatterProduction = cDryMatterProduction)[VegetativeGrowthFilter]
      
      
      # The function returns INST, XNST, XNLF, INLF, NUP and INGRN during vegetative growth (6 numeric vectors)
      
      cDemandNAccumulation[VegetativeGrowthFilter] <- (FixationVegetativeGrowth$NUP)[VegetativeGrowthFilter]
      sNitrogenContent[VegetativeGrowthFilter] <- sNitrogenContent[VegetativeGrowthFilter] + (FixationVegetativeGrowth$BNF)[VegetativeGrowthFilter]
      sAccumulatedNStem[VegetativeGrowthFilter] <- sAccumulatedNStem[VegetativeGrowthFilter] + (FixationVegetativeGrowth$INST)[VegetativeGrowthFilter] - (FixationVegetativeGrowth$XNST)[VegetativeGrowthFilter]
      sAccumulatedLeafNitrogen[VegetativeGrowthFilter] <-  sAccumulatedLeafNitrogen[VegetativeGrowthFilter] + (FixationVegetativeGrowth$INLF)[VegetativeGrowthFilter] - (FixationVegetativeGrowth$XNLF)[VegetativeGrowthFilter]
      sAccumulatedNGrain[VegetativeGrowthFilter] <- sAccumulatedNGrain[VegetativeGrowthFilter] + (FixationVegetativeGrowth$INGRN)[VegetativeGrowthFilter]
      cTotalAccumulatedNitrogen[VegetativeGrowthFilter] <- sAccumulatedNStem[VegetativeGrowthFilter] + sAccumulatedLeafNitrogen[VegetativeGrowthFilter]
      
    }
    
    if (any(SeedGrowthFilter)){
      
      FixationSeedGrowth <- data.frame(INST <- rep(0,5), XNST <- rep(0,5), INLF <- rep(0,5), XNLF <- rep(0,5), BNF <- rep(0,5),INGRN <- rep(0,5),NUP <- rep(0,5))
      FixationSeedGrowth[SeedGrowthFilter] <- fNFixationDuringSeedGrowth(cDailySeedWeightIncrease = cDailySeedWeightIncrease, 
                                                                         pGrainMaxConcentrationN  = pGrainMaxConcentrationN, 
                                                                         pSNCG = pSNCG,
                                                                         cGrowthLAI = cGrowthLAI, 
                                                                         pSpecLeafNGreenLeaf = pSpecLeafNGreenLeaf, 
                                                                         pNUPmax =  pNUPmax, 
                                                                         sCoefBiologicalNFixation = sCoefBiologicalNFixation, 
                                                                         sAccumulatedVegetativeDryMatter = sAccumulatedVegetativeDryMatter, 
                                                                         cCoefWaterStressSaturation = cCoefWaterStressSaturation, 
                                                                         cDailyStemWeightIncrease = cDailyStemWeightIncrease, 
                                                                         cDryMatterProduction = cDryMatterProduction, 
                                                                         pGrainConversionCoefficient = pGrainConversionCoefficient,
                                                                         cFTSWRrootZone = cFTSWRrootZone, 
                                                                         sTotalAvailableUptakeN = sTotalAvailableUptakeN , 
                                                                         sAccumulatedStemDryMatter = sAccumulatedStemDryMatter, 
                                                                         pGrainMinConcentrationN = pGrainMinConcentrationN, 
                                                                         sLAI = sLAI, 
                                                                         pStemMinimumNconcentration =  pStemMinimumNconcentration,
                                                                         fxlf = FXLF)[SeedGrowthFilter]
      
      
      # The function returns INST, XNST, XNLF, INLF, NUP and INGRN during seed growth (6 numeric vectors)
      
      cDemandNAccumulation[SeedGrowthFilter] <- (FixationSeedGrowth$NUP)[SeedGrowthFilter]
      sNitrogenContent[SeedGrowthFilter] <- sNitrogenContent[SeedGrowthFilter] + (FixationSeedGrowth$BNF)[SeedGrowthFilter]
      sAccumulatedNStem[SeedGrowthFilter] <- sAccumulatedNStem[SeedGrowthFilter] + (FixationSeedGrowth$INST)[SeedGrowthFilter] - (FixationSeedGrowth$XNST)[SeedGrowthFilter]
      sAccumulatedLeafNitrogen[SeedGrowthFilter] <- sAccumulatedLeafNitrogen[SeedGrowthFilter] + (FixationSeedGrowth$INLF)[SeedGrowthFilter] - (FixationSeedGrowth$XNLF)[SeedGrowthFilter]
      sAccumulatedNGrain[SeedGrowthFilter] <- sAccumulatedNGrain[SeedGrowthFilter] + (FixationSeedGrowth$INGRN)[SeedGrowthFilter]
      cTotalAccumulatedNitrogen[SeedGrowthFilter] <- sAccumulatedNStem[SeedGrowthFilter] + sAccumulatedLeafNitrogen[SeedGrowthFilter]
      
    }
    
    
    
    
    
    
    # Extraction of cDemandNAccumulation, which is calculated in 3 different ways depending on which phase we are
    
    #cDemandNAccumulation <- numeric(length(sAccumulatedStemDryMatter))
    #cDemandNAccumulation[BeforeBNF] <- 0
    #cDemandNAccumulation[VegetativeGrowthFilter] <- (FixationVegetativeGrowth[7])[VegetativeGrowthFilter]
    #cDemandNAccumulation[SeedGrowthFilter] <- (FixationSeedGrowth[SeedGrowthFilter])[7]
    
    
    
    
    
    
    
    
    # Application of filters on the functions to calculate 
    
    
    #cEstimatesSupplyVeg <- fEstimateOfSupplyVegs(cBSGMobilizable = TRLN,
    #sBdFromSowingToTerminationLeaf =  toto1,
    #sBdFromSowingToSeedGrowth = toto, 
    #bd = sBiologicalDay ,
    #cDailySeedWeightIncrease =  cDailySeedWeightIncrease ,
    #cUptakeNGrain = ,
    #pGrainMinConcentrationN = pGrainMinConcentrationN,
    #pGrainMaxConcentrationN = pGrainMaxConcentrationN, 
    #cDemandNAccumulation = NUP)
    
    # Calculation Cumulated Variables : 
    
    #[BeforeBNF] <- 0
    #sNitrogenContent[VegetativeGrowthFilter] <- sNitrogenContent[VegetativeGrowthFilter] + FixationVegetativeGrowth[VegetativeGrowthFilter]$BNF
    # BNF (cBiologicalNFixation) is the 5th argument returned by the first fonction
    #sNitrogenContent[SeedGrowthFilter] <- sNitrogenContent[SeedGrowthFilter] + FixationSeedGrowth[VegetativeGrowthFilter]$BNF
    # BNF (cBiologicalNFixation) is the 5th argument returned by the second fonction
    
    
    
    #sAccumulatedNStem[BeforeBNF] <- 0
    #sAccumulatedNStem[VegetativeGrowthFilter] <- sAccumulatedNStem[VegetativeGrowthFilter] + FixationVegetativeGrowth[VegetativeGrowthFilter]$INST - FixationVegetativeGrowth[VegetativeGrowthFilter]$XNST
    # INST (sAccumulatedNStem) is the first argument returned by the first function
    # XNST (sDailyRateNFromStem) is the second argument returned by the first function
    #sAccumulatedNStem[SeedGrowthFilter] <- sAccumulatedNStem[SeedGrowthFilter] + FixationSeedGrowth[SeedGrowthFilter]$INST - FixationSeedGrowth[SeedGrowthFilter]$XNST
    # same process here but with the 2nd function
    
    #sAccumulatedLeafNitrogen[BeforeBNF] <- 0
    #sAccumulatedLeafNitrogen[VegetativeGrowthFilter] <-  sAccumulatedLeafNitrogen[VegetativeGrowthFilter] + FixationVegetativeGrowth[VegetativeGrowthFilter]$INLF - FixationVegetativeGrowth[VegetativeGrowthFilter]$XNLF
    # INLF (sDailyAccumulationLeavesN) is the 3th argument returned by the first function
    # XNLF (sDailyRateNfromLeave) is the 4th argument returned by the first function
    #sAccumulatedLeafNitrogen[SeedGrowthFilter] <- sAccumulatedLeafNitrogen[SeedGrowthFilter] + FixationSeedGrowth[SeedGrowthFilter]$INLF - FixationSeedGrowth[SeedGrowthFilter]$XNLF
    # same process here but with the 2nd function
    
    #sAccumulatedNGrain[BeforeBNF] <- 0
    #sAccumulatedNGrain[VegetativeGrowthFilter] <- sAccumulatedNGrain[VegetativeGrowthFilter] + FixationVegetativeGrowth[VegetativeGrowthFilter]$INGRN
    # INGRN (cDailySeedsNDemands) is the 6th argument returned by the first function
    #sAccumulatedNGrain[SeedGrowthFilter] <- sAccumulatedNGrain[VegetativeGrowthFilter] + FixationSeedGrowth[SeedGrowthFilter]$INGRN
    
    #cTotalAccumulatedNitrogen <- sAccumulatedNStem + sAccumulatedLeafNitrogen
    # NVEG (cTotalAccumulatedNitrogen) is the Total Accumulated Nitrogen (leave + stem)
    
    
    
    
    ALLDAYDATA[,c("sNitrogenContent", "sAccumulatedNStem", "sAccumulatedLeafNitrogen",
                  "sAccumulatedNGrain", "cTotalAccumulatedNitrogen", "cDemandNAccumulation")]<<-data.frame(
                    sNitrogenContent, sAccumulatedNStem, sAccumulatedLeafNitrogen,
                    sAccumulatedNGrain, cTotalAccumulatedNitrogen, cDemandNAccumulation) 
    
    
  }
}