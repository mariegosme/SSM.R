### TEST

##### PlantNitrogen 
  
  # ---- function which returns the total mobilizable N available in the plant before seed growth ----

#' Title : function which returns the total mobilizable N available in the plant before seed growth

#'
#' @param lai numeric vector of LAI values.
#' @param slns numeric vector which contains values of the specific leaf nitrogen in senesced leaves(g N m-2).
#' @param slng numeric vector which contains values of the specific leaf nitrogen in green leaves (g N m-2).
#' @param nst numeric vector which contains values of the accumulated Nitrogen in stem (g N m-1).
#' @param wst numeric vector which contains values of the cumulative stem dry matter (g m−2).
#' @param sncs numeric vector which contains values of the minimum stem N concentration(g N g−1).
#'
#' @return total mobilizable N available in the plant at the beginning of seed fill (g N m-2).
#' @export
#'
#' @examples
fComputeNAvailableatBeginningseedFill<-function(lai,slns, slng, nst, wst, sncs){
  
  return (lai * (slng - slns)+(nst-wst*sncs))
  # Equation 17.4 on the PDF, p228
  # TRLNBSG (name variable in VBA code)
}




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
#' @param pContentStemN (SNCG) : numeric vector which contains N content per unit stem weight (g N g-1).
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
fNFixationDuringVegetativeGrowth <- function(sAccumulatedStemDryMatter, pContentStemN, cGrowthLAI, pSpecLeafNGreenLeaf, pNUPmax, sTotalAvailableUptakeN,cDailyStemWeightIncrease, sAccumulatedVegetativeDryMatter,cFTSWRrootZone,pStemMinimumNconcentration, sLAI, pSpecLeafNSenescenceLeaf){
  
  WSXF <- c(1,1,1)
  # Waiting to have more information about WSXF (ICICICICICICI)
  sNitrogenAccumulation <- rep(1,length(sAccumulatedStemDryMatter))
  # Value initialized at 1 outside the function, don't know why this because used in a multiplication
  sAccumulatedLeafNitrogen <- sLAI*pSpecLeafNGreenLeaf
  # Initialization of NLF : NLF <- LAI * SLNG
  sAccumulatedNStem <- sAccumulatedStemDryMatter * pContentStemN
  # Initialization of NST : NST <- WST * SNCG
  sAccumulatedNGrain <- numeric(length(sAccumulatedStemDryMatter))
  # Initialization of NGRN : Vector of 0 of the length of sAccumulatedStemDryMatter
  cCoefBiologicalNFixation <- c(1,1,1)
  # Arbitrary initialization of NFC cCoefBiologicalNFixation : Not done in the VBA code (ICICICICICICIC)
  
  cDailySeedsNDemands <- numeric(length(sAccumulatedStemDryMatter))
  cDemandResultDeficiencies <- pmax((sAccumulatedStemDryMatter * pContentStemN) - sAccumulatedNStem, 0)
  cDemandNAccumulation <- pmin((cDailyStemWeightIncrease * pContentStemN) + (cGrowthLAI * pSpecLeafNGreenLeaf) + cDemandResultDeficiencies, pNUPmax)
  cCoefBiologicalNFixation <- cCoefBiologicalNFixation*(3/4) + (cDemandNAccumulation/sAccumulatedVegetativeDryMatter)*(1/4) ### ICICICIICICIC pas de sens physique à cette équation
  ### 3/4 and 1/4 seem to be like ponderation coefficients
  cDemandNAccumulation <- pmax(cDemandNAccumulation*sNitrogenAccumulation, 0) 
  
  
  ConditionFTSWRZ <- cFTSWRrootZone > 1
  cDemandNAccumulation[ConditionFTSWRZ] <- cDemandNAccumulation[ConditionFTSWRZ]*WSXF[ConditionFTSWRZ]
  
  
  ConditionDDMP <- cDryMatterProduction == 0
  cDemandNAccumulation[ConditionDDMP] <- 0
  
  cDemandNAccumulation <- pmin(cDemandNAccumulation,sTotalAvailableUptakeN)
  
  
  ## cBiologicalNFixation : BNF
  
  cCoefBiologicalNFixation <- numeric(length(sAccumulatedStemDryMatter))
  cBiologicalNFixation <- numeric(length(sAccumulatedStemDryMatter))
  LimitationCropUptake <- cDemandNAccumulation > sTotalAvailableUptakeN
  # Condition 
  cBiologicalNFixation[LimitationCropUptake] <- pmax(cDemandNAccumulation[LimitationCropUptake]-sTotalAvailableUptakeN[sTotalAvailableUptakeN],0)
  
  # ------------------------- Beginning of translation of the Diagramm of the PDF which describes the repartition of nitrogen exchanges during seed growth ------------------- #
  
  
  
  
  
  ## Initialisation for the calcul of the 4 principal variables : 
  
  sDailyAccumulationStemN <- sDailyRateNFromStem <- sDailyAccumulationLeavesN <- sDailyRateNfromLeave <- numeric(length(sAccumulatedStemDryMatter))
  
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
  sDailyRateNFromStem[NLeafGreaterThanNNeeded] <- min(sDailyAccumulationLeavesN  - cDemandNAccumulation, sAccumulatedNStem - sAccumulatedStemDryMatter * pStemMinimumNconcentration)[NLeafGreaterThanNNeeded]
  
  # NStemGreaterThanNNeeded = 4th condition : INST > NUP and the first condition is satisfied
  # NStemSmallerThanNNeeded = 5th condition : INST < NUP and the first condition is satisfied
  
  NStemGreaterThanNNeeded <- StemDryMatterCondition & sDailyAccumulationStemN >cDemandNAccumulation
  NStemSmallerThanNNeeded <- StemDryMatterCondition & sDailyAccumulationStemN <=cDemandNAccumulation
  sDailyAccumulationLeavesN [NStemSmallerThanNNeeded ]<-min(cGrowthLAI*pSpecLeafNGreenLeaf, cDemandNAccumulation-sDailyAccumulationStemN )[NStemSmallerThanNNeeded ]
  sDailyRateNfromLeave[NStemGreaterThanNNeeded ]<-(sDailyAccumulationStemN -cDemandNAccumulation)[NStemGreaterThanNNeeded ]
  sDailyAccumulationStemN [NStemSmallerThanNNeeded ]<-(cDemandNAccumulation-sDailyAccumulationLeavesN )[NStemSmallerThanNNeeded ]
  
  
   
  
  return (list(val1 = sDailyAccumulationStemN ,val2 = sDailyRateNFromStem,val3 = sDailyAccumulationLeavesN,val4 = sDailyRateNfromLeave, val5 = cBiologicalNFixation, val6 = cDailySeedsNDemands, val7 = cDemandNAccumulation))
  # Return 6 vectors which contains the values of : INST (sDailyAccumulationStemN), XNST (sDailyRateNFromStem), INLF (sDailyAccumulationLeavesN), XNLF (sDailyRateNFromLeave), BNF (cBiologicalNFixation) and INGRN (cDailySeedsNDemands)
  # Values used to updated cumulated variables NST, NLF, NGRN and CUMBNF in the procedure
  
  #----------------Filtre à appliquer dans le Excel--------------- #
  
  # NFixationDuringVegetativeGrowth.filter is.after('EMR', 3.5) & is.before('R5')       
}


#---------------------- Function which occurs during seed growth, which means between phase R5 and R7 ---------------------#

#' Title function which returns the accumulated nitrogen in the stem, leaves and grain each day

#' @param pStemMinimumNconcentration (SNCS) : numeric vector which contains values of the minimum stem N concentration (g N g−1).
#' @param pGrainConversionCoefficient (GCC) : numeric vector which contains values of ratio of energy content of vegetative tissues to that of grain (g g−1 )
#' @param pContentStemN (SNCG) : numeric vector which contains N content per unit stem weight (g N g-1).
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

fNFixationDuringSeedGrowth <- function(cDailySeedWeightIncrease, pGrainMaxConcentrationN,pContentStemN, cGrowthLAI, pSpecLeafNGreenLeaf, pNUPmax, cCoefBiologicalNFixation,sAccumulatedVegetativeDryMatter,cCoefWaterStressSaturation,cDailyStemWeightIncrease,cDryMatterProduction,pGrainConversionCoefficient,cFTSWrootZone,sTotalAvailableUptakeN,sAccumulatedStemDryMatter,pGrainMinConcentrationN,sLAI,pStemMinimumNconcentration){
  
  sCumulativeNFixation <- numeric(length(cDailySeedWeightIncrease))
  # Initialization of the cumulative 
  sNitrogenAccumulation <- rep(1,length(cDailySeedWeightIncrease))
  # WSFN = 1 in the VBA code, but no explanations and only used in water-stress factors
  sAccumulatedLeafNitrogen <- sLAI*pSpecLeafNGreenLeaf
  # Initialization of NLF : NLF <- LAI * SLNG
  sAccumulatedNStem <- sAccumulatedStemDryMatter * pContentStemN
  # Initialization of NST : NST <- WST * SNCG
  sAccumulatedNGrain <- numeric(length(sAccumulatedStemDryMatter))
  # Initialization of NGRN : Vector of 0 of the length of sAccumulatedStemDryMatter
  
  
  
  cDailySeedsNDemands <- cDailySeedWeightIncrease*pGrainMaxConcentrationN
  # cDailySeedWeightIncrease : SGR, daily seed growth 
  # First estimate using maximum GNC, maybe we could have tried to use minimum GNC sDailyAccumulationStemN ead, the pdf has a single constant value for GNC
  cDemandNAccumulation <- pmin(pmax(cDailySeedsNDemands + (cDailyStemWeightIncrease * pContentStemN) + (cGrowthLAI * pSpecLeafNGreenLeaf),0),pNUPmax)
  # Daily demand for N accumulation can't be negative and is limited to a maximum.
  # cDailyStemWeightIncrease (GST) : cDailyStemWeightIncrease
  cUptakeNGrain <- cDailySeedsNDemands / (cDemandNAccumulation + 0.00000001)
  # No really information about the variable cUptakeNGrain (cDemandNAccumulation_fr_grn) in the book, not defined
  # No idea of the mean of the value 0.00000001
  
  #fBeforeNFixationOrNonLegum <- function(cRateBiologicalNFix){
  #cRateBiologicalNFix <- numeric(length(cRateBiologicalNFix))
  # before BNF activation OR for non-legum crops
  # DNF :cRateBiologicalNFix, Actual rate of biological nitrogen fixation
  # Fonction which occurs beforce N Fixation
  # }
  
  # ElseIf CBD >= bdBNF in the VBA version, but we already are in that case
  
  sPotentielRateNitrogenFixation <- pmin(cCoefBiologicalNFixation * sAccumulatedVegetativeDryMatter, cDemandNAccumulation)
  # No value initialized for NFC (cCoefBiologicalNFixation) in the VBA code
  # PDNF : Potential rate of biological nitrogen fixation, which is limited to the daily demand for N accumulation
  # for legum
  cRateBiologicalNFix <- sPotentielRateNitrogenFixation * sNitrogenAccumulation 
  #for legum
  
  DryMatterCondition <- (cDryMatterProduction <= cDailySeedWeightIncrease*pGrainConversionCoefficient | cDryMatterProduction == 0)
  cRateBiologicalNFix[DryMatterCondition] <- 0
  # For Legum
  
  
  ConditionFTSWRZ <- cFTSWrootZone > 1
  cDemandNAccumulation[ConditionFTSWRZ]<- cDemandNAccumulation[ConditionFTSWRZ]*cCoefWaterStressSaturation[ConditionFTSWRZ]
  
  
  SeedGrowthCondition <- cDryMatterProduction < cDailySeedWeightIncrease/pGrainConversionCoefficient
  cDemandNAccumulation[DryMatterCondition] <- 0
  ## cDemandNAccumulation is set equal to 0 when daily dry matter production (DDMP) by the crop does not exceed SGR (Daily Seed Growth)
  
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  
  cDemandNAccumulation <- pmin(cDemandNAccumulation,sTotalAvailableUptakeN + cRateBiologicalNFix)
  cBiologicalNFixation <- pmax(cDemandNAccumulation - sTotalAvailableUptakeN, 0)
  sCumulativeNFixation <- sCumulativeNFixation + cBiologicalNFixation
  # Variable updated at each time step 
  
  
  
  #----------------- Computation of the Nitrogen dispersal in leaves and stems during the vegetative Growth ------------- #
  
  
  ## Initialisation at 0 for the calcul of the 4 principal variables : 
  
  sDailyAccumulationStemN <- sDailyRateNFromStem <- sDailyAccumulationLeavesN <- sDailyRateNfromLeave <- numeric(length(sAccumulatedStemDryMatter))
  
  # INST = XNST = INLF = XNLF = 0
  
  NUP2 <- cDemandNAccumulation - cDailySeedWeightIncrease*(pGrainMinConcentrationN+pGrainMaxConcentrationN)/2
  # The demand in Nitrogen seems to be adjusted in that case (NUP2 is the adjusted value)
  
  # ICICICICI : the code source uses a GNC value which is not used in the book or in the excel, i chose to use the mean between GNCmin and GNCmax to replace this value.
  
  GrainCondition <-  cDemandNAccumulation > cDailySeedsNDemands
  
  # First condition : NUP > INGRN
  
  # If this first condition is not satisfied : 
  
  sDailyAccumulationLeavesN[!GrainCondition] <- 0
  sDailyAccumulationStemN[!GrainCondition] <- 0
  sDailyRateNfromLeave[!GrainCondition] <- (cDailySeedsNDemands- cDemandNAccumulation)*FXLF
  sDailyRateNFromStem[!GrainCondition] <- (cDailySeedsNDemands -  cDemandNAccumulation)*(1 - FXLF)
  
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
  
  return (list(val1 = sDailyAccumulationStemN ,val2 = sDailyRateNFromStem,val3 = sDailyAccumulationLeavesN,val4 = sDailyRateNfromLeave, val5 = cBiologicalNFixation, val6 = cDailySeedsNDemands, val7 = cDemandNAccumulation))
  # Return 6 vectors which contains the values of : INST (sDailyAccumulationStemN), XNST (sDailyRateNFromStem), INLF (sDailyAccumulationLeavesN), XNLF (sDailyRateNFromLeave), BNF (cBiologicalNFixation) and INGRN (cDailySeedsNDemands)
  # Values used to updated cumulated variables NST, NLF, NGRN and CUMBNF in the procedure
}


rUpdatePlantNitrogen <-function(){
  
  ## Listing of all parameters and variables needed in the functions
  
  cDailySeedWeightIncrease <- ALLDAYDATA$cDailySeedWeightIncrease # to get SGR
  pStemMinimumNconcentration <-ALLDAYDATA$pStemMinimumNconcentration # to get SNCS
  pSpecLeafNGreenLeaf <- ALLDAYDATA$pSpecLeafNGreenLeaf # to get SNLG
  pSpecLeafNSenescenceLeaf <- ALLDAYDATA$pSpecLeafNSenescenceLeaf # to get SNLS
  sLAI <- ALLDAYDATA$sLAI # to get LAI
  sNstem <- ALLDAYDATA$sNstem # to get NST
  sAccumulatedStemDryMatter <- ALLDAYDATA$sAccumulatedStemDryMatter # to get WST
  pGrainConversionCoefficient <- ALLDAYDATA$pGrainConversionCoefficient # to get GCC
  pContentStemN <- ALLDAYDATA$pContentStemN # to get SNCG
  pGrainMaxConcentrationN <- ALLDAYDATA$pGrainMaxConcentrationN # to get GNCmax
  pGrainMinConcentrationN  <- ALLDAYDATA$pGrainMinConcentrationN # to get GNCmin
  pNUPmax <- ALLDAYDATA$pNUPmax # to get MXNUP
  cGrowthLAI <- ALLDAYDATA$cGrowthLAI # to get GLAI
  cCoefBiologicalNFixation <- ALLDAYDATA$cCoefBiologicalNFixation # to get NFC
  cCoefWaterStressSaturation <- ALLDAYDATA$cCoefWaterStressSaturation # to get WSXF
  cDailyStemWeightIncrease <- ALLDAYDATA$cDailyStemWeightIncrease # to get GST
  cDryMatterProduction <- ALLDAYDATA$cDryMatterProduction # to get DDMP
  cFTSWrootZone <- ALLDAYDATA$cFTSWrootZone # to get FTSWRZ
  sTotalAvailableUptakeN <- ALLDAYDATA$sTotalAvailableUptakeN # to get SNAVL
  sAccumulatedStemDryMatter <- ALLDAYDATA$sAccumulatedStemDryMatter # to get WST
  sAccumulatedVegetativeDryMatter <- ALLDAYDATA$sAccumulatedVegetativeDryMatter # to get WVEG
  sBiologicalDay  <- ALLDAYDATA$sBiologicalDay # to get biological days
  
  
  ## Listing of variables needed in the procedure to calcul cumulative variables
  
  sNitrogenContent <- ALLDAYDATA$sNitrogenContent # to get CUMBNF
  sAccumulatedNStem <- ALLDAYDATA$sAccumulatedNStem # to get NST
  sAccumulatedLeafNitrogen <- ALLDAYDATA$sAccumulatedLeafNitrogen # to get NLF
  sAccumulatedsAccumulatedNGrain <- ALLDAYDATA$sAccumulatedsAccumulatedNGrain # to get NGRN
  cTotalAccumulatedNitrogen <- ALLDAYDATA$cTotalAccumulatedNitrogen # to get NVEG
  
  ## Creation of the filters 
  
  VegetativeGrowthFilter <- applyfilters("VegetativeGrowthPhase")
  SeedGrowthFilter <- applyfilters("SeedGrowthPhase")
  BeforeBNF <- applyfilters("BeforeNFixation")
  
  ## Calculation of intermediate variables for the 2 main functions
  
  TRLN <- fComputeNAvailableatBeginningseedFill(lai = sLAI,
                                                slns = pSpecLeafNSenescenceLeaf, 
                                                slng =  pSpecLeafNGreenLeaf, 
                                                nst = sNstem , 
                                                wst = sAccumulatedStemDryMatter, 
                                                sncs = pStemMinimumNconcentration)
  # TRLNB is the Total mobilizable N available in the plant  and is used in the 2nd function
  
  FXLF <- fComputeDailyNTransferFromleaves(lai = sLAI,
                                           slng = pSpecLeafNGreenLeaf, 
                                           sncs = pStemMinimumNconcentration, 
                                           TRLN = TRLN)
  # FXLF is proportion of the daily N transfer from the leaves 

  

  
  ## Application of the filters on the 2 main functions 
  
  resultat1[VegetativeGrowthFilter] <- fNFixationDuringVegetativeGrowth(cDailySeedWeightIncrease = cDailySeedWeightIncrease,
                                                                       pGrainMaxConcentrationN = pGrainMaxConcentrationN,
                                                                       pContentStemN = pContentStemN,
                                                                       cGrowthLAI = cGrowthLAI,
                                                                       pSpecLeafNGreenLeaf = pSpecLeafNGreenLeaf,
                                                                       pNUPmax = pNUPmax,
                                                                       cCoefBiologicalNFixation = cCoefBiologicalNFixation,
                                                                       sAccumulatedVegetativeDryMatter = sAccumulatedVegetativeDryMatter,
                                                                       cCoefWaterStressSaturation = cCoefWaterStressSaturation,
                                                                       cDailyStemWeightIncrease = cDailyStemWeightIncrease,
                                                                       cDryMatterProduction = cDryMatterProduction,
                                                                       pGrainConversionCoefficient = pGrainConversionCoefficient,
                                                                       cFTSWrootZone = cFTSWrootZone,
                                                                       sTotalAvailableUptakeN = sTotalAvailableUptakeN,
                                                                       sAccumulatedStemDryMatter = sAccumulatedStemDryMatter,
                                                                       pGrainMinConcentrationN =  pGrainMinConcentrationN ,
                                                                       sLAI = sLAI,
                                                                       pStemMinimumNconcentration = SNCS)[VegetativeGrowthFilter]
  
  resultat2[SeedGrowthFilter] <- fNFixationDuringSeedGrowth(cDailySeedWeightIncrease = cDailySeedWeightIncrease, 
                                                           pGrainMaxConcentrationN  = pGrainMaxConcentrationN, 
                                                           pContentStemN = pContentStemN,
                                                           cGrowthLAI = cGrowthLAI, 
                                                           pSpecLeafNGreenLeaf = pSpecLeafNGreenLeaf, 
                                                           pNUPmax =  pNUPmax, 
                                                           cCoefBiologicalNFixation = cCoefBiologicalNFixation, 
                                                           sAccumulatedVegetativeDryMatter = sAccumulatedVegetativeDryMatter, 
                                                           cCoefWaterStressSaturation = cCoefWaterStressSaturation, 
                                                           cDailyStemWeightIncrease = cDailyStemWeightIncrease, 
                                                           cDryMatterProduction = cDryMatterProduction, 
                                                           pGrainConversionCoefficient = pGrainConversionCoefficient,
                                                           cFTSWrootZone = cFTSWrootZone, 
                                                           sTotalAvailableUptakeN = sTotalAvailableUptakeN , 
                                                           sAccumulatedStemDryMatter = sAccumulatedStemDryMatter, 
                                                           pGrainMinConcentrationN = pGrainMinConcentrationN, 
                                                           sLAI = sLAI, 
                                                           pStemMinimumNconcentration =  pStemMinimumNconcentration)[SeedGrowthFilter]
  
  
 
  cDemandNAccumulation <- numeric(length(sAccumulatedStemDryMatter))
  # Extraction of cDemandNAccumulation, which is calculated in 2 different ways depending on which phase we are
  cDemandNAccumulation[VegetativeGrowthFilter] <- (resultat1$val7)[VegetativeGrowthFilter]
  cDemandNAccumulation[SeedGrowthFilter] <- (resultat2$val7)[SeedGrowthFilter]
  
  
  
  
  
  
  # Application of filters on the functions to calculate 
  
  
  cEstimatesSupplyVeg <- fEstimateOfSupplyVegs(cBSGMobilizable = TRLN,
                                              sBdFromSowingToTerminationLeaf =  ## ???,
                                              sBdFromSowingToSeedGrowth = ## ???, 
                                              bd = sBiologicalDay ,
                                              cDailySeedWeightIncrease =  cDailySeedWeightIncrease ,
                                              cUptakeNGrain = ,
                                              pGrainMinConcentrationN = pGrainMinConcentrationN,
                                              pGrainMaxConcentrationN = pGrainMaxConcentrationN, 
                                              cDemandNAccumulation = NUP)
  
  # Calculation Cumulated Variables : 
  
  
  sNitrogenContent[VegetativeGrowthFilter] <-  sNitrogenContent[VegetativeGrowthFilter] + resultat1[VegetativeGrowthFilter]$val5
  # BNF (cCoefBiologicalNFixation) is the 5th argument returned by the first fonction
  sNitrogenContent[SeedGrowthFilter] <- sNitrogenContent[SeedGrowthFilter] + resultat2[VegetativeGrowthFilter]$val5
  # BNF (cCoefBiologicalNFixation) is the 5th argument returned by the second fonction
  
  
  sAccumulatedNStem[VegetativeGrowthFilter] <- sAccumulatedNStem[VegetativeGrowthFilter] + resultat1[VegetativeGrowthFilter]$val1 - resultat1[VegetativeGrowthFilter]$val2
  # INST (sAccumulatedNStem) is the first argument returned by the first function
  # XNST (sDailyRateNFromStem) is the second argument returned by the first function
  sAccumulatedNStem[SeedGrowthFilter] <- sAccumulatedNStem[SeedrowthFilter] + resultat2[SeedGrowthFilter]$val1 - resultat2[SeedGrowthFilter]$val2
  # same process here but with the 2nd function
  
  sAccumulatedLeafNitrogen[VegetativeGrowthFilter] <-  sAccumulatedLeafNitrogen[VegetativeGrowthFilter] + resultat1[VegetativeGrowthFilter]$val3 - resultat1[VegetativeGrowthFilter]$val4
  # INLF (sDailyAccumulationLeavesN) is the 3th argument returned by the first function
  # XNLF (sDailyRateNfromLeave) is the 4th argument returned by the first function
  sAccumulatedLeafNitrogen[SeedGrowthFilter] <- sAccumulatedLeafNitrogen[SeedGrowthFilter] + resultat2[SeedGrowthFilter]$val3 - resultat2[SeedGrowthFilter]$val4
  # same process here but with the 2nd function
  
  sAccumulatedsAccumulatedNGrain[VegetativeGrowthFilter] <- sAccumulatedsAccumulatedNGrain[VegetativeGrowthFilter] + resultat1[VegetativeGrowthFilter]$val6
  # INGRN (cDailySeedsNDemands) is the 6th argument returned by the first function
  sAccumulatedsAccumulatedNGrain[SeedGrowthFilter] <- sAccumulatedsAccumulatedNGrain[VegetativeGrowthFilter] + resultat2[SeedGrowthFilter]$val6
  
  cTotalAccumulatedNitrogen <- sAccumulatedNStem + sAccumulatedLeafNitrogen
  # NVEG (cTotalAccumulatedNitrogen) is the Total Accumulated Nitrogen (leave + stem)
  
  
  
  
  ALLDAYDATA[,c("sNitrogenContent", "sAccumulatedNStem", "sAccumulatedLeafNitrogen",
                "sAccumulatedsAccumulatedNGrain", "cTotalAccumulatedNitrogen","cDemandNAccumulation", "cEstimateSupplyVeg")]<<-data.frame(
                  sNitrogenContent, sAccumulatedNStem, sAccumulatedLeafNitrogen,
                  sAccumulatedsAccumulatedNGrain, cTotalAccumulatedNitrogen, cDemandNAccumulation, cEstimatesSupplyVeg
                )
  
  
}


fNitrogenBeforeSowing <- function(SNCG){
  cDemandNAccumulation <- numeric(length(SNCG))
  sDail
  
}
