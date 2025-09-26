## Can we make this better? What about accounting for decreasing variability with time on account of lower mean? What about adding a flat probability of producing a structural zero because of wider ecological range of bivalves?
bestModel_disp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), dispformula = ~stage, data = raw_data, family = nbinom2(link = "log"))
bestModel_zero <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), ziformula = ~1, data = raw_data, family = nbinom2(link = "log"))
bestModel_zeroDisp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), ziformula = ~1, dispformula = ~stage, data = raw_data, family = nbinom2(link = "log"))
## None of these models converged.

## Simulate residuals for both and then test
#resbestModel_disp <- simulateResiduals(bestModel_disp, re.form = NULL)
#testDispersion(resbestModel_disp, type = "DHARMa") ##
#testZeroInflation(resbestModel_disp) ##
#testOutliers(resbestModel_disp, type = "bootstrap") ##
#plot(resbestModel_disp) ##
#check_collinearity(bestModel_disp) ##

#resbestModel_zero <- simulateResiduals(bestModel_zero, re.form = NULL)
#testDispersion(resbestModel_zero, type = "DHARMa") ##
#testZeroInflation(resbestModel_zero) ##
#testOutliers(resbestModel_zero, type = "bootstrap") ##
#plot(resbestModel_zero) ##
#check_collinearity(bestModel_zero) ##

#resbestModel_zeroDisp <- simulateResiduals(bestModel_zeroDisp, re.form = NULL)
#testDispersion(resbestModel_zeroDisp, type = "DHARMa") ##
#testZeroInflation(resbestModel_zeroDisp) ##
#testOutliers(resbestModel_zeroDisp, type = "bootstrap") ##
#plot(resbestModel_zeroDisp) ##
#check_collinearity(bestModel_zeroDisp) ##

## Best model so far is vanilla. Might get better with dredge.
bestModel <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + AbsLat + (bivalve|stage), data = raw_data, family = nbinom2(link = "log"))
resBestModel <- simulateResiduals(bestModel, re.form = NULL)


#Try fitting dispersion and zero-inflation models again.
bestModel_disp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage), dispformula = ~stage, data = raw_data, family = nbinom2(link = "log"))
bestModel_zero <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage), ziformula = ~1, data = raw_data, family = nbinom2(link = "log"))
bestModel_zeroDisp <- glmmTMB(brachiopod ~ bivalve * PTME + bath + reef + lith + (bivalve|stage), ziformula = ~1, dispformula = ~stage, data = raw_data, family = nbinom2(link = "log"))
## Last model didn't converge.

## Simulate residuals for both and then test
resbestModel_disp <- simulateResiduals(bestModel_disp, re.form = NULL)
testDispersion(resbestModel_disp, type = "DHARMa") ## Ratio of 1.7474! Insignificant but that ratio is awful
testZeroInflation(resbestModel_disp) ## Ratio of 0.44322, significant.
testOutliers(resbestModel_disp, type = "bootstrap") ## Observed 0.3124 versus 0.0018, significant. Bad!
plot(resbestModel_disp) ## Normality and dispersion test failure.
check_collinearity(bestModel_disp) ## VIFs better. PTME and bivalve:PTME still exhibiting collinearity but below 5 now.

resbestModel_zero <- simulateResiduals(bestModel_zero, re.form = NULL)
testDispersion(resbestModel_zero, type = "DHARMa") ## Ratio is 0.62837, significant
testZeroInflation(resbestModel_zero) ## Ratio is 1.2399, insignificant.
testOutliers(resbestModel_zero, type = "bootstrap") ## Expected 0.0014 versus 0.0059, insignificant.
plot(resbestModel_zero) ## K-S test and dispersion test failure.
check_collinearity(bestModel_zero) ## VIFs a little better.

#resbestModel_zeroDisp <- simulateResiduals(bestModel_zeroDisp, re.form = NULL)
#testDispersion(resbestModel_zeroDisp, type = "DHARMa") ##
#testZeroInflation(resbestModel_zeroDisp) ##
#testOutliers(resbestModel_zeroDisp, type = "bootstrap") ##
#plot(resbestModel_zeroDisp) ##
#check_collinearity(bestModel_zeroDisp) ##

## Final check - dropping PTME solo term and interaction term (to address VIFs)
## Re-do best model just to make sure it has no convergence issues. No solo term.
bestModelSansPTME <- glmmTMB(brachiopod ~ bivalve + bivalve:PTME + bath + lith + reef + (bivalve|stage), data = modelData, family = nbinom2(link = "log"))
## Model convergence issue.

## No interaction term
bestModelSansPTME <- glmmTMB(brachiopod ~ bivalve + PTME + bath + lith + reef + (bivalve|stage), data = modelData, family = nbinom2(link = "log"))
## Model convergence issue.

## Test assumptions once again to be sure!
resbestModelSansPTME <- simulateResiduals(bestModelSansPTME, re.form = NULL)
testDispersion(resbestModelSansPTME, type = "DHARMa") ## Ratio is 0.26109, significant and worse.
testZeroInflation(resbestModelSansPTME) ## Ratio is 1.7058, significant and worse.
testOutliers(resbestModelSansPTME, type = "bootstrap") ## Insignificant. Observed 0.0014 versus 0.007 expected.
plot(resbestModelSansPTME) ## All tests fail.
check_collinearity(bestModelSansPTME) ## VIFs fine though!

## Best model really is the one with the high VIFs.

#### Power analysis using old method ####
## Shuffle model data
shuffled_data <- lapply(1:1000, function(x){
  out <- modelData
  out$brachiopod <- out$brachiopod[sample(1:length(out$brachiopod),length(out$brachiopod),replace = F)]
  return(out)
})

## Rerun best model for all
shuffled_data_models <- lapply(1:length(shuffled_data), function(y){
  model <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + (bivalve|stage),
                 data = shuffled_data[[y]], family = poisson(link = "sqrt"),
                 control = glmerControl(optimizer="bobyqa"))
})

## Count number of significant terms for each predictor with jumbled responses.
## Do first model for reference
coeffs1 <- get_model_data(shuffled_data_models[[1]], type = "est")
reps = 1000
tracker_coeff <- data.frame(matrix(NA, ncol = 5, nrow = reps))
tracker_pstars <- data.frame(matrix(NA, ncol = 5, nrow = reps))
colnames(tracker_coeff) <- coeffs1[,"term"]
colnames(tracker_pstars) <- coeffs1[,"term"]

for(i in 1:reps){
  ## get model data
  coeffs <- get_model_data(shuffled_data_models[[i]], type = "est")
  ## add coefficients
  tracker_coeff[i,which(colnames(tracker_coeff) == coeffs[,"term"])] <- coeffs[,"estimate"]
  ## Log pstars
  tracker_pstars[i,which(colnames(tracker_pstars) == coeffs[,"term"])] <- coeffs[,"p.stars"]
}

## Summarise - find all significant terms
length(which(str_detect(tracker_pstars[,"bivalve"], fixed("*"))))
length(which(str_detect(tracker_pstars[,"PTMEPostPTME"], fixed("*"))))
length(which((str_detect(tracker_pstars[,"bath"], fixed("*")))))
length(which(str_detect(tracker_pstars[,"lith"], fixed("*"))))
length(which(str_detect(tracker_pstars[,"reef"], fixed("*"))))

#### Using unstandardised data ####
## Retain a copy of data that is not standardised
raw_data_unstd <- raw_data

## Rearrange to match new order of standardised
raw_data_unstd <- raw_data_unstd[,c(1,3,4,5,10,2,6,7,8,9)]
colnames(raw_data_unstd) <- c("stage", "brachiopod", "long", "lat", "PTME", "bivalve", "lith", "bath", "reef", "AbsLat")

## Get model data before standardisation
modelData_unstd <- raw_data_unstd[,c(1,2,5,6,7,8,9,10)]

## Fit best model to unstandardised data
bestModel_raw <- glmer(brachiopod ~ bivalve + PTME + bath + reef + lith + (bivalve|stage), data = modelData_unstd, family = poisson(link = "sqrt"), control = glmerControl(optimizer="bobyqa"))


