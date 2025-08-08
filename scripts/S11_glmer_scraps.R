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


Try fitting dispersion and zero-inflation models again.
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
