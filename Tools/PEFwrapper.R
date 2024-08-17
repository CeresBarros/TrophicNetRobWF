PEFwrapper <- function(obj, DT, term, level = 0, fixed.at = list()) {
  DT2 <- copy(DT)
  DT2$nuPred <- getPEF.own(obj, parameter = "nu", term = term, fixed.at = fixed.at, data = DT,
                           type = "response", how = "mean", plot = FALSE, level = level)(DT2[[term]])
  DT2$tauPred <- getPEF.own(obj, parameter = "tau", term = term, fixed.at = fixed.at, data = DT,
                            type = "response", how = "mean", plot = FALSE, level = level)(DT2[[term]])
  DT2[ , `:=`(p0 = nuPred / (1 + nuPred + tauPred),
              p1 = tauPred / (1 + nuPred + tauPred))]
  DT2$muPred <- getPEF.own(obj, parameter = "mu", term = term, fixed.at = fixed.at, data = DT,
                           type = "response", how = "mean", plot = FALSE, level = level)(DT2[[term]])
  DT2[, expMean := .calcMeanBEINF(muPred, nuPred, tauPred)]
  DT2$fittedValsScale <- getPEF.own(obj, parameter = "sigma", term = term, fixed.at = fixed.at, data = DT,
                                    type = "response", how = "mean", plot = FALSE, level = level)(DT2[[term]])
  return(DT2)
}