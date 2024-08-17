netMetricDeltas <- function(all_metrics) {
  all_metrics2 <- copy(all_metrics)
  
  all_metrics2[, deltaS := S - BL_S]
  all_metrics2[, deltaS2 := (S - BL_S)/BL_S] ## % species lost/gained
  all_metrics2[, deltaL := L - BL_L]   ##
  all_metrics2[, deltaL2 := (L - BL_L)/BL_L] ## % links lost/gained
  all_metrics2[, deltaLD := LD - BL_LD]
  all_metrics2[, deltaC := C - BL_C]
  all_metrics2[, absDeltaC := abs(C - BL_C)]
  all_metrics2[, deltanormGen := normGen - BL_normGen]
  all_metrics2[, deltanormVul := normVul - BL_normVul]
  all_metrics2[, deltaSDnormGen := SDnormGen - BL_SDnormGen]
  all_metrics2[, deltaSDnormVul := SDnormVul - BL_SDnormVul]
  all_metrics2[, deltapropB := propB - BL_propB]
  all_metrics2[, deltapropI := propI - BL_propI]
  all_metrics2[, deltapropT := propT - BL_propT]
  all_metrics2[, deltapropOmn := propOmn - BL_propOmn]
  all_metrics2[, deltapropB2 := (propB - BL_propB)/BL_propB]
  all_metrics2[, deltapropI2 := (propI - BL_propI)/BL_propI]
  all_metrics2[, deltapropT2 := (propT - BL_propT)/BL_propT]
  all_metrics2[, deltapropOmn2 := (propOmn - BL_propOmn)/BL_propOmn] ## % change
  all_metrics2[, deltamean.TL := mean.TL - BL_mean.TL]
  all_metrics2[, deltamax.TL := max.TL - BL_max.TL]
  all_metrics2[, deltasd.TL := sd.TL - BL_sd.TL]
  
  return(all_metrics2)
}