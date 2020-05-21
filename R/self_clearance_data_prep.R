library("here")

at_risk_scaled = 100000

data_self_clearance = data.frame("time" = c(13.5,40,60,80), "at_risk" = c(111,33,85,36), "cleared" = c(23,19.7,54.8,25.7), "type" = c("TST reversion","autopsy","autopsy","autopsy"))

data_self_clearance$prop_med =100*qbeta(0.5,data_self_clearance$cleared, data_self_clearance$at_risk-data_self_clearance$cleared)
data_self_clearance$prop_low =100*qbeta(0.025,data_self_clearance$cleared, data_self_clearance$at_risk-data_self_clearance$cleared)
data_self_clearance$prop_upp =100*qbeta(0.975,data_self_clearance$cleared, data_self_clearance$at_risk-data_self_clearance$cleared)

data_self_clearance$scaled_med = data_self_clearance$prop_med/100*at_risk_scaled
data_self_clearance$scaled_low = data_self_clearance$prop_low/100*at_risk_scaled
data_self_clearance$scaled_upp = data_self_clearance$prop_upp/100*at_risk_scaled

write.csv(data_self_clearance, here::here("data", paste0("data_self_clear.csv")), row.names=FALSE)