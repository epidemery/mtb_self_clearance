library("here")

file_suffix = "primary" 

at_risk_init = 2672
cases_total = 243

distal_pro_risk = 20/100000    

distal_pro_start = 11
final_year = 50
early_case_dist = c(0,58.337,23.762,8.991,3.96,0.99,0.99,1.98,0.99,0,0)
at_risk_init_scaled = 100000
time_data_points = c(seq(1,10,1),seq(20,final_year,10))

### First 10 years ### 

time = seq(0,final_year,1)

data_TB_dis = data.frame("time"=time,"per"=c(early_case_dist,rep(0,length(time)-length(early_case_dist))))

data_TB_dis$cases_year = data_TB_dis$per/100*cases_total

data_TB_dis$cases_cum = cumsum(data_TB_dis$cases_year) 

data_TB_dis$at_risk_year = at_risk_init-data_TB_dis$cases_cum 

data_TB_dis$prob_year = data_TB_dis$cases_year/(data_TB_dis$cases_year+data_TB_dis$at_risk)

data_TB_dis$cases_scaled_year = 0

data_TB_dis$at_risk_scaled_year = 0

data_TB_dis$at_risk_scaled_year[1] = at_risk_init_scaled

for(i in seq(2,final_year+1,1)){
  data_TB_dis$cases_scaled_year[i]=data_TB_dis$prob_year[i]*data_TB_dis$at_risk_scaled_year[i-1]
  data_TB_dis$at_risk_scaled_year[i]=data_TB_dis$at_risk_scaled_year[i-1]-data_TB_dis$cases_scaled_year[i]
}

data_TB_dis$cases_scaled_cum = cumsum(data_TB_dis$cases_scaled_year)

### After 10 years ### 

data_TB_dis$prob_year[seq(distal_pro_start+1,final_year+1,1)] = distal_pro_risk 

for(i in seq(distal_pro_start+1,final_year+1,1)){
  data_TB_dis$cases_year[i]=data_TB_dis$prob_year[i]*data_TB_dis$at_risk_year[i-1]
  data_TB_dis$at_risk_year[i]=data_TB_dis$at_risk_year[i-1]-data_TB_dis$cases_year[i]
  data_TB_dis$cases_scaled_year[i]=data_TB_dis$prob_year[i]*data_TB_dis$at_risk_scaled_year[i-1]
  data_TB_dis$at_risk_scaled_year[i]=data_TB_dis$at_risk_scaled_year[i-1]-data_TB_dis$cases_scaled_year[i]
}

data_TB_dis$cases_cum = cumsum(data_TB_dis$cases_year)
data_TB_dis$cases_scaled_cum = cumsum(data_TB_dis$cases_scaled_year)

### Data points ### 

data_TB_dis = subset(data_TB_dis,time %in% time_data_points)

data_TB_dis$at_risk_init = at_risk_init
data_TB_dis$at_risk_init_scaled = at_risk_init_scaled

data_TB_dis$risk_cum_med = 100*qbeta(0.5,data_TB_dis$cases_cum, at_risk_init-data_TB_dis$cases_cum)
data_TB_dis$risk_cum_low = 100*qbeta(0.025,data_TB_dis$cases_cum, at_risk_init-data_TB_dis$cases_cum)
data_TB_dis$risk_cum_upp = 100*qbeta(0.975,data_TB_dis$cases_cum, at_risk_init-data_TB_dis$cases_cum)

write.csv(data_TB_dis,here::here("data",paste0("data_TB_dis_",file_suffix,".csv")),row.names=FALSE)




