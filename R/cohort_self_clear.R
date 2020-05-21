### Import relevant libraries ###

library("deSolve")
library("FME")
library("reshape2")
library("ggplot2")
library("rriskDistributions")
library("here")

### Define the parameters ###

data_TB_dis_file_suffix = "primary" 

file_suffix = "primary"

end_time = 150

step_size = 0.1

round = 1

initial_population = 100000 

params_var = c(f_a = 0.081,
               f_s = 0.821,
               s_a = 0.00036,
               s_c = 0.1,
               f_c = 0.1,
               age_inf_mean = 10,
               age_inf_mean_t = 3)  

params_fix = c(a_d = 0.2,
               a_l = 0.2,
               chi_f = 0, 
               phi_f = 1,             
               age_mean = 60,           
               age_variance = 10^2) 

niter = 5500; outputlength = 5000; burninlength = 500; updatecov = 100

samples = 125

### Model ###

fitted = c(params_var,params_fix)

steps_per_year = 1/step_size

final_row = end_time*(1/step_size) 

mu = function(time,parameters){
  
  with(as.list(c(parameters)),{
    
    alpha = (age_mean^2)/age_variance
    beta = age_variance/age_mean
    mu = dgamma(time, shape = alpha, scale = beta)/(1-((1-10^-16)*pgamma(time, shape = alpha, scale = beta)))
    return(mu)
    
  })
}

trajectory_plot = function(parameters){ 

  cutoff_row = as.integer(with(as.list(c(parameters)),{round(age_inf_mean,round)*(1/step_size)}))
  
  cutoff_row_t = as.integer(with(as.list(c(parameters)),{round(age_inf_mean_t,round)*(1/step_size)}))
  
  infection_events = with(as.list(c(parameters)),{ data.frame(var = c("F_1_t","F_1"), time = c(round(age_inf_mean_t,round),round(age_inf_mean,round)), value = initial_population, method = "add")})

  equations = function(time, state, parameters) {
    
     with(as.list(c(state,parameters)),{
       
      dF_1 = -f_c*F_1                     - f_s*F_1 - f_a*F_1                                                 - mu(time,parameters)*F_1
      dF_2 = -f_c*F_2                     - f_s*F_2 - f_a*F_2                      +     chi_f*a_l*A          - mu(time,parameters)*F_2
      dS_1 = -s_c*S_1                     + f_s*F_1 - s_a*S_1                                                 - mu(time,parameters)*S_1
      dS_2 = -s_c*S_2                     + f_s*F_2 - s_a*S_2                      + (1-chi_f)*a_l*A          - mu(time,parameters)*S_2
      dC =  f_c*(F_1+F_2)+s_c*(S_1+S_2)                                                                       - mu(time,parameters)*C
      dA =                                          f_a*(F_1+F_2)+s_a*(S_1+S_2)            - a_l*A  - a_d*A   - mu(time,parameters)*A 
      dD =                                                                                            a_d*A
      dM =                                                                                                      mu(time,parameters)*(F_1+F_2+S_1+S_2+C+A)
      dA_l_birth =                                      f_a*F_1+s_a*S_1
      dC_l =                                            f_c*F_1+s_c*S_1
      
      dF_1_t = -f_c*F_1_t                     - f_s*F_1_t - f_a*F_1_t                                                 - mu(time,parameters)*F_1_t
      dF_2_t = -f_c*F_2_t                     - f_s*F_2_t - f_a*F_2_t                      +     chi_f*a_l*A_t          - mu(time,parameters)*F_2_t
      dS_1_t = -s_c*S_1_t                     + f_s*F_1_t - s_a*S_1_t                                                 - mu(time,parameters)*S_1_t
      dS_2_t = -s_c*S_2_t                     + f_s*F_2_t - s_a*S_2_t                      + (1-chi_f)*a_l*A_t          - mu(time,parameters)*S_2_t
      dC_t =  f_c*(F_1_t+F_2_t)+s_c*(S_1_t+S_2_t)                                                                       - mu(time,parameters)*C_t
      dA_t =                                          f_a*(F_1_t+F_2_t)+s_a*(S_1_t+S_2_t)            - a_l*A_t  - a_d*A_t   - mu(time,parameters)*A_t 
      dD_t =                                                                                            a_d*A_t
      dM_t =                                                                                                      mu(time,parameters)*(F_1_t+F_2_t+S_1_t+S_2_t+C_t+A_t)
      dA_l_birth_t =                                      f_a*F_1_t+s_a*S_1_t
      dC_l_t =                                            f_c*F_1_t+s_c*S_1_t
      
      return(list(c(dF_1,dF_2,dS_1,dS_2,dC,dA,dD,dM,dA_l_birth,dC_l,
                    dF_1_t,dF_2_t,dS_1_t,dS_2_t,dC_t,dA_t,dD_t,dM_t,dA_l_birth_t,dC_l_t),
                    A_l=A_l_birth,C_l_inf=C_l,F=F_1+F_2,S=S_1+S_2,L=F_1+F_2+S_1+S_2,L_1=F_1+S_1,L_1_inf=F_1+S_1,Su=initial_population-M,R_inst=F_1+F_2+S_1+S_2,
                    A_l_t=A_l_birth_t,C_l_inf_t=C_l_t,F_t=F_1_t+F_2_t,S_t=S_1_t+S_2_t,L_t=F_1_t+F_2_t+S_1_t+S_2_t,L_1_t=F_1_t+S_1_t,L_1_inf_t=F_1_t+S_1_t,Su_t=initial_population-M_t,R_inst_t=F_1_t+F_2_t+S_1_t+S_2_t))
    })
  }
  
  state = with(as.list(c(parameters)),{
    
    c(F_1 = 0, 
      F_2 = 0,
      S_1 = 0,
      S_2 = 0,
      C = 0,
      A = 0,
      D = 0,
      M = 0,
      A_l_birth = 0,
      C_l = 0,
      F_1_t = 0, 
      F_2_t = 0,
      S_1_t = 0,
      S_2_t = 0,
      C_t = 0,
      A_t = 0,
      D_t = 0,
      M_t = 0,
      A_l_birth_t = 0,
      C_l_t = 0)
  })
  
  times = seq(from = 0, to = end_time, by = step_size) 
  
  output = ode(func = equations, y = state, parms = parameters, times = times, events = list(data = infection_events)) 

  output[,"A_l"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"A_l_birth"],rep(max(output[,"A_l_birth"]),length(output[,"A_l"])-length(output[seq(cutoff_row+1,final_row+1,1),"A_l_birth"])))})
  
  output[,"A_l_t"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row_t+1,final_row+1,1),"A_l_birth_t"],rep(max(output[,"A_l_birth_t"]),length(output[,"A_l_t"])-length(output[seq(cutoff_row_t+1,final_row+1,1),"A_l_birth_t"])))})
  
  output[,"C_l_inf"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"C_l"],rep(max(output[,"C_l"]),length(output[,"C_l_inf"])-length(output[seq(cutoff_row+1,final_row+1,1),"C_l"])))})
  
  output[,"C_l_inf_t"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row_t+1,final_row+1,1),"C_l_t"],rep(max(output[,"C_l_t"]),length(output[,"C_l_inf_t"])-length(output[seq(cutoff_row_t+1,final_row+1,1),"C_l_t"])))})
  
  output[,"L_1_inf"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"L_1"],rep(output[final_row+1,"L_1"],length(output[,"L_1_inf"])-length(output[seq(cutoff_row+1,final_row+1,1),"L_1"])))})

  output[,"L_1_inf_t"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row_t+1,final_row+1,1),"L_1_t"],rep(output[final_row+1,"L_1_t"],length(output[,"L_1_inf_t"])-length(output[seq(cutoff_row_t+1,final_row+1,1),"L_1_t"])))})
  
  output[1,"L_1_inf"] = initial_population 
  output[1,"L_1_inf_t"] = initial_population 
  
  output[,"R_inst"] = c(diff(output[,"A_l"], lag = 1), rep(NaN,1))/output[,"L_1_inf"]*100
  output[,"R_inst_t"] = c(diff(output[,"A_l_t"], lag = 1), rep(NaN,1))/output[,"L_1_inf_t"]*100
  
  output[final_row+1,"R_inst"] = 0
  output[final_row+1,"R_inst_t"] = 0
  
  return(output)
}

trajectory_fit = function(parameters){ 
  output = trajectory_plot(parameters)
  return(as.data.frame(output))
}

### Data ###

data_TB_dis = read.csv(file = here::here("data",paste0("data_TB_dis_",data_TB_dis_file_suffix,".csv")), header = TRUE, sep = ",")
data_TB_dis$A_l = data_TB_dis$cases_scaled_cum
data_TB_dis$av = mean(data_TB_dis$cases_scaled_cum)

data_self_clear_full = read.table(file = here::here("data","data_self_clear.csv"), header = TRUE, sep = ",")

data_self_clear = subset(data_self_clear_full, type == "autopsy")
data_self_clear$C_l = data_self_clear$scaled_med
data_self_clear$av = mean(data_self_clear$scaled_med)

data_self_clear_t = subset(data_self_clear_full, type == "TST reversion")
data_self_clear_t$C_l_t = data_self_clear_t$scaled_med
data_self_clear_t$av = mean(data_self_clear_t$scaled_med)

#### Least squares fitting ###

cost_function = function(parameters){
  
  output = trajectory_fit(parameters)
  cost_active_disease = modCost(model = output, obs = data_TB_dis[,c("time","A_l","av")], err = "av")
  cost_self_cure_t = modCost(model = output, obs = data_self_clear_t[,c("time","C_l_t","av")], cost = cost_active_disease, err = "av")
  cost_self_cure = modCost(model = output, obs = data_self_clear[,c("time","C_l","av")], cost = cost_self_cure_t, err = "av")

}

log_cost_function = function(log_parameters){
  cost_function(c(exp(log_parameters), params_fix))
}

best_fit_parameters = modFit(f = log_cost_function, p = log(params_var), method = "Marq")

best_fit_parameters$par = exp(coef(best_fit_parameters))

fitted = c(best_fit_parameters$par) 

### Bayesian fitting ###

unif_int = c(0,3)

quants = c(0.025, 0.5, 0.975)

log_prior = function(parameters){

  with((as.list(parameters)),{

    f_c_prior = dunif(f_c, unif_int[1], unif_int[2])

    f_s_prior = dunif(f_s, unif_int[1], unif_int[2])

    f_a_prior = dunif(f_a, unif_int[1], unif_int[2])

    s_c_prior = dunif(s_c, unif_int[1], unif_int[2])

    s_a_prior = dunif(s_a, unif_int[1], unif_int[2])
    
    age_inf_mean_prior = dnorm(1/age_inf_mean,0.100,0.025)
    
    age_inf_mean_prior_t = dunif(age_inf_mean_t, 0, 7)
  
    log_prior = log(f_c_prior) + log(f_s_prior) + log(f_a_prior) + log(s_c_prior) + log(s_a_prior) + log(age_inf_mean_prior) + log(age_inf_mean_prior_t) #+ log(a_l_prior) + log(a_d_prior)

    log_prior = -2*log_prior

    return(log_prior)

  })
}

log_like = function(parameters){
  
  print(parameters)
  
  trajectory = trajectory_fit(c(parameters,params_fix))

  model_output_A_l = subset(x=(trajectory[,c("time","A_l")]), time %in% data_TB_dis[,c("time")])

  model_output_A_l$A_l_mod_red = model_output_A_l$A_l/data_TB_dis$at_risk_init_scaled[1]*data_TB_dis$at_risk_init[1]

  comparison = data.frame("time" = model_output_A_l$time, "A_l_mod_red" = model_output_A_l$A_l_mod_red, "A_l_dat" = data_TB_dis$cases_cum)

  like_A_l = sum(log(dbeta(comparison$A_l_mod_red/data_TB_dis$at_risk_init[1],comparison$A_l_dat,data_TB_dis$at_risk_init[1]-comparison$A_l_dat)))

  model_output_C_l = subset(x=(trajectory[,c("time","C_l")]), time %in% data_self_clear[,c("time")])

  model_output_C_l$C_l_mod_red = model_output_C_l$C_l/100000

  like_C_l = sum(log(dbeta(model_output_C_l$C_l_mod_red,data_self_clear$cleared,data_self_clear$at_risk - data_self_clear$cleared)))

  model_output_C_l_t = subset(x=(trajectory[,c("time","C_l_t")]), time %in% data_self_clear_t[,c("time")])
  
  model_output_C_l_t$C_l_mod_red_t = model_output_C_l_t$C_l_t/100000
  
  like_C_l_t = sum(log(dbeta(model_output_C_l_t$C_l_mod_red_t,data_self_clear_t$cleared,data_self_clear_t$at_risk - data_self_clear_t$cleared)))

  likelihood = -2*(like_A_l+like_C_l+like_C_l_t)
  
  print(paste0("Running MCMC: ",round(z*100/niter,2),"%"))
  
  z <<- z + 1
  
  return(likelihood)

}

MCMC_clock_start = proc.time()

z = 1 

MCMC = modMCMC(log_like, c(fitted),
               jump = 0.1*fitted, lower = 0, upper = +Inf,
               prior = log_prior, var0 = NULL, wvar0 = NULL, n0 = NULL,
               niter = niter, outputlength = outputlength, burninlength = burninlength, updatecov = updatecov, 
               covscale = 2.4^2/length(fitted),
               ntrydr = 1, drscale = NULL, verbose = 10)

MCMC_elapsed_time = proc.time() - MCMC_clock_start

round(MCMC_elapsed_time["elapsed"]/60,1)
round(MCMC_elapsed_time["elapsed"]/(60^2),1)

params_MCMC = MCMC$pars

#### Sampling ###

dim_set = trajectory_plot(c(params_var,params_fix))

sampled_trajectories = array(dim=c(dim(dim_set)[1],dim(dim_set)[2],2*samples))

colnames(sampled_trajectories) = colnames(dim_set)

sampled_parameters = params_MCMC[sample(nrow(params_MCMC),samples),]

for(i in seq(1,samples,1)){
  sampled_trajectories[,,i] = trajectory_plot(c(sampled_parameters[i,],params_fix))[seq(1,1501,1),]
  print(paste0("Sampling trajectories: ",i*100/(2*samples),"%"))
}

sampled_parameters_switched = sampled_parameters
sampled_parameters_switched[,"age_inf_mean"] = sampled_parameters[,"age_inf_mean_t"]
sampled_parameters_switched[,"age_inf_mean_t"] = sampled_parameters[,"age_inf_mean"]

for(i in seq(samples+1,2*samples,1)){
  sampled_trajectories[,,i] = trajectory_plot(c(sampled_parameters_switched[i-samples,],params_fix))[seq(1,1501,1),]
  print(paste0("Sampling trajectories: ",i*100/(2*samples),"%"))
}

#### Results ###

saveRDS(MCMC, here::here("results",paste0("MCMC_self_cure_",file_suffix,".Rdata")))
saveRDS(MCMC$pars, here::here("results",paste0("MCMC_parameters_self_cure_",file_suffix,".Rdata")))
saveRDS(sampled_parameters, here::here("results",paste0("sampled_parameters_self_cure_",file_suffix,".Rdata")))
saveRDS(sampled_parameters_switched, here::here("results",paste0("sampled_parameters_switched_self_cure_",file_suffix,".Rdata")))
saveRDS(sampled_trajectories, here::here("results",paste0("sampled_trajectories_self_cure_",file_suffix,".Rdata")))








