### Import relevant libraries ###

library("deSolve")
library("FME")
library("reshape2")
library("ggplot2")
library("rriskDistributions")
library("here")

### Define the parameters ###

age_inf_mean_top = 10

data_TB_dis_file_suffix = "primary" 

file_suffix = "primary"

end_time = 150

step_size = 0.1

round = 1

initial_population = 100000 

params_var = c(f_a = 0.081,
               f_s = 0.821,
               s_a = 0.00036)

params_fix = c(f_c = 0,
               s_c = 0,
               a_d = 0.2,
               a_l = 0.2,
               chi_f = 0, 
               phi_f = 1,
               age_mean = 60,          
               age_variance = 10^2,
               age_inf_mean = age_inf_mean_top)

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
  
  infection_events = with(as.list(c(parameters)),{ data.frame(var = "F_1", time = round(age_inf_mean,round), value = initial_population, method = "add")})
  
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
      
      return(list(c(dF_1,dF_2,dS_1,dS_2,dC,dA,dD,dM,dA_l_birth,dC_l),A_l=A_l_birth,C_l_inf=C_l,F=F_1+F_2,S=S_1+S_2,L=F_1+F_2+S_1+S_2,L_1=F_1+S_1,L_1_inf=F_1+S_1,Su=initial_population-M,R_inst=F_1+F_2+S_1+S_2))
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
      C_l = 0)
  })
  
  times = seq(from = 0, to = end_time, by = step_size) 
  
  output = ode(func = equations, y = state, parms = parameters, times = times, events = list(data = infection_events)) 
  
  output[,"A_l"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"A_l_birth"],rep(max(output[,"A_l_birth"]),cutoff_row))})
  
  output[,"C_l_inf"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"C_l"],rep(max(output[,"C_l"]),cutoff_row))})
  
  output[,"L_1_inf"] = with(as.list(c(parameters)),{c(output[seq(cutoff_row+1,final_row+1,1),"L_1"],rep(output[final_row+1,"L_1"],cutoff_row))})
  
  output[1,"L_1_inf"] = initial_population 
  
  output[,"R_inst"] = c(diff(output[,"A_l"], lag = 1), rep(NaN,1))/output[,"L_1_inf"]*100
  
  output[final_row+1,"R_inst"] = 0
  
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

#### Least squares fitting ###

cost_function = function(parameters){
  
  output = trajectory_fit(parameters)
  cost_active_disease = modCost(model = output, obs = data_TB_dis[,c("time","A_l","av")], err = "av")
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

    f_s_prior = dunif(f_s, unif_int[1], unif_int[2])

    f_a_prior = dunif(f_a, unif_int[1], unif_int[2])

    s_a_prior = dunif(s_a, unif_int[1], unif_int[2])

    log_prior =  log(f_s_prior) + log(f_a_prior) + log(s_a_prior) # log(f_c_prior) + log(s_c_prior) + log(age_inf_mean_prior) + log(a_l_prior) + log(a_d_prior)

    log_prior = -2*log_prior

    return(log_prior)

  })
}

log_like = function(parameters){
  
  trajectory = trajectory_fit(c(parameters,params_fix))
  
  model_output_A_l = subset(x=(trajectory[,c("time","A_l")]), time %in% data_TB_dis[,c("time")])
  
  model_output_A_l$A_l_mod_red = model_output_A_l$A_l/data_TB_dis$at_risk_init_scaled[1]*data_TB_dis$at_risk_init[1]
  
  comparison = data.frame("time" = model_output_A_l$time, "A_l_mod_red" = model_output_A_l$A_l_mod_red, "A_l_dat" = data_TB_dis$cases_cum)
  
  like_A_l = sum(log(dbeta(comparison$A_l_mod_red/data_TB_dis$at_risk_init[1],comparison$A_l_dat,data_TB_dis$at_risk_init[1]-comparison$A_l_dat)))
  
  likelihood = -2*(like_A_l)#+like_C_l)
  
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
               ntrydr = 1, drscale = NULL, verbose = TRUE)

MCMC_elapsed_time = proc.time() - MCMC_clock_start

round(MCMC_elapsed_time["elapsed"]/60,1)
round(MCMC_elapsed_time["elapsed"]/(60^2),1)

params_MCMC = MCMC$pars

#### Sampling ###

dim_set = trajectory_plot(c(params_var,params_fix))

sampled_trajectories = array(dim=c(dim(dim_set)[1],dim(dim_set)[2],2*samples))

colnames(sampled_trajectories) = colnames(dim_set)

sampled_parameters = data.frame(params_MCMC[sample(nrow(params_MCMC),samples),])
sampled_parameters = rbind(sampled_parameters,sampled_parameters)

sampled_parameters_self_cure = readRDS(here::here("results",paste0("sampled_parameters_self_cure_",file_suffix,".Rdata")))
sampled_parameters_switched_self_cure = readRDS(here::here("results",paste0("sampled_parameters_switched_self_cure_",file_suffix,".Rdata")))

sampled_parameters$age_inf_mean = 1
sampled_parameters$age_inf_mean[seq(1,samples,1)] = sampled_parameters_self_cure[,"age_inf_mean"]
sampled_parameters$age_inf_mean[seq(samples+1,2*samples,1)] = sampled_parameters_switched_self_cure[,"age_inf_mean"]

sampled_parameters = as.vector(sampled_parameters)

for(i in seq(1,2*samples,1)){
  #sampled_trajectories[,,i] = trajectory_plot(c(sampled_parameters[i,],params_fix))
  sampled_trajectories[,,i] = trajectory_plot(c(sampled_parameters[i,],params_fix[-9]))[seq(1,1501,1),]
  print(paste0("Sampling trajectories: ",i*100/(2*samples),"%"))
}

#### Results ###

saveRDS(MCMC, here::here("results",paste0("MCMC_no_self_cure_",file_suffix,".Rdata")))
saveRDS(MCMC$pars, here::here("results",paste0("MCMC_parameters_no_self_cure_",file_suffix,".Rdata")))
saveRDS(sampled_parameters, here::here("results",paste0("sampled_parameters_no_self_cure_",file_suffix,".Rdata")))
saveRDS(sampled_trajectories, here::here("results",paste0("sampled_trajectories_no_self_cure_",file_suffix,".Rdata")))








