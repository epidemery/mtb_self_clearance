### Import relevant libraries ###

library("deSolve", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("FME", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("numDeriv", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

### Define the parameters ###

file_suffix = "primary"

start_year = 1910

end_year = 2019

step_size = 0.1 

times = seq(from = start_year, to = end_year, by = step_size) 

max_age = 105

cohort_width = 5

ari_file_suffix = "primary"

params_sampled = readRDS(here::here("results", paste0("sampled_parameters_no_self_cure_", file_suffix, ".Rdata")))
params_sampled = params_sampled[seq(1,dim(params_sampled)[1]/2,1),]

parameters = data.frame(params_sampled, "f_c"= 0, "s_c" = 0, "chi_f"= 0, "a_l" = 0.2, "a_d" = 0.2, "phi_f" = 1, "mu" = 0)

cohorts = c("1914",
            "1919",
            "1924",
            "1929",
            "1934",
            "1939",
            "1944",
            "1949",
            "1954",
            "1959",
            "1964",
            "1969",
            "1974",
            "1979",
            "1984",
            "1989",
            "1994",
            "1999",
            "2004",
            "2009",
            "2014")

################################################################################################################################################################

### JAPAN ###

################################################################################################################################################################

birth_table = read.table(file = here::here("data","births_JPN.csv"), header = TRUE, sep = ",")
ari_table_all = readRDS(here::here("results", paste0("ARI_GPR_JPN_regression_", ari_file_suffix, ".Rdata")))
ari_table_all$replicate = 1
  

nu_prep = function(time){
  with(as.list(c(data_births)),{
    if(time<birth_year){
      birth_rate = 0
    }else if(time>=birth_year+cohort_width){
      birth_rate = 0
    }else{
      birth_rate = births/cohort_width
    }
    return(birth_rate)
  })
}

nu = function(time){
  nu = apply(FUN = nu_prep, X = as.matrix(time), MARGIN = 1)
  return(nu)
}

ari_table_all$ari = exp(ari_table_all$lari)

for(i in seq(1,dim(ari_table_all)[1],1)){
  if(ari_table_all[i,"ari"]>1){
    ari_table_all[i,"ari"] = 0.99
  }else if(ari_table_all[i,"ari"]<0){
    ari_table_all[i,"ari"] = 0.01
  }
}

ari_table_all$ari_rate = -log(1-ari_table_all$ari)

ari_table = ari_table_all

ari_table_function_spline_rate = splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")

lambda_rate = function(time){
  if(time<1934){
    lambda_rate = ari_table[1,"ari_rate"]
  }else{
    lambda_rate = apply(FUN = ari_table_function_spline_rate, X = as.matrix(time), MARGIN = 1)  
  }
  return(lambda_rate)
}

### Calculate trajectories ###

trajectory_plot = function(parameters){

  equations = function(time, state, parameters) {

    with(as.list(c(state,parameters)),{

        dP =  nu(time) - mu*P
       dSU =  nu(time)                                                                                        - mu*SU   - lambda_rate(time)*SU
      dF_1 = -f_c*F_1                     - f_s*F_1 - f_a*F_1                                                 - mu*F_1  + lambda_rate(time)*SU
      dF_2 = -f_c*F_2                     - f_s*F_2 - f_a*F_2                      +     chi_f*a_l*A          - mu*F_2
      dS_1 = -s_c*S_1                     + f_s*F_1 - s_a*S_1                                                 - mu*S_1
      dS_2 = -s_c*S_2                     + f_s*F_2 - s_a*S_2                      + (1-chi_f)*a_l*A          - mu*S_2
        dC =  f_c*(F_1+F_2)+s_c*(S_1+S_2)                                                                     - mu*C
        dA =                                          f_a*(F_1+F_2)+s_a*(S_1+S_2)            - a_l*A  - a_d*A - mu*A
        dD =                                                                                            a_d*A
        dM =                                                                                                    mu*(SU+F_1+F_2+S_1+S_2+C+A)
      dA_l =                                          f_a*F_1+s_a*S_1
      dC_l =                                          f_c*F_1+s_c*S_1

      return(list(c(dP,dSU,dF_1,dF_2,dS_1,dS_2,dC,dA,dD,dM,dA_l,dC_l),F=F_1+F_2,S=S_1+S_2,L=F_1+F_2+S_1+S_2,R=F_1+F_2+S_1+S_2,LP=F_1+F_2+S_1+S_2+A+D+M))
    })
  }

  state = with(as.list(c(parameters)),{

    c(  P = 0,
       SU = 0,
      F_1 = 0,   #phi_f*initial_population,
      F_2 = 0,
      S_1 = 0,  #(1-phi_f)*initial_population,
      S_2 = 0,
        C = 0,
        A = 0,
        D = 0,
        M = 0,
      A_l = 0,
      C_l = 0)
  })

  output = ode(func = equations, y = state, parms = parameters, times = times)

  return(output)
}

trajectory_birth_year = function(parameters,birth_year,replicate){
  data_births <<- data.frame(birth_year = birth_year, births = birth_table[,paste("X", birth_year, sep = "")])
  ari_table <<- ari_table_all[ari_table_all[,"replicate"]==replicate,]
  ari_table_function_spline_rate <<- splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")
  trajectory_birth_year = trajectory_plot(parameters)
  return(trajectory_birth_year)
}

trajectories_cohorts = array(dim = c(dim(trajectory_birth_year(parameters[1,],1914,1))[1],dim(trajectory_birth_year(parameters[1,],1914,1))[2],length(cohorts),dim(parameters)[1]))

dimnames(trajectories_cohorts) = list(c(1:dim(trajectory_birth_year(parameters[1,],1914,1))[1]),c(colnames(trajectory_birth_year(parameters[1,],1914,1))),cohorts,c(rownames(parameters)))

for(j in c(1:dim(parameters)[1])){
  for(i in seq(1,length(cohorts),1)){
    trajectories_cohorts[,,cohorts[i],j] = trajectory_birth_year(parameters[j,],as.numeric(cohorts[i]),1)
    print(paste("JPN", j, cohorts[i]))
  }
}

saveRDS(trajectories_cohorts, file = here::here("results",paste0("trajectories_cohorts_no_self_cure_JPN_",file_suffix,".Rdata")))

# ################################################################################################################################################################

### CHINA ###

################################################################################################################################################################

birth_table = read.table(file =  here::here("data","births_CHN.csv"), header = TRUE, sep = ",")
ari_table_all = readRDS(here::here("results",paste0("ARI_GPR_CHN_regression_", ari_file_suffix, ".Rdata")))
ari_table_all$replicate = 1

nu_prep = function(time){
  with(as.list(c(data_births)),{
    if(time<birth_year){
      birth_rate = 0
    }else if(time>=birth_year+cohort_width){
      birth_rate = 0
    }else{
      birth_rate = births/cohort_width
    }
    return(birth_rate)
  })
}

nu = function(time){
  nu = apply(FUN = nu_prep, X = as.matrix(time), MARGIN = 1)
  return(nu)
}

ari_table_all$ari = exp(ari_table_all$lari)

for(i in seq(1,dim(ari_table_all)[1],1)){
  if(ari_table_all[i,"ari"]>1){
    ari_table_all[i,"ari"] = 0.99
  }else if(ari_table_all[i,"ari"]<0){
    ari_table_all[i,"ari"] = 0.01
  }
}

ari_table_all$ari_rate = -log(1-ari_table_all$ari)

ari_table = ari_table_all

ari_table_function_spline_rate = splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")

lambda_rate = function(time){
  if(time<1934){
    lambda_rate = ari_table[1,"ari_rate"]
  }else{
    lambda_rate = apply(FUN = ari_table_function_spline_rate, X = as.matrix(time), MARGIN = 1)  
  }
  return(lambda_rate)
}

### Calculate trajectories ###

trajectory_plot = function(parameters){

  equations = function(time, state, parameters) {

    with(as.list(c(state,parameters)),{

      dP =  nu(time) - mu*P
      dSU =  nu(time)                                                                                        - mu*SU   - lambda_rate(time)*SU
      dF_1 = -f_c*F_1                     - f_s*F_1 - f_a*F_1                                                 - mu*F_1  + lambda_rate(time)*SU
      dF_2 = -f_c*F_2                     - f_s*F_2 - f_a*F_2                      +     chi_f*a_l*A          - mu*F_2
      dS_1 = -s_c*S_1                     + f_s*F_1 - s_a*S_1                                                 - mu*S_1
      dS_2 = -s_c*S_2                     + f_s*F_2 - s_a*S_2                      + (1-chi_f)*a_l*A          - mu*S_2
      dC =  f_c*(F_1+F_2)+s_c*(S_1+S_2)                                                                     - mu*C
      dA =                                          f_a*(F_1+F_2)+s_a*(S_1+S_2)            - a_l*A  - a_d*A - mu*A
      dD =                                                                                            a_d*A
      dM =                                                                                                    mu*(SU+F_1+F_2+S_1+S_2+C+A)
      dA_l =                                          f_a*F_1+s_a*S_1
      dC_l =                                          f_c*F_1+s_c*S_1

      return(list(c(dP,dSU,dF_1,dF_2,dS_1,dS_2,dC,dA,dD,dM,dA_l,dC_l),F=F_1+F_2,S=S_1+S_2,L=F_1+F_2+S_1+S_2,R=F_1+F_2+S_1+S_2,LP=F_1+F_2+S_1+S_2+A+D+M))
    })
  }

  state = with(as.list(c(parameters)),{

    c(  P = 0,
        SU = 0,
        F_1 = 0,   #phi_f*initial_population,
        F_2 = 0,
        S_1 = 0,  #(1-phi_f)*initial_population,
        S_2 = 0,
        C = 0,
        A = 0,
        D = 0,
        M = 0,
        A_l = 0,
        C_l = 0)
  })

  output = ode(func = equations, y = state, parms = parameters, times = times)

  return(output)
}

trajectory_birth_year = function(parameters,birth_year,replicate){
  data_births <<- data.frame(birth_year = birth_year, births = birth_table[,paste("X", birth_year, sep = "")])
  ari_table <<- ari_table_all[ari_table_all[,"replicate"]==replicate,]
  ari_table_function_spline_rate <<- splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")
  trajectory_birth_year = trajectory_plot(parameters)
  return(trajectory_birth_year)
}

trajectories_cohorts = array(dim = c(dim(trajectory_birth_year(parameters[1,],1914,1))[1],dim(trajectory_birth_year(parameters[1,],1914,1))[2],length(cohorts),dim(parameters)[1]))

dimnames(trajectories_cohorts) = list(c(1:dim(trajectory_birth_year(parameters[1,],1914,1))[1]),c(colnames(trajectory_birth_year(parameters[1,],1914,1))),cohorts,c(rownames(parameters)))

for(j in c(1:dim(parameters)[1])){
  for(i in seq(1,length(cohorts),1)){
    trajectories_cohorts[,,cohorts[i],j] = trajectory_birth_year(parameters[j,],as.numeric(cohorts[i]),1)
    print(paste("CHN", j, cohorts[i]))
  }
}

saveRDS(trajectories_cohorts, file = here::here("results",paste0("trajectories_cohorts_no_self_cure_CHN_",file_suffix,".Rdata")))

################################################################################################################################################################

### INDIA ###

################################################################################################################################################################

birth_table = read.table(file =  here::here("data","births_IND.csv"), header = TRUE, sep = ",")
ari_table_all = readRDS(here::here("results", paste0("ARI_GPR_IND_regression_", ari_file_suffix, ".Rdata")))
ari_table_all$replicate = 1

nu_prep = function(time){
  with(as.list(c(data_births)),{
    if(time<birth_year){
      birth_rate = 0
    }else if(time>=birth_year+cohort_width){
      birth_rate = 0
    }else{
      birth_rate = births/cohort_width
    }
    return(birth_rate)
  })
}

nu = function(time){
  nu = apply(FUN = nu_prep, X = as.matrix(time), MARGIN = 1)
  return(nu)
}

ari_table_all$ari = exp(ari_table_all$lari)

for(i in seq(1,dim(ari_table_all)[1],1)){
  if(ari_table_all[i,"ari"]>1){
    ari_table_all[i,"ari"] = 0.99
  }else if(ari_table_all[i,"ari"]<0){
    ari_table_all[i,"ari"] = 0.01
  }
}

ari_table_all$ari_rate = -log(1-ari_table_all$ari)

ari_table = ari_table_all

ari_table_function_spline_rate = splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")

lambda_rate = function(time){
  if(time<1934){
    lambda_rate = ari_table[1,"ari_rate"]
  }else{
    lambda_rate = apply(FUN = ari_table_function_spline_rate, X = as.matrix(time), MARGIN = 1)  
  }
  return(lambda_rate)
}

### Calculate trajectories ###

trajectory_plot = function(parameters){

  equations = function(time, state, parameters) {

    with(as.list(c(state,parameters)),{

      dP =  nu(time) - mu*P
      dSU =  nu(time)                                                                                        - mu*SU   - lambda_rate(time)*SU
      dF_1 = -f_c*F_1                     - f_s*F_1 - f_a*F_1                                                 - mu*F_1  + lambda_rate(time)*SU
      dF_2 = -f_c*F_2                     - f_s*F_2 - f_a*F_2                      +     chi_f*a_l*A          - mu*F_2
      dS_1 = -s_c*S_1                     + f_s*F_1 - s_a*S_1                                                 - mu*S_1
      dS_2 = -s_c*S_2                     + f_s*F_2 - s_a*S_2                      + (1-chi_f)*a_l*A          - mu*S_2
      dC =  f_c*(F_1+F_2)+s_c*(S_1+S_2)                                                                     - mu*C
      dA =                                          f_a*(F_1+F_2)+s_a*(S_1+S_2)            - a_l*A  - a_d*A - mu*A
      dD =                                                                                            a_d*A
      dM =                                                                                                    mu*(SU+F_1+F_2+S_1+S_2+C+A)
      dA_l =                                          f_a*F_1+s_a*S_1
      dC_l =                                          f_c*F_1+s_c*S_1

      return(list(c(dP,dSU,dF_1,dF_2,dS_1,dS_2,dC,dA,dD,dM,dA_l,dC_l),F=F_1+F_2,S=S_1+S_2,L=F_1+F_2+S_1+S_2,R=F_1+F_2+S_1+S_2,LP=F_1+F_2+S_1+S_2+A+D+M))
    })
  }

  state = with(as.list(c(parameters)),{

    c(  P = 0,
        SU = 0,
        F_1 = 0,   #phi_f*initial_population,
        F_2 = 0,
        S_1 = 0,  #(1-phi_f)*initial_population,
        S_2 = 0,
        C = 0,
        A = 0,
        D = 0,
        M = 0,
        A_l = 0,
        C_l = 0)
  })

  output = ode(func = equations, y = state, parms = parameters, times = times)

  return(output)
}

trajectory_birth_year = function(parameters,birth_year,replicate){
  data_births <<- data.frame(birth_year = birth_year, births = birth_table[,paste("X", birth_year, sep = "")])
  ari_table <<- ari_table_all[ari_table_all[,"replicate"]==replicate,]
  ari_table_function_spline_rate <<- splinefun(ari_table$year, ari_table$ari_rate, method = "fmm")
  trajectory_birth_year = trajectory_plot(parameters)
  return(trajectory_birth_year)
}

trajectories_cohorts = array(dim = c(dim(trajectory_birth_year(parameters[1,],1914,1))[1],dim(trajectory_birth_year(parameters[1,],1914,1))[2],length(cohorts),dim(parameters)[1]))

dimnames(trajectories_cohorts) = list(c(1:dim(trajectory_birth_year(parameters[1,],1914,1))[1]),c(colnames(trajectory_birth_year(parameters[1,],1914,1))),cohorts,c(rownames(parameters)))

for(j in c(1:dim(parameters)[1])){
  for(i in seq(1,length(cohorts),1)){
    trajectories_cohorts[,,cohorts[i],j] = trajectory_birth_year(parameters[j,],as.numeric(cohorts[i]),1)
    print(paste("IND", j, cohorts[i]))
  }
}

saveRDS(trajectories_cohorts, file = here::here("results",paste0("trajectories_cohorts_no_self_cure_IND_",file_suffix,".Rdata")))












