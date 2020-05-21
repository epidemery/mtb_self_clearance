library("ggplot2")
library("reshape2")
library("here")

### Define the parameters ###

no_self_cure_age_infection = 10

data_TB_dis_file_suffix = "primary"

file_suffix = "primary"

display_end_time = 75 

end_time = 125

step_size = 0.1

steps_per_year = 1/step_size

lower_age = 10
upper_age = 100

cutoff = 1

### Load parameter and trajectory samples ###

params_MCMC_no_self_cure = readRDS(here::here("results",paste0("MCMC_parameters_no_self_cure_",file_suffix,".Rdata")))
params_MCMC_self_cure = readRDS(here::here("results",paste0("MCMC_parameters_self_cure_",file_suffix,".Rdata")))

sampled_trajectories_no_self_cure = readRDS(here::here("results",paste0("sampled_trajectories_no_self_cure_",file_suffix,".Rdata")))
sampled_trajectories_self_cure = readRDS(here::here("results",paste0("sampled_trajectories_self_cure_",file_suffix,".Rdata")))

### Parameter results ###

## Quantiles ###

parameter_results_quantiles_no_self_cure = apply(params_MCMC_no_self_cure, c(2), quantile, probs = c(0.025,0.5,0.975))
parameter_results_quantiles_self_cure = apply(params_MCMC_self_cure, c(2), quantile, probs = c(0.025,0.5,0.975))

## Mean ###

parameter_results_mean_no_self_cure = apply(params_MCMC_no_self_cure, c(2), mean)
parameter_results_mean_self_cure = apply(params_MCMC_self_cure, c(2), mean)

### Trajectory results ###

### All trajectories ###

### No self-cure ###

median_no_self_cure = apply(sampled_trajectories_no_self_cure, c(1,2), quantile, probs = c(0.5), na.rm = TRUE)
lower_no_self_cure = apply(sampled_trajectories_no_self_cure, c(1,2), quantile, probs = c(0.025), na.rm = TRUE)
upper_no_self_cure = apply(sampled_trajectories_no_self_cure, c(1,2), quantile, probs = c(0.975), na.rm = TRUE)
mean_no_self_cure = apply(sampled_trajectories_no_self_cure, c(1,2), mean, na.rm = TRUE)

### Self-cure ###

median_self_cure = apply(sampled_trajectories_self_cure, c(1,2), quantile, probs = c(0.5), na.rm = TRUE)
lower_self_cure = apply(sampled_trajectories_self_cure, c(1,2), quantile, probs = c(0.025), na.rm = TRUE)
upper_self_cure = apply(sampled_trajectories_self_cure, c(1,2), quantile, probs = c(0.975), na.rm = TRUE)
mean_self_cure = apply(sampled_trajectories_self_cure, c(1,2), mean, na.rm = TRUE)

### Convert to % of initial cohort except time and R_inst ###

### No self-cure ###

median_no_self_cure[, !colnames(median_no_self_cure) %in% c("time","R_inst")]=median_no_self_cure[, !colnames(median_no_self_cure) %in% c("time","R_inst")]/1000
upper_no_self_cure[, !colnames(upper_no_self_cure) %in% c("time","R_inst")]=upper_no_self_cure[, !colnames(upper_no_self_cure) %in% c("time","R_inst")]/1000
lower_no_self_cure[, !colnames(lower_no_self_cure) %in% c("time","R_inst")]=lower_no_self_cure[, !colnames(lower_no_self_cure) %in% c("time","R_inst")]/1000
mean_no_self_cure[, !colnames(mean_no_self_cure) %in% c("time","R_inst")]=mean_no_self_cure[, !colnames(mean_no_self_cure) %in% c("time","R_inst")]/1000

### Self-cure ###

median_self_cure[, !colnames(median_self_cure) %in% c("time","R_inst")]=median_self_cure[, !colnames(median_self_cure) %in% c("time","R_inst")]/1000
upper_self_cure[, !colnames(upper_self_cure) %in% c("time","R_inst")]=upper_self_cure[, !colnames(upper_self_cure) %in% c("time","R_inst")]/1000
lower_self_cure[, !colnames(lower_self_cure) %in% c("time","R_inst")]=lower_self_cure[, !colnames(lower_self_cure) %in% c("time","R_inst")]/1000
mean_self_cure[, !colnames(mean_self_cure) %in% c("time","R_inst")]=mean_self_cure[, !colnames(mean_self_cure) %in% c("time","R_inst")]/1000

### A_l ###

median_A_l_birth_no_self_cure = as.data.frame(median_no_self_cure[,c("time","A_l_birth")])
mean_A_l_birth_no_self_cure = as.data.frame(mean_no_self_cure[,c("time","A_l_birth")])
upper_A_l_birth_no_self_cure = as.data.frame(upper_no_self_cure[,c("time","A_l_birth")])
lower_A_l_birth_no_self_cure = as.data.frame(lower_no_self_cure[,c("time","A_l_birth")])
unc_A_l_birth_no_self_cure = data.frame("time" = upper_A_l_birth_no_self_cure$time, "lower_A_l_birth_no_self_cure" = lower_A_l_birth_no_self_cure$A_l_birth, "upper_A_l_birth_no_self_cure" = upper_A_l_birth_no_self_cure$A_l)

median_A_l_no_self_cure = as.data.frame(median_no_self_cure[,c("time","A_l")])
mean_A_l_no_self_cure = as.data.frame(mean_no_self_cure[,c("time","A_l")])
upper_A_l_no_self_cure = as.data.frame(upper_no_self_cure[,c("time","A_l")])
lower_A_l_no_self_cure = as.data.frame(lower_no_self_cure[,c("time","A_l")])
unc_A_l_no_self_cure = data.frame("time" = upper_A_l_no_self_cure$time, "lower_A_l_no_self_cure" = lower_A_l_no_self_cure$A_l, "upper_A_l_no_self_cure" = upper_A_l_no_self_cure$A_l)

median_A_l_birth_self_cure = as.data.frame(median_self_cure[,c("time","A_l_birth")])
mean_A_l_birth_self_cure = as.data.frame(mean_self_cure[,c("time","A_l_birth")])
upper_A_l_birth_self_cure = as.data.frame(upper_self_cure[,c("time","A_l_birth")])
lower_A_l_birth_self_cure = as.data.frame(lower_self_cure[,c("time","A_l_birth")])
unc_A_l_birth_self_cure = data.frame("time" = upper_A_l_birth_self_cure$time, "lower_A_l_birth_self_cure" = lower_A_l_birth_self_cure$A_l_birth, "upper_A_l_birth_self_cure" = upper_A_l_birth_self_cure$A_l)

median_A_l_self_cure = as.data.frame(median_self_cure[,c("time","A_l")])
mean_A_l_self_cure = as.data.frame(mean_self_cure[,c("time","A_l")])
upper_A_l_self_cure = as.data.frame(upper_self_cure[,c("time","A_l")])
lower_A_l_self_cure = as.data.frame(lower_self_cure[,c("time","A_l")])
unc_A_l_self_cure = data.frame("time" = upper_A_l_self_cure$time, "lower_A_l_self_cure" = lower_A_l_self_cure$A_l, "upper_A_l_self_cure" = upper_A_l_self_cure$A_l)

### C_l ###

median_C_l_no_self_cure = as.data.frame(median_no_self_cure[,c("time","C_l")])
mean_C_l_no_self_cure = as.data.frame(mean_no_self_cure[,c("time","C_l")])
upper_C_l_no_self_cure = as.data.frame(upper_no_self_cure[,c("time","C_l")])
lower_C_l_no_self_cure = as.data.frame(lower_no_self_cure[,c("time","C_l")])
unc_C_l_no_self_cure = data.frame("time" = upper_C_l_no_self_cure$time, "lower_C_l_no_self_cure" = lower_C_l_no_self_cure$C_l, "upper_C_l_no_self_cure" = upper_C_l_no_self_cure$C_l)

median_C_l_self_cure = as.data.frame(median_self_cure[,c("time","C_l")])
mean_C_l_self_cure = as.data.frame(mean_self_cure[,c("time","C_l")])
upper_C_l_self_cure = as.data.frame(upper_self_cure[,c("time","C_l")])
lower_C_l_self_cure = as.data.frame(lower_self_cure[,c("time","C_l")])
unc_C_l_self_cure = data.frame("time" = upper_C_l_self_cure$time, "lower_C_l_self_cure" = lower_C_l_self_cure$C_l, "upper_C_l_self_cure" = upper_C_l_self_cure$C_l)

median_C_l_inf_self_cure = as.data.frame(median_self_cure[,c("time","C_l_inf")])
mean_C_l_inf_self_cure = as.data.frame(mean_self_cure[,c("time","C_l_inf")])
upper_C_l_inf_self_cure = as.data.frame(upper_self_cure[,c("time","C_l_inf")])
lower_C_l_inf_self_cure = as.data.frame(lower_self_cure[,c("time","C_l_inf")])
unc_C_l_inf_self_cure = data.frame("time" = upper_C_l_inf_self_cure$time, "lower_C_l_inf_self_cure" = lower_C_l_inf_self_cure$C_l_inf, "upper_C_l_inf_self_cure" = upper_C_l_inf_self_cure$C_l_inf)

### L_1 ###

mean_L_1_no_self_cure = as.data.frame(mean_no_self_cure[,c("time","L_1")])
median_L_1_no_self_cure = as.data.frame(median_no_self_cure[,c("time","L_1")])
upper_L_1_no_self_cure = as.data.frame(upper_no_self_cure[,c("time","L_1")])
lower_L_1_no_self_cure = as.data.frame(lower_no_self_cure[,c("time","L_1")])
unc_L_1_no_self_cure = data.frame("time" = upper_L_1_no_self_cure$time, "lower_L_1_no_self_cure" = lower_L_1_no_self_cure$L_1, "upper_L_1_no_self_cure" = upper_L_1_no_self_cure$L_1)

median_L_1_self_cure = as.data.frame(median_self_cure[,c("time","L_1")])
mean_L_1_self_cure = as.data.frame(mean_self_cure[,c("time","L_1")])
upper_L_1_self_cure = as.data.frame(upper_self_cure[,c("time","L_1")])
lower_L_1_self_cure = as.data.frame(lower_self_cure[,c("time","L_1")])
unc_L_1_self_cure = data.frame("time" = upper_L_1_self_cure$time, "lower_L_1_self_cure" = lower_L_1_self_cure$L_1, "upper_L_1_self_cure" = upper_L_1_self_cure$L_1)

median_L_1_inf_self_cure = as.data.frame(median_self_cure[,c("time","L_1_inf")])
mean_L_1_inf_self_cure = as.data.frame(mean_self_cure[,c("time","L_1_inf")])
upper_L_1_inf_self_cure = as.data.frame(upper_self_cure[,c("time","L_1_inf")])
lower_L_1_inf_self_cure = as.data.frame(lower_self_cure[,c("time","L_1_inf")])
unc_L_1_inf_self_cure = data.frame("time" = upper_L_1_inf_self_cure$time, "lower_L_1_inf_self_cure" = lower_L_1_inf_self_cure$L_1_inf, "upper_L_1_inf_self_cure" = upper_L_1_inf_self_cure$L_1)

### R_inst ###

median_R_inst_no_self_cure = as.data.frame(median_no_self_cure[,c("time","R_inst")])
mean_R_inst_no_self_cure = as.data.frame(mean_no_self_cure[,c("time","R_inst")])
upper_R_inst_no_self_cure = as.data.frame(upper_no_self_cure[,c("time","R_inst")])
lower_R_inst_no_self_cure = as.data.frame(lower_no_self_cure[,c("time","R_inst")])
unc_R_inst_no_self_cure = data.frame("time" = upper_R_inst_no_self_cure$time, "lower_R_inst_no_self_cure" = lower_R_inst_no_self_cure$R_inst, "upper_R_inst_no_self_cure" = upper_R_inst_no_self_cure$R_inst)

median_R_inst_self_cure = as.data.frame(median_self_cure[,c("time","R_inst")])
mean_R_inst_self_cure = as.data.frame(mean_self_cure[,c("time","R_inst")])
upper_R_inst_self_cure = as.data.frame(upper_self_cure[,c("time","R_inst")])
lower_R_inst_self_cure = as.data.frame(lower_self_cure[,c("time","R_inst")])
unc_R_inst_self_cure = data.frame("time" = upper_R_inst_self_cure$time, "lower_R_inst_self_cure" = lower_R_inst_self_cure$R_inst, "upper_R_inst_self_cure" = upper_R_inst_self_cure$R_inst)

### Cumulative risk results ###

### No self-cure ###

### Before/after ###

prob_before_median_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.5))
prob_before_lower_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.025))
prob_before_upper_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.975))
prob_before_mean_no_self_cure = mean(apply(sampled_trajectories_no_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum))

prob_after_median_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2), sum, na.rm = TRUE),c(0.5))
prob_after_lower_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.025))
prob_after_upper_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.975))
prob_after_mean_no_self_cure = mean(apply(sampled_trajectories_no_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE))

probability_results_no_self_cure = data.frame(lower_no_self_cure = c(prob_before_lower_no_self_cure, prob_after_lower_no_self_cure),
                                             median_no_self_cure = c(prob_before_median_no_self_cure, prob_after_median_no_self_cure),
                                             upper_no_self_cure  = c(prob_before_upper_no_self_cure, prob_after_upper_no_self_cure)) 

row.names(probability_results_no_self_cure) = c("before_no_self_cure","after_no_self_cure")

### Lifetime ###

prob_lifetime_median_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2), sum, na.rm = TRUE),c(0.5))
prob_lifetime_lower_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.025))
prob_lifetime_upper_no_self_cure = quantile(apply(sampled_trajectories_no_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.975))
prob_lifetime_mean_no_self_cure = mean(apply(sampled_trajectories_no_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE))

probability_lifetime_results_no_self_cure = data.frame(lower_no_self_cure = c(prob_lifetime_lower_no_self_cure),
                                                       median_no_self_cure = c(prob_lifetime_median_no_self_cure),
                                                       upper_no_self_cure  = c(prob_lifetime_upper_no_self_cure)) 

row.names(probability_lifetime_results_no_self_cure) = c("lifetime")

### Self-cure ###

### Before/after ###

prob_before_median_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.5))
prob_before_lower_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.025))
prob_before_upper_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum),c(0.975))
prob_before_mean_self_cure = mean(apply(sampled_trajectories_self_cure[seq(1,cutoff*steps_per_year,1),"R_inst",],c(2),sum))

prob_after_median_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2), sum, na.rm = TRUE),c(0.5))
prob_after_lower_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.025))
prob_after_upper_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.975))
prob_after_mean_self_cure = mean(apply(sampled_trajectories_self_cure[seq(cutoff*steps_per_year+1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE))

probability_results_self_cure = data.frame(lower_self_cure = c(prob_before_lower_self_cure, prob_after_lower_self_cure),
                                              median_self_cure = c(prob_before_median_self_cure, prob_after_median_self_cure),
                                              upper_self_cure  = c(prob_before_upper_self_cure, prob_after_upper_self_cure)) 

row.names(probability_results_self_cure) = c("before_self_cure","after_self_cure")

### Lifetime ###

prob_lifetime_median_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2), sum, na.rm = TRUE),c(0.5))
prob_lifetime_lower_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.025))
prob_lifetime_upper_self_cure = quantile(apply(sampled_trajectories_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE),c(0.975))
prob_lifetime_mean_self_cure = mean(apply(sampled_trajectories_self_cure[seq(1,end_time*steps_per_year+1,1),c("R_inst"),],c(2),sum, na.rm = TRUE))

probability_lifetime_results_self_cure = data.frame(lower_self_cure = c(prob_lifetime_lower_self_cure),
                                                       median_self_cure = c(prob_lifetime_median_self_cure),
                                                       upper_self_cure  = c(prob_lifetime_upper_self_cure)) 

row.names(probability_lifetime_results_self_cure) = c("lifetime")


### Data ###

data_TB_dis = read.csv(file = here::here("data",paste0("data_TB_dis_",data_TB_dis_file_suffix,".csv")), header = TRUE, sep = ",")
data_TB_dis$time_birth = data_TB_dis$time + no_self_cure_age_infection

data_self_clear = read.table(file = here::here("data","data_self_clear.csv"), header = TRUE, sep = ",")

age_inf_aut_med = parameter_results_quantiles_self_cure["50%","age_inf_mean"]
age_inf_aut_low = parameter_results_quantiles_self_cure["2.5%","age_inf_mean"]
age_inf_aut_upp = parameter_results_quantiles_self_cure["97.5%","age_inf_mean"]
 
age_inf_tst_med = parameter_results_quantiles_self_cure["50%","age_inf_mean_t"]
age_inf_tst_low = parameter_results_quantiles_self_cure["2.5%","age_inf_mean_t"]
age_inf_tst_upp = parameter_results_quantiles_self_cure["97.5%","age_inf_mean_t"]

data_self_clear$time_med = data_self_clear$time - age_inf_aut_med
data_self_clear$time_upp = data_self_clear$time - age_inf_aut_low
data_self_clear$time_low = data_self_clear$time - age_inf_aut_upp

data_self_clear$time_med[1] = data_self_clear$time[1] - age_inf_tst_med
data_self_clear$time_upp[1] = data_self_clear$time[1] - age_inf_tst_low
data_self_clear$time_low[1] = data_self_clear$time[1] - age_inf_tst_upp

### Plots ####

### Settings ###

font_adjust = 8
title_size = 17 + font_adjust
axis_title_size = 16 + font_adjust
axis_text_size = 15 + font_adjust
title_align = 0.5
title_face = "bold"
legend_title_size = 17 + font_adjust
legend_text_size = 17 + font_adjust
legend_position = c(0.78, 0.3)
line_thickness = 0.7
height = 16
aspect_ratio = 1.4
bar_width = 1
colour_self_cure = "#CE2931"
colour_no_self_cure = "#0A74CC"
error_bar_thickness = 0.5
point_size = 3

### Main paper ###

### Self-cure - C_l_inf ###

a = ggplot() +
  geom_line(data = median_C_l_inf_self_cure, size = line_thickness, aes(x = time, y = C_l_inf), na.rm=TRUE, colour = colour_self_cure) +
  geom_ribbon(data = unc_C_l_inf_self_cure, aes(x = time, ymin = lower_C_l_inf_self_cure, ymax = upper_C_l_inf_self_cure), fill = colour_self_cure, alpha = 0.2) +
  geom_point(data = data_self_clear, aes(x = time_med, y = prop_med, shape = type), size = point_size, colour = "black") +
  geom_errorbar(data = data_self_clear, aes(x = time_med, ymin=prop_low, ymax=prop_upp), width = bar_width, size = error_bar_thickness) +
  geom_errorbarh(data = data_self_clear, aes(y = prop_med, xmin=time_low, xmax=time_upp), height = bar_width, size = error_bar_thickness) +
  scale_shape_manual(values = c("circle","cross"), labels = c("Autopsy data","TST-reversion data")) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), expand = c(0, 0)) +
  labs(title = expression(paste(bold("Cumulative self-cleared "), bolditalic("Mtb"),bold(" infection"))),
       x = "Time since infection (years)",
       y = "% of cohort") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.125),
        legend.text = element_text(size = 19),
        legend.spacing.x = unit(0.15,"cm"),
        legend.key = element_blank()) 

saveRDS(a,here::here("results", paste0(file_suffix,"_results_C_l_self_cure_main_paper.Rdata")))

### Self-cure - A_l ###

b = ggplot() +
  geom_line(data = median_A_l_self_cure, size = line_thickness, aes(x = time, y = A_l), na.rm=TRUE, colour = colour_self_cure) +
  geom_ribbon(data = unc_A_l_self_cure, aes(x = time, ymin = lower_A_l_self_cure, ymax = upper_A_l_self_cure), fill = colour_self_cure, alpha = 0.2) +
  geom_point(data = data_TB_dis, aes(x = time, y = risk_cum_med), size = point_size, colour = "black") +
  geom_errorbar(data = data_TB_dis, aes(x = time, ymin=risk_cum_low, ymax=risk_cum_upp), width = bar_width, size = error_bar_thickness) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12.5, 2.5), limits = c(0, 12.5), expand = c(0, 0)) +
  labs(title = "Cumulative TB disease",
       x = "Time since infection (years)",
       y = "% of cohort") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(b,here::here("results",paste0(file_suffix,"_results_A_l_self_cure_main_paper.Rdata")))

### Supplementary materials ###

### No self-cure progression to disease comparison ###

### No self-cure - A_l ###

c = ggplot() +
  geom_line(data = median_A_l_no_self_cure, size = line_thickness, aes(x = time, y = A_l), na.rm=TRUE, colour = colour_no_self_cure) +
  geom_ribbon(data = unc_A_l_no_self_cure, aes(x = time, ymin = lower_A_l_no_self_cure, ymax = upper_A_l_no_self_cure), fill = colour_no_self_cure, alpha = 0.2) +
  geom_point(data = data_TB_dis, aes(x = time, y = risk_cum_med), size = point_size, colour = "black") +
  geom_errorbar(data = data_TB_dis, aes(x = time, ymin=risk_cum_low, ymax=risk_cum_upp), width = bar_width, size = error_bar_thickness) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12.5, 2.5), limits = c(0, 12.5), expand = c(0, 0)) +
  #labs(title = "Cumulative TB disease",
  labs(title = "Lifelong infection",
       x = "Time since infection (years)",
       y = "% progressed to TB disease") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 10, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(c,here::here("results",paste0(file_suffix,"_results_A_l_no_self_cure_supp.Rdata")))

### Self-cure - A_l ###

d = ggplot() +
  geom_line(data = median_A_l_self_cure, size = line_thickness, aes(x = time, y = A_l), na.rm=TRUE, colour = colour_self_cure) +
  geom_ribbon(data = unc_A_l_self_cure, aes(x = time, ymin = lower_A_l_self_cure, ymax = upper_A_l_self_cure), fill = colour_self_cure, alpha = 0.2) +
  geom_point(data = data_TB_dis, aes(x = time, y = risk_cum_med), size = point_size, colour = "black") +
  geom_errorbar(data = data_TB_dis, aes(x = time, ymin=risk_cum_low, ymax=risk_cum_upp), width = bar_width, size = error_bar_thickness) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12.5, 2.5), limits = c(0, 12.5), expand = c(0, 0)) +
  labs(title = "Self-clearance of infection",
       x = "Time since infection (years)",
       y = "% progressed to TB disease") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(d,here::here("results",paste0(file_suffix,"_results_A_l_self_cure_supp.Rdata")))

### Sensitivity plots ###

### No self-cure - A_l ###

e = ggplot() +
  geom_line(data = median_A_l_no_self_cure, size = line_thickness, aes(x = time, y = A_l), na.rm=TRUE, colour = colour_no_self_cure) +
  geom_ribbon(data = unc_A_l_no_self_cure, aes(x = time, ymin = lower_A_l_no_self_cure, ymax = upper_A_l_no_self_cure), fill = colour_no_self_cure, alpha = 0.2) +
  geom_point(data = data_TB_dis, aes(x = time, y = risk_cum_med), size = point_size, colour = "black") +
  geom_errorbar(data = data_TB_dis, aes(x = time, ymin=risk_cum_low, ymax=risk_cum_upp), width = bar_width, size = error_bar_thickness) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12.5, 2.5), limits = c(0, 12.5), expand = c(0, 0)) +
  labs(title = "Cumulative TB disease",
       x = "Time since infection (years)",
       y = "% of cohort") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 10, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(e,here::here("results",paste0(file_suffix,"_results_A_l_no_self_cure.Rdata")))

### Self-cure - A_l ###

f = ggplot() +
  geom_line(data = median_A_l_self_cure, size = line_thickness, aes(x = time, y = A_l), na.rm=TRUE, colour = colour_self_cure) +
  geom_ribbon(data = unc_A_l_self_cure, aes(x = time, ymin = lower_A_l_self_cure, ymax = upper_A_l_self_cure), fill = colour_self_cure, alpha = 0.2) +
  geom_point(data = data_TB_dis, aes(x = time, y = risk_cum_med), size = point_size, colour = "black") +
  geom_errorbar(data = data_TB_dis, aes(x = time, ymin=risk_cum_low, ymax=risk_cum_upp), width = bar_width, size = error_bar_thickness) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 12.5, 2.5), limits = c(0, 12.5), expand = c(0, 0)) +
  labs(title = "Cumulative TB disease",
       x = "Time since infection (years)",
       y = "% of cohort") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(f,here::here("results",paste0(file_suffix,"_results_A_l_self_cure.Rdata")))

### Self-cure - C_l_inf ###

g = ggplot() +
  geom_line(data = median_C_l_inf_self_cure, size = line_thickness, aes(x = time, y = C_l_inf), na.rm=TRUE, colour = colour_self_cure) +
  geom_ribbon(data = unc_C_l_inf_self_cure, aes(x = time, ymin = lower_C_l_inf_self_cure, ymax = upper_C_l_inf_self_cure), fill = colour_self_cure, alpha = 0.2) +
  geom_point(data = data_self_clear, aes(x = time_med, y = prop_med, shape = type), size = point_size, colour = "black") +
  geom_errorbar(data = data_self_clear, aes(x = time_med, ymin=prop_low, ymax=prop_upp), width = bar_width, size = error_bar_thickness) +
  geom_errorbarh(data = data_self_clear, aes(y = prop_med, xmin=time_low, xmax=time_upp), height = bar_width, size = error_bar_thickness) +
  scale_shape_manual(values = c("circle","cross")) +
  scale_x_continuous(breaks = seq(0, display_end_time, 10), limits = c(0, display_end_time+1), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), expand = c(0, 0)) +
  labs(title = expression(paste(bold("Cumulative self-cleared infection"))),
       x = "Time since infection (years)",
       y = "% of cohort") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 3, margin = margin(l = 15), size = axis_title_size),
        axis.text.x.bottom = element_text(vjust = -0.4, size = axis_text_size),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none") 

saveRDS(g,here::here("results",paste0(file_suffix,"_results_C_l_self_cure.Rdata")))

### Display results ###

### Parameters ###

results_parameters = rbind(data.frame("median" = t(round(parameter_results_quantiles_no_self_cure,9))[,2],
                                      "lower" = t(round(parameter_results_quantiles_no_self_cure,9))[,1],
                                      "upper" = t(round(parameter_results_quantiles_no_self_cure,9))[,3]),
                           data.frame("median" = t(round(parameter_results_quantiles_self_cure,9))[,2],
                                      "lower" = t(round(parameter_results_quantiles_self_cure,9))[,1],
                                      "upper" = t(round(parameter_results_quantiles_self_cure,9))[,3]))

write.csv(results_parameters, here::here("tables_plots",paste0(file_suffix,"_results_parameters.csv")))

### Proportions self-clear ###

results_clearance = data.frame("median" = c(round(subset(median_C_l_inf_self_cure,time==lower_age)[[2]],1), round(subset(median_C_l_inf_self_cure,time==upper_age)[[2]],1)), 
                               "lower"  = c(round(subset(lower_C_l_inf_self_cure,time==lower_age)[[2]],1),round(subset(lower_C_l_inf_self_cure,time==upper_age)[[2]],1)),
                               "upper" = c(round(subset(upper_C_l_inf_self_cure,time==lower_age)[[2]],1),round(subset(upper_C_l_inf_self_cure,time==upper_age)[[2]],1)))          

row.names(results_clearance) = c("10 years","lifetime")

write.csv(results_clearance, here::here("tables_plots",paste0(file_suffix,"_results_clearance.csv")))

### Probabilities ###

results_risk = data.frame("median" = c(round(probability_lifetime_results_no_self_cure[[2]],1),round(probability_lifetime_results_self_cure[[2]],1)),
                         "lower" = c(round(probability_lifetime_results_no_self_cure[[1]],1),round(probability_lifetime_results_self_cure[[1]],1)),
                         "upper" = c(round(probability_lifetime_results_no_self_cure[[3]],1),round(probability_lifetime_results_self_cure[[3]],1)))

row.names(results_risk) = c("lifelong infection","self-clearance") 

write.csv(results_risk, here::here("tables_plots",paste0(file_suffix,"_results_risk.csv")))

