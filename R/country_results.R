library("ggplot2")
library("here")

### Define the parameters ###

file_suffix = "primary"

### JPN NO SELF CURE###

trajectories_cohorts_no_self_cure_JPN = readRDS(here::here("results", paste0("trajectories_cohorts_no_self_cure_JPN_",file_suffix,".Rdata")))

JPN_cohort_no_self_cure = as.data.frame(t(apply(trajectories_cohorts_no_self_cure_JPN[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(JPN_cohort_no_self_cure) = c("lower","median","upper")
JPN_cohort_no_self_cure$cohort = as.numeric(rownames(JPN_cohort_no_self_cure))
JPN_cohort_no_self_cure$scenario = "no self-cure"

JPN_total_no_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_no_self_cure_JPN[1091,c("LP"),,],c(2),sum))
colnames(JPN_total_no_self_cure_replicates) = c("total_no_self_cure")

remove(trajectories_cohorts_no_self_cure_JPN)

### JPN SELF CURE###

trajectories_cohorts_self_cure_JPN = readRDS(here::here("results",paste0("trajectories_cohorts_self_cure_JPN_",file_suffix,".Rdata")))

JPN_cohort_self_cure = as.data.frame(t(apply(trajectories_cohorts_self_cure_JPN[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(JPN_cohort_self_cure) = c("lower","median","upper")
JPN_cohort_self_cure$cohort = as.numeric(rownames(JPN_cohort_self_cure))
JPN_cohort_self_cure$scenario = "self-cure"

JPN_total_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_self_cure_JPN[1091,c("LP"),,],c(2),sum))
colnames(JPN_total_self_cure_replicates) = c("total_self_cure")

remove(trajectories_cohorts_self_cure_JPN)

JPN_total_relative = as.data.frame(t(apply(JPN_total_self_cure_replicates/JPN_total_no_self_cure_replicates,2,quantile,c(0.025,0.5,0.975))))
colnames(JPN_total_relative) = c("lower","median","upper")
rownames(JPN_total_relative) = "fraction"
JPN_total_relative$country = "Japan"

### CHN NO SELF CURE###

trajectories_cohorts_no_self_cure_CHN = readRDS(here::here("results",paste0("trajectories_cohorts_no_self_cure_CHN_",file_suffix,".Rdata")))

CHN_cohort_no_self_cure = as.data.frame(t(apply(trajectories_cohorts_no_self_cure_CHN[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(CHN_cohort_no_self_cure) = c("lower","median","upper")
CHN_cohort_no_self_cure$cohort = as.numeric(rownames(CHN_cohort_no_self_cure))
CHN_cohort_no_self_cure$scenario = "no self-cure"

CHN_total_no_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_no_self_cure_CHN[1091,c("LP"),,],c(2),sum))
colnames(CHN_total_no_self_cure_replicates) = c("total_no_self_cure")

remove(trajectories_cohorts_no_self_cure_CHN)

### CHN SELF CURE###

trajectories_cohorts_self_cure_CHN = readRDS(here::here("results", paste0("trajectories_cohorts_self_cure_CHN_",file_suffix,".Rdata")))

CHN_cohort_self_cure = as.data.frame(t(apply(trajectories_cohorts_self_cure_CHN[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(CHN_cohort_self_cure) = c("lower","median","upper")
CHN_cohort_self_cure$cohort = as.numeric(rownames(CHN_cohort_self_cure))
CHN_cohort_self_cure$scenario = "self-cure"

CHN_total_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_self_cure_CHN[1091,c("LP"),,],c(2),sum))
colnames(CHN_total_self_cure_replicates) = c("total_self_cure")

remove(trajectories_cohorts_self_cure_CHN)

CHN_total_relative = as.data.frame(t(apply(CHN_total_self_cure_replicates/CHN_total_no_self_cure_replicates,2,quantile,c(0.025,0.5,0.975))))
colnames(CHN_total_relative) = c("lower","median","upper")
rownames(CHN_total_relative) = "fraction"
CHN_total_relative$country = "China"

### IND NO SELF CURE###

trajectories_cohorts_no_self_cure_IND = readRDS(here::here("results",paste0("trajectories_cohorts_no_self_cure_IND_",file_suffix,".Rdata")))

IND_cohort_no_self_cure = as.data.frame(t(apply(trajectories_cohorts_no_self_cure_IND[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(IND_cohort_no_self_cure) = c("lower","median","upper")
IND_cohort_no_self_cure$cohort = as.numeric(rownames(IND_cohort_no_self_cure))
IND_cohort_no_self_cure$scenario = "no self-cure"

IND_total_no_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_no_self_cure_IND[1091,c("LP"),,],c(2),sum))
colnames(IND_total_no_self_cure_replicates) = c("total_no_self_cure")

remove(trajectories_cohorts_no_self_cure_IND)

### IND SELF CURE###

trajectories_cohorts_self_cure_IND = readRDS(here::here("results", paste0("trajectories_cohorts_self_cure_IND_",file_suffix,".Rdata")))

IND_cohort_self_cure = as.data.frame(t(apply(trajectories_cohorts_self_cure_IND[1091,c("LP"),,], c(1), quantile, probs = c(0.025,0.5,0.975))))
colnames(IND_cohort_self_cure) = c("lower","median","upper")
IND_cohort_self_cure$cohort = as.numeric(rownames(IND_cohort_self_cure))
IND_cohort_self_cure$scenario = "self-cure"

IND_total_self_cure_replicates = as.data.frame(apply(trajectories_cohorts_self_cure_IND[1091,c("LP"),,],c(2),sum))
colnames(IND_total_self_cure_replicates) = c("total_self_cure")

remove(trajectories_cohorts_self_cure_IND)

IND_total_relative = as.data.frame(t(apply(IND_total_self_cure_replicates/IND_total_no_self_cure_replicates,2,quantile,c(0.025,0.5,0.975))))
colnames(IND_total_relative) = c("lower","median","upper")
rownames(IND_total_relative) = "fraction"
IND_total_relative$country = "India"

font_adjust = 8
title_size = 17 + font_adjust
axis_title_size = 16 + font_adjust
axis_text_size = 15 + font_adjust
axis_text_size_2014 = axis_text_size - 3
title_align = 0.5
title_face = "bold"
legend_title_size = 17 + font_adjust
legend_text_size = 1
legend_text_size_2014 = 19
legend_position = c(0.8, 0.2)
legend_position_2014 = c(0.8, 0.9)
line_thickness = 1.1
colour_self_cure = "#CE2931"
colour_no_self_cure = "#0A74CC"
height = 16
aspect_ratio = 1.4
error_bar_width = 1.4
bar_width = 0.85
error_bar_thickness = 0.8
x_label_shift = 6
alpha = 0.75

labels = c("2014"  = "0-5",
           "2009" = "6-10",
           "2004" = "11-15",
           "1999" = "16-20",
           "1994" = "21-25",
           "1989" = "26-30",
           "1984" = "31-35",
           "1979" = "36-40",
           "1974" = "41-45",
           "1969" = "46-50",
           "1964" = "51-55",
           "1959" = "56-60",
           "1954" = "61-65",
           "1949" = "66-70",
           "1944" = "71-75",
           "1939" = "76-80",
           "1934" = "81-85",
           "1929" = "86-90",
           "1924" = "91-95",
           "1919" = "96-100",
           "1914" = "101+")

a = ggplot() +
  geom_bar(data = JPN_cohort_no_self_cure, stat="identity", aes(x = cohort, y = median/1000, fill = scenario), colour = colour_self_cure) +
  geom_bar(data = JPN_cohort_self_cure, stat="identity", aes(x = cohort, y = median/1000, fill = scenario), alpha = alpha)+
  geom_errorbar(data = JPN_cohort_self_cure, aes(x = cohort, ymin = lower/1000, ymax = upper/1000), width = error_bar_width+1, size = error_bar_thickness) +
  scale_fill_manual(values=c("white", colour_self_cure), labels=c("Lifelong infection","Self-clearance of infection"), guide = FALSE) + 
  scale_x_continuous(trans = "reverse", breaks = seq(2014, 1914, -5), labels = labels, expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 2.5, 0.5), limits = c(0, 2.5), expand = c(0, 0)) +
  labs(title = "Japan",
       x = "Age",
       y = "Number (millions)") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 10, margin = margin(l = 31), size = axis_title_size),
        axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, size = axis_text_size_2014, angle = 90, margin = margin(t = x_label_shift)),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face),
        legend.title = element_blank(),
        legend.position = c(0.275, 0.9),
        legend.text = element_text(size = 18),
        legend.spacing.x = unit(0.15,"cm"))

saveRDS(a,here::here("results",paste0(file_suffix,"_results_country_JPN.Rdata")))

### CHN ###

b = ggplot() +
  geom_bar(data = CHN_cohort_no_self_cure, stat="identity", aes(x = cohort, y = median/1000), fill = "white", colour = colour_self_cure) +
  geom_bar(data = CHN_cohort_self_cure, stat="identity", aes(x = cohort, y = median/1000), fill = colour_self_cure, alpha = alpha) +
  geom_errorbar(data = CHN_cohort_self_cure, aes(x = cohort, ymin = lower/1000, ymax = upper/1000), width = error_bar_width+1, size = error_bar_thickness) +
  scale_x_continuous(trans = "reverse", breaks = seq(2014, 1914, -5), labels = labels, expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40), expand = c(0, 0)) +
  labs(title = "China",
       x = "Age",
       y = "Number (millions)") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 10, margin = margin(l = 31), size = axis_title_size),
        axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, size = axis_text_size_2014, angle = 90, margin = margin(t = x_label_shift)),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none")

saveRDS(b,here::here("results",paste0(file_suffix,"_results_country_CHN.Rdata")))

### IND ###

c = ggplot() +
  geom_bar(data = IND_cohort_no_self_cure, stat="identity", aes(x = cohort, y = median/1000), fill = "white", colour = colour_self_cure) +
  geom_bar(data = IND_cohort_self_cure, stat="identity", aes(x = cohort, y = median/1000), fill = colour_self_cure, alpha = alpha) +
  geom_errorbar(data = IND_cohort_self_cure, aes(x = cohort, ymin = lower/1000, ymax = upper/1000), width = error_bar_width+1, size = error_bar_thickness) +
  scale_x_continuous(trans = "reverse", breaks = seq(2014, 1914, -5), labels = labels, expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40), expand = c(0, 0)) +
  labs(title = "India",
       x = "Age",
       y = "Number (millions)") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 10, margin = margin(l = 31), size = axis_title_size),
        axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5, size = axis_text_size_2014, angle = 90, margin = margin(t = x_label_shift)),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none")

saveRDS(c,here::here("results",paste0(file_suffix,"_results_country_IND.Rdata")))

### All countries ###

d = ggplot() +
  geom_bar(data = IND_total_relative, stat="identity", aes(x = country, y = median), fill = colour_self_cure, width = bar_width, alpha = alpha) +
  geom_errorbar(data = IND_total_relative, aes(x = country, ymin = lower, ymax = upper), width = 0.1, size = error_bar_thickness) +
  geom_bar(data = CHN_total_relative, stat="identity", aes(x = country, y = median), fill = colour_self_cure, width = bar_width, alpha = alpha) +
  geom_errorbar(data = CHN_total_relative, aes(x = country, ymin = lower, ymax = upper), width = 0.1, size = error_bar_thickness) +
  geom_bar(data = JPN_total_relative, stat="identity", aes(x = country, y = median), fill = colour_self_cure, width = bar_width, alpha = alpha) +
  geom_errorbar(data = JPN_total_relative, aes(x = country, ymin = lower, ymax = upper), width = 0.1, size = error_bar_thickness) +
  scale_x_discrete(limits = c("India","China","Japan"), expand = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0, 1), labels = seq(0,100,10),expand = c(0, 0)) +
  labs(title = "Total (relative to lifelong infection)",
       x = "Country",
       y = "Percentage") +
  theme(text = element_text(family = "sans"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(),
        axis.line.x.bottom = element_line(size = line_thickness),
        axis.line.y.left = element_line(size = line_thickness),
        axis.ticks = element_line(size = line_thickness),
        axis.title.x.bottom = element_text(vjust = -4, margin = margin(b = 18), size = axis_title_size),
        axis.title.y.left = element_text(vjust= 10, margin = margin(l = 31), size = axis_title_size),
        axis.text.x.bottom = element_text(hjust = 0.5, vjust = -1, size = axis_text_size_2014, angle = 0, margin = margin(t = x_label_shift)),
        axis.text.y.left = element_text(size = axis_text_size),
        plot.title = element_text(size = title_size, vjust = 8, hjust = title_align, margin = margin(t = 22.5), face = title_face)) +
  theme(legend.position = "none")

saveRDS(d,here::here("results",paste0(file_suffix,"_results_country_ALL.Rdata")))

results_country_fraction = round(100*data.frame("median" = c(JPN_total_relative$median,CHN_total_relative$median,IND_total_relative$median),
                                       "lower" = c(JPN_total_relative$lower,CHN_total_relative$lower,IND_total_relative$lower),
                                       "upper" = c(JPN_total_relative$upper,CHN_total_relative$upper,IND_total_relative$upper)),1)

row.names(results_country_fraction) = c("JPN","CHN","IND")

write.csv(results_country_fraction, here::here("tables_plots",paste0(file_suffix,"_results_country_fraction.csv")))

results_country_reduced = 100 - round(100*data.frame("median" = c(JPN_total_relative$median,CHN_total_relative$median,IND_total_relative$median),
                                                     "lower" = c(JPN_total_relative$upper,CHN_total_relative$upper,IND_total_relative$upper),
                                                     "upper" = c(JPN_total_relative$lower,CHN_total_relative$lower,IND_total_relative$lower)),1)

row.names(results_country_reduced) = c("JPN","CHN","IND")

write.csv(results_country_reduced,here::here("tables_plots",paste0(file_suffix,"_results_country_reduced.csv")))






