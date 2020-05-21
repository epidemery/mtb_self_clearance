library("ggplot2")
library("patchwork")
library("here")

options(scipen = 999)

file_suffix = "primary"
file_suffix_primary = "primary"

### Primary results ###

### Cohort ###

aspect_ratio = 2.7
height = 16

a = readRDS(here::here("results", paste0(file_suffix_primary,"_results_C_l_self_cure_main_paper.Rdata")))
b = readRDS(here::here("results", paste0(file_suffix_primary,"_results_A_l_self_cure_main_paper.Rdata")))

plot = (a|b) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 24), plot.tag.position = c(0.03,0.965))

plot = plot + plot_annotation(title = 'Cohort model fitted to data over time since infection', theme = theme(plot.title=element_text(size = 28, face = "bold", vjust = 6),plot.margin=unit(c(0.7,0.2,0.2,0.2),"cm")))

plot 

ggsave(here::here("tables_plots", paste0("OUTPUT_plots_cohort_",file_suffix,".png")), width = aspect_ratio*height, height = height, units = "cm", dpi = 300)

### Country ###

aspect_ratio = 1.4
height = 30

a = readRDS(here::here("results", paste0(file_suffix_primary,"_results_country_IND.Rdata")))
b = readRDS(here::here("results", paste0(file_suffix_primary,"_results_country_CHN.Rdata")))
c = readRDS(here::here("results", paste0(file_suffix_primary,"_results_country_JPN.Rdata")))
d = readRDS(here::here("results", paste0(file_suffix_primary,"_results_country_ALL.Rdata")))

plot = (a|b)/plot_spacer()/(c|d) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 24), plot.tag.position = c(0.03,0.965))

plot + plot_annotation(title = expression(paste(bold("Population with a viable "), bolditalic("Mtb "),bold("infection in 2019 - lifelong infection versus self-clearance"))), theme = theme(plot.title=element_text(size = 28, face = "bold", vjust = 6),plot.margin=unit(c(0.7,0.2,0.2,0.2),"cm"))) + plot_layout(heights=c(1,0.05,1))

ggsave(here::here("tables_plots", paste0("OUTPUT_plots_country_",file_suffix,".png")), width = aspect_ratio*height, height = height, units = "cm", dpi = 300)

### Supplementary results ###

### Cohort ###

aspect_ratio = 2.7
height = 16

a = readRDS(here::here("results", paste0(file_suffix_primary,"_results_A_l_no_self_cure_supp.Rdata")))
b = readRDS(here::here("results", paste0(file_suffix_primary,"_results_A_l_self_cure_supp.Rdata")))

plot = (a|b) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 24), plot.tag.position = c(0.03,0.965))

plot = plot + plot_annotation(title = 'Cohort model fitted to TB disease data over time since infection', theme = theme(plot.title=element_text(size = 28, face = "bold", vjust = 6),plot.margin=unit(c(0.7,0.2,0.2,0.2),"cm")))

plot 

ggsave(here::here("tables_plots", paste0("OUTPUT_plots_cohort_supplementary_",file_suffix,".png")), width = aspect_ratio*height, height = height, units = "cm", dpi = 300)

