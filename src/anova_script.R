# -------------------------------------------------------------------------
# Only for Running Manually -----------------------------------------------
# -------------------------------------------------------------------------
# this section is only for running the script manually, for both building/writing and debugging. 
# tvar <- "NO3Nstock"
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Purpose:  ---------------------------------------------------------------
# this script will be applied to each of the response variables in the SP dataset. 
# it will automate the process of performing a number of statistical tests, figure generation, and printing output to files. 

# resources ---------------------------------------------------------------
# https://www.statology.org/two-way-anova-r/

# collect arguments -------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
tvar <- args[1]

tvar 
# class(tvar)

# tasks and output that this script must perform/create -------------------
# ) table of count of observations for each treatment. 
# ) perform anova: 
#     * two-way anova, type II SS
#   - for each model; 
#     * obtain tidy output
#     * check anova model assumptions: 
#       - independence (done though the study design, can't test for')
#       - Normality: plot histogram of the model residuals
#       - equal variance: car::leveneTest
#       - 4-panel plot
#     * post-hoc test via TukeyHSD
# ) create boxplots, off ALL sites (not just those included in the anova)

# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
# library(rstatix)
# library(ggpubr)
# library(broom)
# library(tidyverse)
# library(corrplot)
# library(MASS)
datfull <- dat <- 
  readRDS(file.path(getwd(), "/data/", "dat.RDS"))


# prepare data ------------------------------------------------------------
# str(dat)

# remove NA entries
dim(dat)
dat <- dat %>% drop_na(eval(tvar))
dim(dat)

# if qPCR data is being passed, we must take the log of the values 
if (tvar %in% c("AOA", "AOB", "nosZ")) {
  message("Taking log10 of qPCR data.")
  dat[tvar] <- log10(dat[tvar])
  # message("Renaming qPCR data for clarity in plots")
  # names(dat)[which(names(dat) == tvar)] <- paste0("log10(", tvar,")")
  # tvar <- paste0("log10(", tvar,")")
}




# create boxplot ----------------------------------------------------------
# ggpubr used to create these plots
# https://rpkgs.datanovia.com/ggpubr/
p <- ggpubr::ggboxplot(
  data = dat, 
  x = "Treatment_Group", 
  y = tvar,
  col = "Tillage", 
  add = "jitter",
  shape = "Management_System"
)

ggsave(
  file.path(getwd(), "figs", paste0(tvar,"_anova_boxplot.pdf")), 
  width = 6, height = 5, units = "in"
)

# subset data -------------------------------------------------------------
# filter conventional sites
dat <- dat %>% filter(Site != "COV_31")

# stat summary of data ----------------------------------------------------
# https://stackoverflow.com/questions/9057006/getting-strings-recognized-as-variable-names-in-r
# count, mean, and sd
(out_sample_count_summary <- dat %>% group_by(Management_System, Tillage) %>%
  summarize(
    ct = n(),
    # mean2 = mean(AOA),
    # sd2 = sd(AOA),
    # mean = mean(eval(as.name(tvar))), # both of these work
    mean = mean(eval(as.symbol(tvar))), # both of these work
    sd = sd(eval(as.symbol(tvar)))
  ) %>% ungroup())

(out_man_summary <- dat %>% group_by(Management_System) %>% 
  rstatix::get_summary_stats(eval(tvar), type = "mean_sd"))

(out_till_summary <- dat %>% group_by(Tillage) %>% 
  rstatix::get_summary_stats(eval(tvar), type = "mean_sd"))

# anova -------------------------------------------------------------------
# original / old way of performing these calcs using base stats and car packages.
# I'm going to also try to use the rstatix package instead because it avoids the need for messing with contrasts (it does it automatically).
twoway <- lm(
  as.formula(paste0(tvar, " ~ Management_System + Tillage")), 
  contrasts = list(Management_System = contr.sum, Tillage = contr.sum),
  data = dat
)
# summary(twoway)
out_twoway <- car::Anova(twoway, type = 2)

coef(twoway)

# coef(out_twoway)
# # we will double check these estimate values...
# names(coef(twoway))
# coef(twoway)["(Intercept)"] + 1*coef(twoway)["Management_System1"]
# # the above does NOT match what I was expecting! 
# # this is most likely due to the different number of observations. 
# 
# # conventional till
# coef(twoway)["(Intercept)"] + 1*coef(twoway)["Management_System1"] + 1*coef(twoway)["Tillage1"]
# # org red till
# coef(twoway)["(Intercept)"] + -1*coef(twoway)["Management_System1"] + -1*coef(twoway)["Tillage1"]
# 
# # will using the rstatix package work better? 
# # i will try this later. 
# # For now... let's work on investigating the use of an interaction model

# start with the interaction model. 
inter <- lm(
  as.formula(paste0(tvar, " ~ Management_System * Tillage")), 
  contrasts = list(Management_System = contr.sum, Tillage = contr.sum),
  data = dat
)
# summary(inter)
out_inter <- car::Anova(inter, type = 2)

coef(inter)

# coef(out_inter)
# # we will double check these estimate values...
# names(coef(inter))
# # conventional till
# coef(inter)["(Intercept)"] + 
#   1*coef(inter)["Management_System1"] + 
#   1*coef(inter)["Tillage1"] +
#   1*coef(inter)["Management_System1:Tillage1"] 
# # org red till
# coef(inter)["(Intercept)"] + 
#   -1*coef(inter)["Management_System1"] + 
#   -1*coef(inter)["Tillage1"] +
#   1*coef(inter)["Management_System1:Tillage1"] 
# # it looks like this is the correct way to obtain these values. 

# look at residuals 
png(
  file.path(getwd(), "figs", paste0(tvar,"_anova_residuals.png")), 
  width = 2*480, height = 2*480
)
par(mfrow=c(2,2))
plot(twoway)
dev.off()

par(mfrow=c(1,1))

# TODO: I need to check for each test's assumptions. 
# check for normality
# https://www.statology.org/anova-assumptions/
# shapiro.test(dat$AOA)

# ###
# # trying with rstatix package
# ###
# # at least partly following the example pipeline here: 
# # https://www.datanovia.com/en/lessons/anova-in-r/#computation-1
# dat %>% group_by(Management_System, Tillage) %>% 
#   identify_outliers(eval(tvar))

# save output -------------------------------------------------------------
# out <- list(
#   "sample_count_summary" = out_sample_summary,
#   "man_summary" = out_man_summary, 
#   "till_summary" = out_till_summary,
#   "twoway" = out_twoway
# )
# 
# openxlsx::write.xlsx(
#   out, 
#   file = file.path(getwd(), "output", paste0(tvar, "_anova_output.xlsx")), 
#   rowNames = TRUE,
#   colNames = TRUE, 
#   overwrite = TRUE
# )

if (!"ANOVA_output.xlsx" %in% # check to see if excel file exists
    list.files(path = file.path(getwd(), "output"))) {
  
  # create excel file (workbook object) to accept data, if not present in directory 
  wb <- createWorkbook()
  
} else {
  wb <- loadWorkbook(file.path(getwd(), "output", "ANOVA_output.xlsx" ))
}

if (tvar %in% wb$sheet_names) {
  # remove old sheet
  removeWorksheet(wb, sheet = tvar)
  # create new worksheet
  addWorksheet(wb, sheetName = tvar)
} else {
  # create new worksheet
  addWorksheet(wb, sheetName = tvar)
}


# add output to workbook
writeData(
  wb, sheet = tvar, 
  x = out_sample_count_summary, 
  startCol = 1, 
  startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = out_man_summary, 
  startCol = 1, 
  startRow = 7, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = out_till_summary, 
  startCol = 1, 
  startRow = 11, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = out_twoway, 
  startCol = 1, 
  startRow = 15, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = coef(twoway), 
  startCol = 6, 
  startRow = 15, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = out_inter, 
  startCol = 1, 
  startRow = 20, 
  rowNames = TRUE, colNames = TRUE
)
writeData(
  wb, sheet = tvar, 
  x = coef(inter), 
  startCol = 6, 
  startRow = 20, 
  rowNames = TRUE, colNames = TRUE
)

# save to file
saveWorkbook(
  wb, file = file.path(getwd(), "output", "ANOVA_output.xlsx" ),
  overwrite = TRUE
)
