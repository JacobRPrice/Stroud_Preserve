# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# generate plots ----------------------------------------------------------

(pNAGBG <- ggplot(
  data = dat, 
  aes(
    x = `ln(BG)`,
    y = `ln(NAG)`, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.50, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)
)

(pNAGAP <- ggplot(
  data = dat, 
  aes(
    x = `ln(AP)`,
    y = `ln(NAG)`, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = 0.50, size = 0.5) +
    geom_smooth(method = "lm", se = FALSE, size = 0.5)
)

pNAGBG + pNAGAP +
  plot_layout(guides = "collect", ncol = 1)

# ggsave(
#   filename = file.path(getwd(), "figs", "EEA_Ratio_Plots.pdf"),
#   width = 6, height = 6, units = "in"
# )
