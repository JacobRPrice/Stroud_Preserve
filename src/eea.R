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

# save plots --------------------------------------------------------------
pNAGBG + pNAGAP +
  plot_layout(guides = "collect", ncol = 1) &
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  ) &
  guides(color = guide_legend(nrow = 5, byrow = FALSE))

ggsave(
  filename = file.path(getwd(), "figs", "EEA_Ratio_Plots.pdf"),
  width = 10, height = 2*8.5+2, units = "cm"
)
