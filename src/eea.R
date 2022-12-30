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
    color = Treatment_Group
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)
)

(pNAGAP <- ggplot(
  data = dat, 
  aes(
    x = `ln(AP)`,
    y = `ln(NAG)`, 
    color = Treatment_Group
  )
) +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 0.5)
)

# save plots --------------------------------------------------------------
pNAGBG + pNAGAP +
  plot_layout(guides = "collect", ncol = 1) &
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  ) &
  guides(color = guide_legend(nrow = 3, byrow = FALSE))

ggsave(
  filename = file.path(getwd(), "figs", "EEA_Ratio_Plots.pdf"),
  width = 8.5, height = 2*8.5+2, units = "cm"
)
