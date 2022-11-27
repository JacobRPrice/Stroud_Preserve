# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

datfull <- dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# OM_percent vs eea _gSoil ------------------------------------------------
# restructure data
names(datfull)
dat <- pivot_longer(
  data = datfull,
  cols = names(datfull)[-c(1:9, 17, 18)],
  names_to = "Parameter"
)
str(dat)

# remove NA entries
dim(dat)
dat <- dat %>% drop_na(value)
dim(dat)


# subset to just eea data
dat <- subset(
  dat,
  Parameter %in% c(
    # "OM_percent", "Moisture_percent",
    # "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
    "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
    # "BG_gOM", "NAG_gOM", "AP_gOM", 
    "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", 
    "ln(BG):ln(NAG)_gOM", "ln(BG):ln(AP)_gOM"
  )
)

# sort order of variables to be displayed
table(dat$Parameter)
dat$Parameter <- factor(
  dat$Parameter, 
  levels = c(
    # "OM_percent", "Moisture_percent",
    # "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
    "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
    # "BG_gOM", "NAG_gOM", "AP_gOM", 
    "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", 
    "ln(BG):ln(NAG)_gOM", "ln(BG):ln(AP)_gOM"
  )
)


# [ OM_percent vs all ] & [ Moisture_percent vs all ] ---------------------
ggplot(
  data = dat, 
  aes(
    x = OM_percent, 
    y = value, 
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
  theme(
    legend.position = "bottom", 
    axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(
  filename = file.path(getwd(), "figs", "EEA_OM_vs_All.pdf"), 
  width = 6, height = 5*2, units = "in"
)

ggplot(
  data = dat,
  aes(
    x = Moisture_percent,
    y = value,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(
  filename = file.path(getwd(), "figs", "EEA_Moisture_vs_All.pdf"),
  width = 6, height = 5*2, units = "in"
)

# ratio plots: _gSoil -----------------------------------------------------
# shared legends
# https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/
library(patchwork)

# shape = Year,
#     # group = interaction(Year, Treatment_Group),
#     # linetype = Year

pSoil_N.B <- ggplot(
  data = datfull,
  aes(
    x = `ln(NAG)_gSoil`,
    y = `ln(BG)_gSoil`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pSoil_A.B <- ggplot(
  data = datfull,
  aes(
    x = `ln(AP)_gSoil`,
    y = `ln(BG)_gSoil`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pSoil_BGAP.BGNAG <- ggplot(
  data = datfull,
  aes(
    x = `ln(BG):ln(AP)_gSoil`,
    y = `ln(BG):ln(NAG)_gSoil`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pSoil_N.B + pSoil_A.B + pSoil_BGAP.BGNAG +
  plot_layout(guides = "collect", ncol = 1) &
  theme(legend.position = "bottom")

# ratio plots: _gOM -----------------------------------------------------
# shared legends
# https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/
# library(patchwork)

pOM_N.B <- ggplot(
  data = datfull,
  aes(
    x = `ln(NAG)_gOM`,
    y = `ln(BG)_gOM`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pOM_A.B <- ggplot(
  data = datfull,
  aes(
    x = `ln(AP)_gOM`,
    y = `ln(BG)_gOM`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pOM_BGAP.BGNAG <- ggplot(
  data = datfull,
  aes(
    x = `ln(BG):ln(AP)_gOM`,
    y = `ln(BG):ln(NAG)_gOM`,
    color = Treatment_Group#, 
    # shape = Year, 
    # group = interaction(Year, Treatment_Group),
    # linetype = Year
  )
) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) #+
# theme(legend.position = "bottom") +
# guides(color = guide_legend(nrow = 2, byrow = TRUE))

pOM_N.B + pOM_A.B + pOM_BGAP.BGNAG +
  plot_layout(guides = "collect", ncol = 1) &
  theme(legend.position = "bottom")


# ratio plots: join -------------------------------------------------------
pSoil_N.B + pSoil_A.B + pSoil_BGAP.BGNAG +
  pOM_N.B + pOM_A.B + pOM_BGAP.BGNAG +
  plot_layout(guides = "collect", ncol = 2, byrow = FALSE) &
  theme(legend.position = "bottom") &
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(
  filename = file.path(getwd(), "figs", "EEA_Ratio_Plots.pdf"),
  width = 6, height = 5*2, units = "in"
)

# # -------------------------------------------------------------------------
# # -------------------------------------------------------------------------
# # -------------------------------------------------------------------------
# # (original) individual plots ---------------------------------------------
# ggplot(
#   data = dat,
#   aes(
#     x = OM_percent,
#     y = `ln(BG)_gSoil`,
#     color = Treatment_Group
#   )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = dat,
#   aes(
#     x = OM_percent,
#     y = `ln(NAG)_gSoil`,
#     color = Treatment_Group
#   )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = dat,
#   aes(
#     x = OM_percent,
#     y = `ln(AP)_gSoil`,
#     color = Treatment_Group
#   )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = dat,
#   aes(
#     x = OM_percent,
#     y = `ln(BG):ln(NAG)_gSoil`,
#     color = Treatment_Group
#   )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = dat,
#   aes(
#     x = OM_percent,
#     y = `ln(BG):ln(AP)_gSoil`,
#     color = Treatment_Group
#   )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)

# # prepare environment -----------------------------------------------------
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# 
# dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))
# 
# # prepare data ------------------------------------------------------------
# # # restructure data
# # names(dat)
# # str(dat)
# # dat <- pivot_longer(
# #   data = dat, 
# #   cols = names(dat)[-c(1:9)], 
# #   names_to = "Parameter"
# # )
# # str(dat)
# # 
# # # remove NA entries
# # dim(dat)
# # dat <- dat %>% drop_na(value) 
# # dim(dat)
# # 
# # dat <- subset(
# #   dat,
# #   Parameter %in% c(
# #     "OM_percent", "Moisture_percent",
# #     "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
# #     "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", "ln(BG:NAG)_gSoil", "ln(BG:AP)_gSoil", 
# #     "BG_gOM", "NAG_gOM", "AP_gOM", 
# #     "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", "ln(BG:NAG)_gOM", "ln(BG:AP)_gOM"
# #   )
# # )
# 
# # scatter plot ------------------------------------------------------------
# # datwide <- pivot_wider(
# #   data = dat,
# #   names_from = Parameter,
# #   values_from = value
# # )
# ggplot(
#   data = dat, 
#   aes(
#     x = `ln(BG)_gSoil`, 
#     y = `ln(NAG)_gSoil`, 
#     color = Treatment_Group#, 
#     # shape = Year,
#     # group = interaction(Year, Treatment_Group)
#   )
#   # aes_string(
#   #   # x = "ln(GLU)", 
#   #   # y = "ln(NAG)"
#   #   x = "`ln(GLU)`", 
#   #   y = "`ln(NAG)`", 
#   #   # color = "Treatment_Group",
#   #   group = interaction(Year, Treatment_Group)
#   # linetype = Year
#   # )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = datwide, 
#   aes(
#     x = `ln(GLU)`, 
#     y = `ln(PHO)`, 
#     color = Treatment_Group#, 
#     # shape = Year,
#     # group = interaction(Year, Treatment_Group)
#     # linetype = Year
#   )
#   # aes_string(
#   #   # x = "ln(GLU)", 
#   #   # y = "ln(NAG)"
#   #   x = "`ln(GLU)`", 
#   #   y = "`ln(NAG)`", 
#   #   # color = "Treatment_Group",
#   #   group = interaction(Year, Treatment_Group)
#   # )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# ggplot(
#   data = datwide, 
#   aes(
#     x = `ln(GLU:NAG)`, 
#     y = `ln(GLU:PHO)`, 
#     color = Treatment_Group#,
#     # shape = Year,
#     # group = interaction(Year, Treatment_Group),
#     # linetype = Year
#   )
#   # aes_string(
#   #   # x = "ln(GLU)", 
#   #   # y = "ln(NAG)"
#   #   x = "`ln(GLU)`", 
#   #   y = "`ln(NAG)`", 
#   #   # color = "Treatment_Group",
#   #   group = interaction(Year, Treatment_Group)
#   # )
# ) +
#   geom_abline(slope = 1, intercept = 0) +
#   theme_bw() + 
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)
# 
# 
# ggplot(
#   data = dat, 
#   aes(
#     x = Treatment_Group, 
#     y = value, 
#     color = Tillage
#   )
# ) +
#   theme_bw() +
#   geom_boxplot() +
#   geom_jitter(width = 0.2, alpha = 0.5) +
#   facet_grid(Parameter ~ Year, scales = "free_y") + 
#   theme(axis.title.y = element_blank())
# 
# # only 7 points per year...
# with(
#   datwide, 
#   table(Year, Site)
# )
# # but 3 of the 5 treatments have 14 observations per year...
# with(
#   datwide, 
#   table(Year, Treatment_Group)
# )
# # do we want to display the boxplots by both treatment group *and* year, or just treatment group? 