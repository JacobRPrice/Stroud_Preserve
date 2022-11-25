# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------
# # get year and month/day
# dat$Year <- lubridate::year(dat$Date)
# dat$Year <- as.factor(dat$Year)
# dat$Month <- lubridate::month(dat$Date)
# dat$Day <- lubridate::yday(dat$Date)

# # take log of qPCR data
# dat$AOA <- log10(dat$AOA)
# dat$AOB <- log10(dat$AOB)
# dat$nosZ <- log10(dat$nosZ)

# # rename eea variables for cleanliness 
# dat <- rename(
#   dat, 
#   GLU = GLU_gSoil, 
#   NAG = NAG_gSoil, 
#   PHO = PHO_gSoil
# )

# # calc eea ratios
# dat$"GLU:NAG" <- dat$GLU/dat$NAG
# dat$"ln(GLU:NAG)" <- log(dat$`GLU:NAG`)

# restructure data
names(dat)
str(dat)
dat <- pivot_longer(
  data = dat, 
  cols = names(dat)[-c(1:9)],
  names_to = "Parameter"
)
str(dat)

# remove NA entries
dim(dat)
dat <- dat %>% drop_na(value) 
dim(dat)

# subset by analysis type
qpcr <- subset(
  dat,
  Parameter %in% c("AOA", "AOB", "nosZ")
)
NitMin <- subset(
  dat,
  # Parameter %in% c("netMIN", "netNIT", "NH4Nstock", "NO3Nstock")
  Parameter %in% c("Net_Mineralization", "Net_Nitrification", "Soil_NH4N", "Soil_NO3N")
)
# cbind(
#   names(dat)[17:34],
#   c(
#     "OM_percent", "Moisture_percent",
#     "GLU_gSoil", "NAG_gSoil", "PHO_gSoil",
#     "GLU_gOM", "NAG_gOM", "PHO_gOM",
#     "ln(GLU)_gSoil", "ln(NAG)_gSoil", "ln(PHO)_gSoil", "ln(GLU:NAG)_gSoil", "ln(GLU:PHO)_gSoil",
#     "ln(GLU)_gOM", "ln(NAG)_gOM", "ln(PHO)_gOM", "ln(GLU:NAG)_gOM", "ln(GLU:PHO)_gOM")
# )
eea <- subset(
  dat,
  Parameter %in% c(
    "OM_percent", "Moisture_percent",
    "GLU_gSoil", "NAG_gSoil", "PHO_gSoil", 
    "GLU_gOM", "NAG_gOM", "PHO_gOM", 
    "ln(GLU)_gSoil", "ln(NAG)_gSoil", "ln(PHO)_gSoil", "ln(GLU:NAG)_gSoil", "ln(GLU:PHO)_gSoil", 
    "ln(GLU)_gOM", "ln(NAG)_gOM", "ln(PHO)_gOM", "ln(GLU:NAG)_gOM", "ln(GLU:PHO)_gOM"
    # "GLU:NAG"
    # "ln(GLU:NAG)"
    # "GLU_gSoil", "NAG_gSoil", "PHO_gSoil"#, 
    # "GLU_gOM", "NAG_gOM", "PHO_gOM"
  )
)

# order parameters according to how they need to be displayed
qpcr$Parameter <- factor(
  qpcr$Parameter, 
  levels = c("AOA", "AOB", "nosZ")
)
NitMin$Parameter <- factor(
  NitMin$Parameter, 
  levels = c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N")
)
eea$Parameter <- factor(
  eea$Parameter, 
  levels = c(
    "OM_percent", "Moisture_percent",
    "GLU_gSoil", "NAG_gSoil", "PHO_gSoil", 
    "GLU_gOM", "NAG_gOM", "PHO_gOM", 
    "ln(GLU)_gSoil", "ln(NAG)_gSoil", "ln(PHO)_gSoil", "ln(GLU:NAG)_gSoil", "ln(GLU:PHO)_gSoil", 
    "ln(GLU)_gOM", "ln(NAG)_gOM", "ln(PHO)_gOM", "ln(GLU:NAG)_gOM", "ln(GLU:PHO)_gOM"
  )
)

# plot --------------------------------------------------------------------
(pqpcr <- ggplot(
  data = qpcr, 
  aes(
    x = Day,
    y = value, 
    group = interaction(Year, Treatment_Group),
    color = Year
  )
) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Parameter~., scales = "free_y") +
  scale_x_continuous(limits = c(60,340)) + 
  scale_y_continuous(trans = "log10") + 
  # ylab("Gene Copies / g Soil (Log10 Scaled)") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  )
)

(pnitmin <- ggplot(
  data = NitMin, 
  aes(
    x = Day,
    y = value, 
    group = interaction(Year, Treatment_Group),
    color = Year
  )
) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(Parameter~., scales = "free_y") +
    # facet_grid(
    #   factor(Parameter, levels = c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N"))~., 
    #   scales = "free_y"
    # ) +
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(), 
      legend.position = "bottom"
    )
)

(peea <- ggplot(
  data = eea, 
  aes(
    x = Day,
    y = value, 
    group = interaction(Year, Treatment_Group),
    color = Year
  )
) +
    theme_bw() +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(Parameter~., scales = "free_y") +
    # facet_grid(
    #   factor(Parameter, levels = c(
    #     "GLU", "NAG", "PHO", 
    #     # "Moisture_percent", "OM_percent", 
    #     # "GLU:NAG"
    #     "ln(GLU:NAG)"
    #   ))~., 
    #   scales = "free_y"
    # ) +
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(), 
      legend.position = "bottom"
    )
)

ggsave(
  plot = pqpcr,
  filename = file.path(getwd(), "figs", "2020_vs_2021_qPCR.pdf"), 
  width = 6, height = 3*2, units = "in"
)
ggsave(
  plot = pnitmin,
  filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin.pdf"), 
  width = 6, height = 4*2, units = "in"
)
ggsave(
  plot = peea,
  filename = file.path(getwd(), "figs", "2020_vs_2021_EEA.pdf"), 
  # width = 6, height = 5*2, units = "in"
  width = 6, height = 18*2, units = "in"
)



# supplemental plots ------------------------------------------------------
# these plots 
#   * based on the main plots above 
#   * use color to distinguish between different treatment groups
#   * use linetype to distinguish between years

(apqpcr <- ggplot(
  data = qpcr, 
  aes(
    x = Day,
    y = value, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
  theme_bw() +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(Parameter~., scales = "free_y") +
  scale_x_continuous(limits = c(60,340)) + 
  scale_y_continuous(trans = "log10") + 
  # ylab("Gene Copies / g Soil (Log10 Scaled)") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  guides(linetype = guide_legend(nrow = 2))
)

(apnitmin <- ggplot(
  data = NitMin, 
  aes(
    x = Day,
    y = value, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(Parameter~., scales = "free_y") +
    # facet_grid(
    #   factor(Parameter, levels = c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N"))~., 
    #   scales = "free_y"
    # ) +
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(linetype = guide_legend(nrow = 2))
)

(apeea <- ggplot(
  data = eea, 
  aes(
    x = Day,
    y = value, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
    theme_bw() +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(Parameter~., scales = "free_y") +
    # facet_grid(
    #   factor(Parameter, levels = c("GLU", "NAG", "PHO", "Moisture_percent", "OM_percent"))~., 
    #   scales = "free_y"
    # ) +
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(linetype = guide_legend(nrow = 2))
)

ggsave(
  plot = apqpcr,
  filename = file.path(getwd(), "figs", "2020_vs_2021_qPCR_supp.pdf"), 
  width = 6, height = 3*2, units = "in"
)
ggsave(
  plot = apnitmin,
  filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin_supp.pdf"), 
  width = 6, height = 4*2, units = "in"
)
ggsave(
  plot = apeea,
  filename = file.path(getwd(), "figs", "2020_vs_2021_EEA_supp.pdf"), 
  # width = 6, height = 5*2, units = "in"
  width = 6, height = 18*2, units = "in"
)

