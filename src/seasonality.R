# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------
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
  Parameter %in% c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N")
)
eea <- subset(
  dat,
  Parameter %in% c(
    "OM_percent", "Moisture_percent",
    # "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
        "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
        "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
    # "BG_gOM", "NAG_gOM", "AP_gOM", 
        "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", 
        "ln(BG):ln(NAG)_gOM", "ln(BG):ln(AP)_gOM"
  )
)
datmain <- subset(
  dat, 
  Parameter %in% c(
    "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
    
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
    "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
    
    "AOA", "AOB", "nosZ"
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
    "OM_percent", 
    # "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
        "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
        "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
    "Moisture_percent",
    # "BG_gOM", "NAG_gOM", "AP_gOM", 
        "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", 
        "ln(BG):ln(NAG)_gOM", "ln(BG):ln(AP)_gOM"
  )
)

datmain$Parameter <- factor(
  datmain$Parameter,
  levels = c(
    "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
    
    "AOA", "AOB", "nosZ", 
    
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
    "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil"
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
    # facet_grid(Parameter~., scales = "free_y") +
    facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
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
    # facet_grid(Parameter~., scales = "free_y") +
    facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(), 
      legend.position = "bottom"
    )
)

ggsave(
  plot = pqpcr,
  filename = file.path(getwd(), "figs", "2020_vs_2021_qPCR.pdf"), 
  # width = 6, height = 3*2, units = "in"
  width = 6, height = 5, units = "in"
)
ggsave(
  plot = pnitmin,
  filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin.pdf"), 
  # width = 6, height = 4*2, units = "in"
  width = 6.5, height = 6, units = "in"
)
ggsave(
  plot = peea,
  filename = file.path(getwd(), "figs", "2020_vs_2021_EEA.pdf"), 
  # width = 6, height = 5*2, units = "in"
  # width = 6, height = 18*2, units = "in"
  width = 6.5, height = 10, units = "in"
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
    # facet_grid(Parameter~., scales = "free_y") +
    facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
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
    # facet_grid(Parameter~., scales = "free_y") +
    facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
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
  # width = 6, height = 3*2, units = "in"
  width = 6, height = 5, units = "in"
)
ggsave(
  plot = apnitmin,
  filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin_supp.pdf"), 
  # width = 6, height = 4*2, units = "in"
  width = 6.5, height = 6, units = "in"
)
ggsave(
  plot = apeea,
  filename = file.path(getwd(), "figs", "2020_vs_2021_EEA_supp.pdf"), 
  # width = 6, height = 5*2, units = "in"
  # width = 6, height = 18*2, units = "in"
  width = 6.5, height = 10, units = "in"
)


# main text figure --------------------------------------------------------
ggplot(
  data = datmain, 
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
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  scale_x_continuous(limits = c(60,340)) + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) #+
  # guides(color = guide_legend(nrow = 2, byrow = TRUE)) #+
  # guides(linetype = guide_legend(nrow = 2))

ggsave(
  filename = file.path(getwd(), "figs", "2020_vs_2021_MAIN.pdf"), 
  width = 6.5, height = 10, units = "in"
)
