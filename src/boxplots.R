# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------

# subset by analysis type
metadata <- c("Date", "Site", "Management_System" , "Tillage", "Cover_Crop", "Treatment_Group", "Year", "Month", "Day")
qpcr <- subset(
  dat, 
  select = c(
    metadata, 
    c("AOA", "AOB", "nosZ")
  )
)
NitMin <- subset(
  dat,
  select = c(
    metadata, 
    c("Net_Mineralization", "Net_Nitrification", "Soil_NH4N", "Soil_NO3N")
  )
)
eea <- subset(
  dat,
  select = c(
    metadata, 
    c(
      "OM_percent", "Moisture_percent",
      "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
      "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", "ln(BG:NAG)_gSoil", "ln(BG:AP)_gSoil", 
      "BG_gOM", "NAG_gOM", "AP_gOM", 
      "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", "ln(BG:NAG)_gOM", "ln(BG:AP)_gOM"
    )
  )
)

# restructure data
names(qpcr)
qpcr <- pivot_longer(
  data = qpcr, 
  cols = names(qpcr)[-c(1:9)], 
  names_to = "Parameter"
)
NitMin <- pivot_longer(
  data = NitMin, 
  cols = names(NitMin)[-c(1:9)], 
  names_to = "Parameter"
)
eea <- pivot_longer(
  data = eea, 
  cols = names(eea)[-c(1:9)], 
  names_to = "Parameter"
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
    "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", "ln(BG:NAG)_gSoil", "ln(BG:AP)_gSoil", 
    "BG_gOM", "NAG_gOM", "AP_gOM", 
    "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", "ln(BG:NAG)_gOM", "ln(BG:AP)_gOM"
  )
)

# remove NA entries
qpcr <- qpcr %>% drop_na(value)
NitMin <- NitMin %>% drop_na(value) 
eea <- eea %>% drop_na(value) 

# create boxplots ---------------------------------------------------------

(pqpcr <- ggplot(
  data = qpcr, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Tillage
  )
) +
  theme_bw() +
  geom_boxplot() +
  geom_jitter(aes(shape = Year), width = 0.2, alpha = 0.5) +
  facet_grid(Parameter ~ ., scales = "free_y") + 
  theme(
    legend.position = "bottom", 
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(trans = "log10") 
)

(pnitmin <- ggplot(
  data = NitMin, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Tillage
  )
) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    geom_jitter(aes(shape = Year), width = 0.2, alpha = 0.5) +
    # facet_grid(Parameter ~ ., scales = "free_y") + 
    facet_wrap(Parameter ~ ., scales = "free_y", nrow = 2) +
    theme(
      legend.position = "bottom", 
      axis.title.y = element_blank()
    ) 
)

(peea <- ggplot(
  data = eea, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Tillage
  )
) +
    theme_bw() +
    geom_boxplot() +
    geom_jitter(aes(shape = Year), width = 0.2, alpha = 0.5) +
    facet_grid(Parameter ~ ., scales = "free_y") + 
    theme(
      legend.position = "bottom", 
      axis.title.y = element_blank()
    ) 
)

# save figures ------------------------------------------------------------
ggsave(
  plot = pqpcr, 
  filename = file.path(getwd(), "figs", "boxplot_qPCR.pdf"), 
  width = 6, height = 3*2, units = "in"
)
ggsave(
  plot = pnitmin, 
  filename = file.path(getwd(), "figs", "boxplot_NitMin.pdf"), 
  width = 6, height = 8, units = "in"
)
ggsave(
  plot = peea, 
  filename = file.path(getwd(), "figs", "boxplot_EEA.pdf"), 
  # width = 6, height = 6, units = "in"
  width = 6, height = 18*2, units = "in"
)
