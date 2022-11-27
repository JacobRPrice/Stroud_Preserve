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
      # "BG_gSoil", "NAG_gSoil", "AP_gSoil", 
          "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
          "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil", 
      # "BG_gOM", "NAG_gOM", "AP_gOM", 
          "ln(BG)_gOM", "ln(NAG)_gOM", "ln(AP)_gOM", 
          "ln(BG):ln(NAG)_gOM", "ln(BG):ln(AP)_gOM"
    )
  )
)
datmain <- subset(
  dat,
  select = c(
    metadata,
    c(
      "Net_Mineralization", "Soil_NH4N",
      
      "AOA", "AOB", "nosZ", 
      
      "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
      "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil"
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
datmain <- pivot_longer(
  data = datmain, 
  cols = names(datmain)[-c(1:9)], 
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
    "Net_Mineralization", "Soil_NH4N",
    
    "AOA", "AOB", "nosZ", 
    
    "ln(BG)_gSoil", "ln(NAG)_gSoil", "ln(AP)_gSoil", 
    "ln(BG):ln(NAG)_gSoil", "ln(BG):ln(AP)_gSoil"
  )
)

# remove NA entries
qpcr <- qpcr %>% drop_na(value)
NitMin <- NitMin %>% drop_na(value) 
eea <- eea %>% drop_na(value) 
datmain <- datmain %>% drop_na(value)

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
    facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
    theme(
      legend.position = "bottom", 
      axis.title.y = element_blank(), 
      axis.text.x = element_text(size = rel(0.85))
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
    # facet_grid(Parameter~., scales = "free_y") +
    facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
    theme(
      legend.position = "bottom", 
      axis.title.y = element_blank(), 
      axis.text.x = element_text(size = rel(0.85))
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
  # width = 6, height = 8, units = "in"
  width = 6, height = 6, units = "in"
)
ggsave(
  plot = peea, 
  filename = file.path(getwd(), "figs", "boxplot_EEA.pdf"), 
  # width = 6, height = 6, units = "in"
  width = 6, height = 10, units = "in"
)

# main text figures -------------------------------------------------------
ggplot(
  data = datmain, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Tillage
  )
) +
  theme_bw() +
  geom_boxplot() +
  geom_jitter(aes(shape = Year), width = 0.2, alpha = 0.5) +
  # facet_grid(Parameter~., scales = "free_y") +
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  theme(
    legend.position = "bottom", 
    axis.title.y = element_blank(), 
    axis.text.x = element_text(size = rel(0.85))
  ) 

ggsave(
  filename = file.path(getwd(), "figs", "boxplot_MAIN.pdf"), 
  width = 6.5, height = 10, units = "in"
)
