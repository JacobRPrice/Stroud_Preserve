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
    c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N")
  )
)
eea <- subset(
  dat,
  select = c(
    metadata, 
    c(
      # "BG", "NAG", "AP",
      "ln(BG)", "ln(NAG)", "ln(AP)", 
      "NAG:BG", "NAG:AP"
    )
  )
)
datmain <- subset(
  dat,
  select = c(
    metadata,
    c(
      "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
      
      # "AOA", "AOB", "nosZ", 
      "log(AOA)", "log(AOB)", "log(nosZ)", 
      
      # "BG", "NAG", "AP",
      "ln(BG)", "ln(NAG)", "ln(AP)", 
      "NAG:BG", "NAG:AP"
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
    # "BG", "NAG", "AP",
    "ln(BG)", "ln(NAG)", "ln(AP)", 
    "NAG:BG", "NAG:AP"
  )
)
datmain$Parameter <- factor(
  datmain$Parameter,
  levels = c(
    "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
    
    # "AOA", "AOB", "nosZ", 
    "log(AOA)", "log(AOB)", "log(nosZ)", 
    
    # "BG", "NAG", "AP",
    "ln(BG)", "ln(NAG)", "ln(AP)", 
    "NAG:BG", "NAG:AP"
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
    color = Year
  )
) +
  theme_bw() +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.height = 0)) +
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
    color = Year
  )
) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    geom_point(position = position_jitterdodge(jitter.height = 0)) +
    facet_grid(Parameter ~ ., scales = "free_y") + 
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
    color = Year
  )
) +
    theme_bw() +
    geom_boxplot() +
    geom_point(position = position_jitterdodge(jitter.height = 0)) +
    facet_grid(Parameter ~ ., scales = "free_y") + 
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
  width = 6, height = 6, units = "in"
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
  width = 6, height = 8, units = "in"
)

# main text figures -------------------------------------------------------
ggplot(
  data = datmain, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Year
  )
) +
  theme_bw() +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.height = 0)) +
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  theme(
    legend.position = "bottom", 
    axis.title.y = element_blank()
  ) 

ggsave(
  filename = file.path(getwd(), "figs", "boxplot_MAIN.pdf"), 
  width = 18, height = 24, units = "cm"
)
