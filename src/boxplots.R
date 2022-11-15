# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------

# # take log of qPCR data
# dat$AOA <- log10(dat$AOA)
# dat$AOB <- log10(dat$AOB)
# dat$nosZ <- log10(dat$nosZ)

# # subset to just the eea data of interest 
# dat <- dat %>% select(-c("GLU_gOM", "NAG_gOM", "PHO_gOM"))

# rename eea variables for cleanliness 
dat <- rename(
  dat, 
  GLU = GLU_gSoil, 
  NAG = NAG_gSoil, 
  PHO = PHO_gSoil
)

# subset by analysis type
metadata <- c("Date", "Site", "Management_System" , "Tillage", "Cover_Crop", "Treatment_Group")
qpcr <- subset(
  dat, 
  select = c(
    metadata, c("AOA", "AOB", "nosZ")
  )
)
NitMin <- subset(
  dat,
  select = c(
    metadata, c("Net_Mineralization", "Net_Nitrification", "Soil_NH4N", "Soil_NO3N")
  )
)
eea <- subset(
  dat,
  select = c(
    metadata, c("OM_percent", "Moisture_percent", "GLU", "NAG", "PHO"))
)


# restructure data
qpcr <- pivot_longer(
  data = qpcr, 
  cols = names(qpcr)[-c(1:6)], 
  names_to = "Parameter"
)
NitMin <- pivot_longer(
  data = NitMin, 
  cols = names(NitMin)[-c(1:6)], 
  names_to = "Parameter"
)
eea <- pivot_longer(
  data = eea, 
  cols = names(eea)[-c(1:6)], 
  names_to = "Parameter"
)

# remove NA entries
qpcr <- qpcr %>% drop_na(value)
NitMin <- NitMin %>% drop_na(value) 
eea <- eea %>% drop_na(value) 


# plot --------------------------------------------------------------------
(pqpcr <- ggpubr::ggboxplot(
  data = qpcr, 
  x = "Treatment_Group", 
  y = "value",
  col = "Tillage", 
  add = "jitter",
  shape = "Management_System",
  ylab = FALSE,
  ggtheme = theme_bw()
))  
(pqpcr <- ggpubr::facet(
  pqpcr, 
  facet.by = "Parameter", 
  scales = "free",
  ncol = 1
) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = rel(0.6)), 
      axis.text.x = element_text(size = rel(0.7))
    ) +
    scale_y_continuous(trans = "log10")
)

(pnitmin <- ggpubr::ggboxplot(
  data = NitMin, 
  x = "Treatment_Group", 
  y = "value",
  col = "Tillage", 
  add = "jitter",
  shape = "Management_System",
  ylab = FALSE,
  ggtheme = theme_bw()
))  
(pnitmin <- ggpubr::facet(
  pnitmin, 
  facet.by = "Parameter", 
  scales = "free",
  nrow = 2,
  ncol = 2
) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = rel(0.6)), 
      axis.text.x = element_text(size = rel(0.7))
    )
)

(peea <- ggpubr::ggboxplot(
  data = eea, 
  x = "Treatment_Group", 
  y = "value",
  col = "Tillage", 
  add = "jitter",
  shape = "Management_System",
  ylab = FALSE,
  ggtheme = theme_bw()
))  
(peea <- ggpubr::facet(
  peea, 
  facet.by = "Parameter", 
  scales = "free",
  nrow = 3,
  ncol = 2
) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(angle = 90, hjust = 0.5, size = rel(0.6)), 
      axis.text.x = element_text(size = rel(0.7))
    )
)


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
  width = 6, height = 6, units = "in"
)

