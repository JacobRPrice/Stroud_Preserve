# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------

# subset by analysis type
metadata <- c("Date", "Site", "Management_System" , "Tillage", "Cover_Crop", "Treatment_Group", "Year", "Month", "Day")
# qpcr <- subset(
#   dat, 
#   select = c(
#     metadata, 
#     c("AOA", "AOB", "nosZ")
#   )
# )
# NitMin <- subset(
#   dat,
#   select = c(
#     metadata, 
#     c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N")
#   )
# )
# eea <- subset(
#   dat,
#   select = c(
#     metadata, 
#     c(
#       # "BG", "NAG", "AP",
#       "ln(BG)", "ln(NAG)", "ln(AP)", 
#       "NAG:BG", "NAG:AP"
#     )
#   )
# )
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
# names(qpcr)
# qpcr <- pivot_longer(
#   data = qpcr, 
#   cols = names(qpcr)[-c(1:9)], 
#   names_to = "Parameter"
# )
# NitMin <- pivot_longer(
#   data = NitMin, 
#   cols = names(NitMin)[-c(1:9)], 
#   names_to = "Parameter"
# )
# eea <- pivot_longer(
#   data = eea, 
#   cols = names(eea)[-c(1:9)], 
#   names_to = "Parameter"
# )
datmain <- pivot_longer(
  data = datmain, 
  cols = names(datmain)[-c(1:9)], 
  names_to = "Parameter"
)


# order parameters according to how they need to be displayed
# qpcr$Parameter <- factor(
#   qpcr$Parameter, 
#   levels = c("AOA", "AOB", "nosZ")
# )
# NitMin$Parameter <- factor(
#   NitMin$Parameter, 
#   levels = c("Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N")
# )
# eea$Parameter <- factor(
#   eea$Parameter, 
#   levels = c(
#     # "BG", "NAG", "AP",
#     "ln(BG)", "ln(NAG)", "ln(AP)", 
#     "NAG:BG", "NAG:AP"
#   )
# )
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
# qpcr <- qpcr %>% drop_na(value)
# NitMin <- NitMin %>% drop_na(value) 
# eea <- eea %>% drop_na(value) 
datmain <- datmain %>% drop_na(value)

# supplemental plots ------------------------------------------------------
# these plots 
#   * based on the main plots 
#   * use color to distinguish between different treatment groups
#   * use linetype to distinguish between years

# (apqpcr <- ggplot(
#   data = qpcr, 
#   aes(
#     x = Day,
#     y = value, 
#     color = Treatment_Group, 
#     linetype = Year
#   )
# ) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(se = FALSE, size = 0.5) +
#   facet_grid(Parameter~., scales = "free_y") +
#   scale_x_continuous(limits = c(60,340)) + 
#   scale_y_continuous(trans = "log10") + 
#   # ylab("Gene Copies / g Soil (Log10 Scaled)") +
#   theme(
#     axis.title.y = element_blank(), 
#     panel.grid.major.x = element_blank(),
#     legend.position = "bottom"
#   ) +
#   guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#   guides(linetype = guide_legend(nrow = 2)) +
#   xlab("Day of the Year")
# )
# 
# (apnitmin <- ggplot(
#   data = NitMin, 
#   aes(
#     x = Day,
#     y = value, 
#     color = Treatment_Group, 
#     linetype = Year
#   )
# ) +
#     theme_bw() +
#     geom_hline(yintercept = 0) +
#     geom_point() +
#     geom_smooth(se = FALSE, size = 0.5) +
#     facet_grid(Parameter~., scales = "free_y") +
#     # facet_wrap(Parameter ~., scales = "free_y", ncol = 2, dir = "v") +
#     scale_x_continuous(limits = c(60,340)) + 
#     theme(
#       axis.title.y = element_blank(), 
#       panel.grid.major.x = element_blank(),
#       legend.position = "bottom"
#     ) +
#     guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#     guides(linetype = guide_legend(nrow = 2)) +
#     xlab("Day of the Year")
# )
# 
# (apeea <- ggplot(
#   data = eea, 
#   aes(
#     x = Day,
#     y = value, 
#     color = Treatment_Group, 
#     linetype = Year
#   )
# ) +
#     theme_bw() +
#     geom_point() +
#     geom_smooth(se = FALSE, size = 0.5) +
#     facet_grid(Parameter~., scales = "free_y") +
#     # facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
#     scale_x_continuous(limits = c(60,340)) + 
#     theme(
#       axis.title.y = element_blank(), 
#       panel.grid.major.x = element_blank(),
#       legend.position = "bottom"
#     ) +
#     guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#     guides(linetype = guide_legend(nrow = 2)) +
#     xlab("Day of the Year")
# )

(apall <- ggplot(
  data = datmain, 
  aes(
    x = Day,
    y = value, 
    color = Treatment_Group, 
    linetype = Year
  )
) +
  theme_bw() +
  # geom_vline(xintercept = 180, color = "red") +
  # geom_vline(xintercept = 225, color = "red") +
  # geom_vline(xintercept = 265, color = "blue") +
  geom_point() +
  geom_smooth(se = FALSE, size = 0.5) +
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  scale_x_continuous(limits = c(60,340)) + 
  theme(
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(linetype = guide_legend(nrow = 2)) +
    xlab("Day of the Year")
)

# save figures ------------------------------------------------------------
# ggsave(
#   plot = apqpcr,
#   filename = file.path(getwd(), "figs", "2020_vs_2021_qPCR.pdf"), 
#   # width = 6, height = 3*2, units = "in"
#   width = 6, height = 6, units = "in"
# )
# ggsave(
#   plot = apnitmin,
#   filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin.pdf"), 
#   # width = 6, height = 4*2, units = "in"
#   width = 6, height = 6, units = "in"
# )
# ggsave(
#   plot = apeea,
#   filename = file.path(getwd(), "figs", "2020_vs_2021_EEA.pdf"), 
#   # width = 6, height = 5*2, units = "in"
#   # width = 6, height = 18*2, units = "in"
#   width = 6, height = 8, units = "in"
# )
ggsave(
  plot = apall,
  filename = file.path(getwd(), "figs", "2020_vs_2021_supp.pdf"), 
  width = 18, height = 24, units = "cm"
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
  # geom_vline(xintercept = 180, color = "red") +
  # geom_vline(xintercept = 225, color = "red") +
  # geom_vline(xintercept = 265, color = "blue") +
  geom_point() +
  geom_smooth(se = FALSE, size = 0.5) +
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  scale_x_continuous(limits = c(60,340)) + 
  theme(
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )  +
  xlab("Day of the Year")

ggsave(
  filename = file.path(getwd(), "figs", "2020_vs_2021_MAIN.pdf"), 
  width = 18, height = 24, units = "cm"
)
