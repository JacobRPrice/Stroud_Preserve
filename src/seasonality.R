# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------
# subset
metadata <- c("Date", "Site", "Management_System" , "Tillage", "Cover_Crop", "Treatment_Group", "Year", "Month", "Day")

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
datmain <- pivot_longer(
  data = datmain, 
  cols = names(datmain)[-c(1:9)], 
  names_to = "Parameter"
)

# order parameters according to how they need to be displayed
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
datmain <- datmain %>% drop_na(value)

# supplemental plots ------------------------------------------------------
# these plots 
#   * based on the main plots 
#   * use color to distinguish between different treatment groups
#   * use linetype to distinguish between years

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
  geom_point() +
  geom_smooth(se = FALSE, size = 0.5) +
  facet_wrap(
    Parameter~., scales = "free_y", ncol = 2, dir = "v", 
    labeller = as_labeller(
      c(
        Net_Nitrification = "Net Nitrification",
        Soil_NH4N = "Soil NH4-N", 
        Net_Mineralization = "Net Mineralization",
        Soil_NO3N = "Soil NO3-N", 
        "log(AOA)" = "log(AOA)",
        "log(AOB)" = "log(AOB)",
        "log(nosZ)" = "log(nosZ)",
        "ln(BG)" = "ln(BG)", 
        "ln(NAG)" = "ln(NAG)",
        "ln(AP)" = "ln(AP)",
        "NAG:BG" = "NAG:BG", 
        "NAG:AP" = "NAG:AP"
      )
    ),
    strip.position = "left"
  ) +
  scale_x_continuous(limits = c(60,340)) + 
  theme(
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom", 
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(linetype = guide_legend(nrow = 2)) +
    xlab("Day of the Year")
)

# ggsave(
#   plot = apall,
#   filename = file.path(getwd(), "figs", "2020_vs_2021_supp.pdf"),
#   width = 18, height = 22, units = "cm"
# )

# main text figure --------------------------------------------------------
ggplot(
  data = datmain, 
  aes(
    x = Day,
    y = value, 
    # group = interaction(Year, Treatment_Group),
    color = Year
  )
) +
  theme_bw() +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, size = 0.5) +
  facet_wrap(
    Parameter~., scales = "free_y", ncol = 2, dir = "v", 
    labeller = as_labeller(
      c(
        Net_Nitrification = "Net Nitrification",
        Soil_NH4N = "Soil NH4-N", 
        Net_Mineralization = "Net Mineralization",
        Soil_NO3N = "Soil NO3-N", 
        "log(AOA)" = "log(AOA)",
        "log(AOB)" = "log(AOB)",
        "log(nosZ)" = "log(nosZ)",
        "ln(BG)" = "ln(BG)", 
        "ln(NAG)" = "ln(NAG)",
        "ln(AP)" = "ln(AP)",
        "NAG:BG" = "NAG:BG", 
        "NAG:AP" = "NAG:AP"
      )
    ),
    strip.position = "left"
  ) +
  scale_x_continuous(limits = c(60,340)) + 
  theme(
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom", 
    strip.background = element_blank(),
    strip.placement = "outside"
  )  +
  xlab("Day of the Year")

# ggsave(
##   filename = file.path(getwd(), "figs", "2020_vs_2021_MAIN.pdf"),
#   filename = file.path(getwd(), "figs", "Figure_3.pdf"),
#   width = 18, height = 22, units = "cm"
# )
