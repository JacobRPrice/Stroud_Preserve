# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------
# get year and month/day
dat$Year <- lubridate::year(dat$Date)
dat$Year <- as.factor(dat$Year)
dat$Month <- lubridate::month(dat$Date)
dat$Day <- lubridate::yday(dat$Date)

# take log of qPCR data
dat$AOA <- log10(dat$AOA)
dat$AOB <- log10(dat$AOB)
dat$nosZ <- log10(dat$nosZ)

# restructure data
str(dat)
dat <- pivot_longer(
  data = dat, 
  cols = names(dat)[-c(1:6, 22:24)], 
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
  Parameter %in% c("netMIN", "netNIT", "NH4Nstock", "NO3Nstock")
)
eea <- subset(
  dat,
  Parameter %in% c(
    "OM_percent", "Moisture_percent", 
    "GLU_gSoil", "NAG_gSoil", "PHO_gSoil"#, 
    # "GLU_gOM", "NAG_gOM", "PHO_gOM"
  )
)

# plot --------------------------------------------------------------------
# TODO: reorder facets in these plots
(pq <- ggplot(
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
    scale_x_continuous(limits = c(60,340)) + 
    theme(
      axis.title.y = element_blank(), 
      legend.position = "bottom"
    )
)

ggsave(
  plot = pq,
  filename = file.path(getwd(), "figs", "2020_vs_2021_qPCR.pdf"), 
  width = 8, height = 3*3, units = "in"
)
ggsave(
  plot = pnitmin,
  filename = file.path(getwd(), "figs", "2020_vs_2021_NitMin.pdf"), 
  width = 8, height = 4*3, units = "in"
)
ggsave(
  plot = peea,
  filename = file.path(getwd(), "figs", "2020_vs_2021_qEEA.pdf"), 
  width = 8, height = 5*3, units = "in"
)


# (pq <- ggplot(
#   data = qpcr, 
#   aes(
#     x = Day,
#     y = value, 
#     group = Treatment_Group,
#     color = Year,
#     # color = Treatment_Group, 
#     linetype = Year
#   )
# ) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(se = FALSE) +
#   # stat_smooth(se = FALSE) +
#   facet_grid(Parameter~., scales = "free_y")
# )
# 
# 
# (pq <- ggplot(
#   data = qpcr, 
#   aes(
#     x = Day,
#     y = value
#   )
# ) +
#     theme_bw() +
#     geom_point(
#       aes(
#           group = Treatment_Group,
#           color = Year)
#     ) +
#     facet_grid(Parameter~., scales = "free_y") +
#     geom_smooth(
#       aes(
#         group = interaction(Year, Treatment_Group),
#         color = Year
#       ), 
#       se = FALSE
#     )
# )
# 
# # https://ggplot2-book.org/collective-geoms.html
# # https://groups.google.com/g/ggplot2/c/vd5n1jR9k40?pli=1
# # https://dplyr.tidyverse.org/articles/programming.html
# 
# 
# 
# 
# (pq <- ggplot(
#   data = qpcr, 
#   aes(
#     x = Day,
#     y = value, 
#     group = Treatment_Group
#     # color = Year,
#     # color = Treatment_Group, 
#     # linetype = Year
#   )
# ) +
#     theme_bw() +
#     geom_point() +
#     # geom_line() +
#     stat_summary(aes(group = Treatment_Group), fun = mean, geom = "line") +
#     facet_grid(Parameter~., scales = "free_y")
# )
# 
# 
# ggplot(
#   data = qpcr, 
#   aes(
#     # x = Date, 
#     x = Day,
#     y = value, 
#     # group = Treatment_Group,
#     color = Treatment_Group, 
#     # color = Year#, 
#     # linetype = as.factor(Year) # I need to figure out how to have two trendlines, one for each year in this plot. 
#     linetype = Year
#   )
# ) +
#   theme_bw() +
#   facet_grid(Parameter~., scales = "free_y") +
#   # scale_x_date(
#   #   date_labels = "%m/%y",
#   #   limits = c(as.Date("2020-01-01"), as.Date("2021-12-31")),
#   #   breaks = scales::date_breaks("2 months")
#   # ) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(se = FALSE) #+
# # # geom_smooth() +
# # # stat_smooth(geom = "line") +
# # # stat_smooth(geom = "line", alpha = 0.5) +
# # theme(
# #   # legend.position = "bottom",
# #   axis.title.x = element_blank(),
# #   axis.title.y = element_text(size = rel(.8)),
# #   axis.text.x = element_text(size = rel(.8)),
# #   axis.text.y = element_text(
# #     size = rel(.8), 
# #     angle = 90, hjust = +0.5, vjust = 1)
# # ) #+
# # # labs(
# # #   y = "Enzyme Activity [umol/hr/g Soil]"
# # # ) +
# # # facet_grid(Parameter~., scales = "free_y")