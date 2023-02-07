# prepare environment -----------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(broom)
library(openxlsx)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))
smsdat <- readRDS(file.path(getwd(), "/data/", "SMS_dat.RDS"))

# calculate daily means, per field ----------------------------------------
sdat <- smsdat %>% group_by(Date, Field) %>% 
  filter(
    (hour(DateTime_EST) >= 8) & (hour(DateTime_EST) < 12)
  ) %>% 
  summarize(
    Soil_Moisture = mean(Segment1_percent)
  ) %>% ungroup()

# join datasets -----------------------------------------------------------
str(dat)
str(smsdat)

nchar(dat$Site)
dat$Field <- substr(dat$Site, nchar(dat$Site) - 1, nchar(dat$Site))

dat <- left_join(
  x = dat, 
  y = sdat,
  by = c("Date", "Field")
)

# spearman correlations ---------------------------------------------------
corlist.s <- list(
  cor.test(log10(dat$AOA), dat$Soil_Moisture, method = "spearman"),
  cor.test(log10(dat$AOB), dat$Soil_Moisture, method = "spearman"),
  cor.test(log10(dat$nosZ), dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$Net_Mineralization, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$Net_Nitrification, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$Soil_NH4N, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$Soil_NO3N, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$BG, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$NAG, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$AP, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$`NAG:BG`, dat$Soil_Moisture, method = "spearman"),
  cor.test(dat$`NAG:AP`, dat$Soil_Moisture, method = "spearman")
)
names(dat)
names(corlist.s) <- names(dat)[c(10:12, 16:19, 20:22, 26:27)]

corlist.s <- lapply(X = corlist.s, broom::tidy)
corres.s <- do.call("rbind", corlist.s)

# pearson correlations ----------------------------------------------------
corlist.p <- list(
  cor.test(log10(dat$AOA), dat$Soil_Moisture, method = "pearson"),
  cor.test(log10(dat$AOB), dat$Soil_Moisture, method = "pearson"),
  cor.test(log10(dat$nosZ), dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$Net_Mineralization, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$Net_Nitrification, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$Soil_NH4N, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$Soil_NO3N, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$BG, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$NAG, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$AP, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$`NAG:BG`, dat$Soil_Moisture, method = "pearson"),
  cor.test(dat$`NAG:AP`, dat$Soil_Moisture, method = "pearson")
)
names(dat)
names(corlist.p) <- names(dat)[c(10:12, 16:19, 20:22, 26:27)]

corlist.p <- lapply(X = corlist.p, broom::tidy)
corres.p <- do.call("rbind", corlist.p)

# merge correlation results together --------------------------------------
# rename spearman first
cor.test(log10(dat$AOA), dat$Soil_Moisture, method = "spearman")
names(corres.s)
corres.s

corres.s <- corres.s %>% rename(
  rho = estimate, 
  S = statistic, 
  p.value.s = p.value,
  method.s = method,
  alternative.s = alternative
)
corres.s$Parameter <- names(corlist.s)
corres.s <- corres.s %>% relocate(Parameter, .before = rho)

# rename pearson next 
cor.test(log10(dat$AOA), dat$Soil_Moisture, method = "pearson")
names(corres.p)
corres.p

corres.p <- corres.p %>% rename(
  corr = estimate, 
  t.p = statistic, 
  p.value.p = p.value,
  df.p = parameter, 
  conf.low.p = conf.low, 
  conf.high.p = conf.high, 
  method.p = method,
  alternative.p = alternative
)
corres.p$Parameter <- names(corlist.p)
corres.p <- corres.p %>% relocate(Parameter, .before = corr)

# join 
corres <- full_join(
  x = corres.s, 
  y = corres.p, 
  by = "Parameter"
)

# now we need to get how many observations could/should exist for each cor.test, and how many of those actually exist
parvec <- names(dat)[c(10:12, 16:19, 20:22, 26:27)]
corres$comp.cases <- unlist(
  lapply(
    X = parvec, 
    FUN = function(i, j = "Soil_Moisture", refdat = dat) {
      tmpdat <- refdat[, c(i, j)]
      # tmpdat
      # class(tmpdat)
      # dim(tmpdat)
      # with(tmpdat, table(i, j))
      sum(complete.cases(tmpdat))
    }
  )
)

# save cor results --------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, sheet = "corres")

writeData(
  wb, 
  x = corres,
  sheet = "corres",
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

# saveWorkbook(
#   wb, 
#   file = file.path(getwd(), "output", "correlation_output.xlsx"), 
#   overwrite = TRUE
# )
