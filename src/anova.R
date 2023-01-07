# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(openxlsx)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# means -------------------------------------------------------------------
(means <- dat %>% 
  tidyr::pivot_longer(
    cols = names(dat)[-c(1:9)], names_to = "Parameter"
  ) %>% 
  drop_na(value) %>% 
  group_by(Treatment_Group, Parameter, Year) %>% 
  summarise(Mean = mean(value), Median = median(value)) %>% 
  ungroup())

(means <- means %>% pivot_wider(
  names_from = Parameter, 
  values_from = c(Mean, Median)
))

# prepare data ------------------------------------------------------------
# filter conventional sites
datfull <- dat
dat <- datfull %>% filter(Site != "COV_31")
datcctest <- datfull %>% filter(
  Treatment_Group %in% c("Conv.T.NC", "Conv.T.CC")
)

# anova -------------------------------------------------------------------
###
# specify models 
###
str(dat)
names(dat)
names(dat)[c(10:12,16:19,20:22, 26:27)]
modlist <- list(
  lm(log10(AOA) ~ Year + Management_System / Tillage, data = dat),
  lm(log10(AOB) ~ Year + Management_System / Tillage, data = dat),
  lm(log10(nosZ) ~ Year + Management_System / Tillage, data = dat),
  lm(Net_Mineralization ~ Year + Management_System / Tillage, data = dat),
  lm(Net_Nitrification ~ Year + Management_System / Tillage, data = dat),
  lm(Soil_NH4N ~ Year + Management_System / Tillage, data = dat),
  lm(Soil_NO3N ~ Year + Management_System / Tillage, data = dat),
  lm(log(BG) ~ Year + Management_System / Tillage, data = dat),
  lm(log(NAG) ~ Year + Management_System / Tillage, data = dat),
  lm(log(AP) ~ Year + Management_System / Tillage, data = dat),
  lm(`NAG:BG` ~ Year + Management_System / Tillage, data = dat),
  lm(`NAG:AP` ~ Year + Management_System / Tillage, data = dat)
)
names(modlist) <- names(dat)[c(10:12,16:19,20:22, 26:27)]

###
# extract anova results 
###
# car::Anova(modlist[[1]], type = 2)
anovalist <- lapply(
  # X = modlist[1:2],
  X = modlist, 
  FUN = function(i) {
    tmp <- as.data.frame(car::Anova(i, type = 2))
    # tmp[, c("Df", "F value", "Pr(>F)")]
    data.frame(
      # "Parameter" = names(modlist)[i],
      "DFn.Y" = tmp[1,2], 
      "DFd.Y" = tmp[4,2],
      "F.Y" = tmp[1,3], 
      "P.Y" = tmp[1,4],
      "DFn.M" = tmp[2,2],
      "DFd.M" = tmp[4,2], 
      "F.M" = tmp[2, 3],
      "P.M" = tmp[2, 4],
      "DFn.T" = tmp[3,2],
      "DFd.T" = tmp[4,2],
      "F.T" = tmp[3, 3],
      "P.T" = tmp[3, 4]
    )
  }
)

(anovadf <- do.call("rbind", anovalist))

# double check that the order of entries are correct. 
car::Anova(modlist[[12]], type = 2)

###
# p-value correction
###
adjdf <- data.frame(
  "pvals" = c(anovadf$P.Y, anovadf$P.M, anovadf$P.T),
  "pvals.adj" = p.adjust(
    c(anovadf$P.Y, anovadf$P.M, anovadf$P.T),
    method = "fdr"
  ), 
  "outcome" = rownames(anovadf), 
  "modterm" = c(rep("Year", 12), rep("M_S", 12), rep("T", 12))
)
adjdf$sig.change <- (adjdf$pvals<0.10) != (adjdf$pvals.adj<0.10)
adjdf$sig <- adjdf$pvals.adj<0.10

anovadf$P.Y.fdr <- adjdf$pvals.adj[1:12]
anovadf$P.M.fdr <- adjdf$pvals.adj[13:24]
anovadf$P.T.fdr <- adjdf$pvals.adj[25:36]


# estimated marginal means ------------------------------------------------
emmlist <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Year + Management_System / Tillage, type = "response")
    temm <- as.data.frame(temm)
    return(temm)
  }
)
emmlist

###
# combine EMM results
###
which(
    sapply(
      lapply(emmlist, colnames), FUN = function(i) {("response" %in% (i))}
    ) == TRUE
)

colnames(emmlist[[1]])[4] <- colnames(emmlist[[2]])[4] <- colnames(emmlist[[3]])[4] <- colnames(emmlist[[8]])[4] <- colnames(emmlist[[9]])[4] <- colnames(emmlist[[10]])[4] <- "emmean"

emmdf <- do.call("rbind", emmlist)
emmdf$Parameter <- rep(names(modlist), each = 8)


# export output  ----------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, sheet ="means")
addWorksheet(wb, sheet ="anovadf")
addWorksheet(wb, sheet ="emmdf")

wb$sheet_names

writeData(
  wb, 
  x = means,
  sheet = "means", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

writeData(
  wb, 
  x = anovadf,
  sheet = "anovadf", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

writeData(
  wb, 
  x = emmdf,
  sheet = "emmdf", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

# save to file
saveWorkbook(
  wb, file = file.path(getwd(), "output", "ANOVA_output.xlsx" ),
  overwrite = TRUE
)
