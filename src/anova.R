# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(openxlsx)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# means -------------------------------------------------------------------
(conv.mean <- dat %>% filter(Site == "COV_31") %>% 
   tidyr::pivot_longer(
     cols = names(dat)[-c(1:9)], names_to = "Parameter"
   ) %>% 
   drop_na(value) %>% 
   group_by(Year, Parameter) %>% 
   summarise(Conv.Mean = mean(value))
)  

(means <- dat %>% 
  tidyr::pivot_longer(
    cols = names(dat)[-c(1:9)], names_to = "Parameter"
  ) %>% 
  drop_na(value) %>% 
  group_by(Treatment_Group, Parameter) %>% 
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
  Treatment_Group %in% c("Conv.NC.T", "Conv.CC.T")
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
      "p.T" = tmp[3, 4]
    )
  }
)

(anovadf <- do.call("rbind", anovalist))

# double check that the order of entries are correct. 
car::Anova(modlist[[12]], type = 2)


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


# comparisons: year -------------------------------------------------------
contlistY <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Year, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
    return(tcont)
  }
)
contlistY

### 
# combine year contrast list
###
which(
  sapply(
    lapply(contlistY, colnames), FUN = function(i) {("null" %in% (i))}
  ) == TRUE
)
contlistY[[1]] <- contlistY[[1]][,-5]
contlistY[[2]] <- contlistY[[2]][,-5]
contlistY[[3]] <- contlistY[[3]][,-5]
contlistY[[8]] <- contlistY[[8]][,-5]
contlistY[[9]] <- contlistY[[9]][,-5]
contlistY[[10]] <- contlistY[[10]][,-5]

colnames(contlistY[[1]])[2] <- colnames(contlistY[[2]])[2] <- colnames(contlistY[[3]])[2] <- colnames(contlistY[[8]])[2] <- colnames(contlistY[[9]])[2] <- colnames(contlistY[[10]])[2] <- "estimate"

contdfY <- do.call("rbind", contlistY)

# comparisons: management system ------------------------------------------
contlistM <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Management_System, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
    return(tcont)
  }
)
contlistM

###
# combine management contrast list
### 
which(
  sapply(
    lapply(contlistM, colnames), FUN = function(i) {("null" %in% (i))}
  ) == TRUE
)
contlistM[[1]] <- contlistM[[1]][,-5]
contlistM[[2]] <- contlistM[[2]][,-5]
contlistM[[3]] <- contlistM[[3]][,-5]
contlistM[[8]] <- contlistM[[8]][,-5]
contlistM[[9]] <- contlistM[[9]][,-5]
contlistM[[10]] <- contlistM[[10]][,-5]

colnames(contlistM[[1]])[2] <- colnames(contlistM[[2]])[2] <- colnames(contlistM[[3]])[2] <- colnames(contlistM[[8]])[2] <- colnames(contlistM[[9]])[2] <- colnames(contlistM[[10]])[2] <- "estimate"

contdfM <- do.call("rbind", contlistM)

# comparisons: tillage (within management system) -------------------------
contlistT <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Tillage | Management_System, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
  }
)
contlistT

###
# combine tillage contrast list
### 
which(
  sapply(
    lapply(contlistT, colnames), FUN = function(i) {("null" %in% (i))}
  ) == TRUE
)
contlistT[[1]] <- contlistT[[1]][,-6]
contlistT[[2]] <- contlistT[[2]][,-6]
contlistT[[3]] <- contlistT[[3]][,-6]
contlistT[[8]] <- contlistT[[8]][,-6]
contlistT[[9]] <- contlistT[[9]][,-6]
contlistT[[10]] <- contlistT[[10]][,-6]

colnames(contlistT[[1]])[3] <- colnames(contlistT[[2]])[3] <- colnames(contlistT[[3]])[3] <- colnames(contlistT[[8]])[3] <- colnames(contlistT[[9]])[3] <- colnames(contlistT[[10]])[3] <- "estimate"

contdfT <- do.call("rbind", contlistT)
contdfT$Parameter <- rep(names(modlist), each = 2)


# test for impact of cover crop  ------------------------------------------
modlistcc <- list(
  lm(log10(AOA) ~ Cover_Crop, data = datcctest),
  lm(log10(AOB) ~ Cover_Crop, data = datcctest),
  lm(log10(nosZ) ~ Cover_Crop, data = datcctest),
  lm(Net_Mineralization ~ Cover_Crop, data = datcctest),
  lm(Net_Nitrification ~ Cover_Crop, data = datcctest),
  lm(Soil_NH4N ~ Cover_Crop, data = datcctest),
  lm(Soil_NO3N ~ Cover_Crop, data = datcctest),
  lm(log(BG) ~ Cover_Crop, data = datcctest),
  lm(log(NAG) ~ Cover_Crop, data = datcctest),
  lm(log(AP) ~ Cover_Crop, data = datcctest),
  lm(`NAG:BG` ~ Cover_Crop, data = datcctest),
  lm(`NAG:AP` ~ Cover_Crop, data = datcctest)
)
names(modlistcc) <- names(dat)[c(10:12,16:19,20:22, 26:27)]


contlistC <- lapply(
  X = modlistcc, 
  FUN = function(i) {
    temm <- emmeans(i, specs = ~ Cover_Crop, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
  }
)
contlistC

###
# combine cover crop contrast list
### 
which(
  sapply(
    lapply(contlistC, colnames), FUN = function(i) {("null" %in% (i))}
  ) == TRUE
)
contlistC[[1]] <- contlistC[[1]][,-5]
contlistC[[2]] <- contlistC[[2]][,-5]
contlistC[[3]] <- contlistC[[3]][,-5]
contlistC[[8]] <- contlistC[[8]][,-5]
contlistC[[9]] <- contlistC[[9]][,-5]
contlistC[[10]] <- contlistC[[10]][,-5]

colnames(contlistC[[1]])[2] <- colnames(contlistC[[2]])[2] <- colnames(contlistC[[3]])[2] <- colnames(contlistC[[8]])[2] <- colnames(contlistC[[9]])[2] <- colnames(contlistC[[10]])[2] <- "estimate"

contdfC <- do.call("rbind", contlistC)

# export output  ----------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, sheet ="conv_mean")
addWorksheet(wb, sheet ="means")
addWorksheet(wb, sheet ="anovadf")
addWorksheet(wb, sheet ="emmdf")
addWorksheet(wb, sheet ="contdfY")
addWorksheet(wb, sheet ="contdfM")
addWorksheet(wb, sheet ="contdfT")
addWorksheet(wb, sheet ="contdfC")

wb$sheet_names

writeData(
  wb, 
  x = conv.mean,
  sheet = "conv_mean", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

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

writeData(
  wb, 
  x = contdfY,
  sheet = "contdfY", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

writeData(
  wb, 
  x = contdfM,
  sheet = "contdfM", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

writeData(
  wb, 
  x = contdfT,
  sheet = "contdfT", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

writeData(
  wb, 
  x = contdfC,
  sheet = "contdfC", 
  startCol = 1, startRow = 1, 
  rowNames = TRUE, colNames = TRUE
)

# save to file
saveWorkbook(
  wb, file = file.path(getwd(), "output", "ANOVA_output.xlsx" ),
  overwrite = TRUE
)
