# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(openxlsx)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------

# # subset to just the eea data of interest 
dat <- dat %>% select(-c("GLU_gOM", "NAG_gOM", "PHO_gOM"))

# rename eea variables for cleanliness 
dat <- rename(
  dat, 
  GLU = GLU_gSoil, 
  NAG = NAG_gSoil, 
  PHO = PHO_gSoil
)

# filter conventional sites
datfull <- dat
dat <- dat %>% filter(Site != "COV_31")

# conventional means for use in table -------------------------------------
(conv.mean <- datfull %>% filter(Site == "COV_31") %>% 
   tidyr::pivot_longer(
     cols = names(datfull)[-c(1:6)], names_to = "Parameter"
   ) %>% 
   drop_na(value) %>% 
   group_by(Parameter) %>% 
   summarise(Conv.Mean = mean(value))
)  


# anova -------------------------------------------------------------------

###
# specify models 
###
# specify_model <- function(targvar) {
#   # targvar <- deparse(substitute(targvar))
#   # print(dim(dat))
#   tmpdat <- dat %>% drop_na(eval(targvar))
#   # print(dim(tmpdat))
#   tmpmod <- lm(
#     as.formula(paste0(targvar, " ~ Management_System / Tillage")), 
#     data = tmpdat
#   )
#   return(tmpmod)
# }
# # specify_model("AOA")
# 
# modlist <- lapply(
#   X = names(dat[7:18]), 
#   FUN = specify_model
# )
# names(modlist) <- names(dat)[7:18]

modlist <- list(
  lm(log10(AOA) ~ Management_System / Tillage, data = dat),
  lm(log10(AOB) ~ Management_System / Tillage, data = dat),
  lm(log10(nosZ) ~ Management_System / Tillage, data = dat),
  lm(Net_Mineralization ~ Management_System / Tillage, data = dat),
  lm(Net_Nitrification ~ Management_System / Tillage, data = dat),
  lm(Soil_NH4N ~ Management_System / Tillage, data = dat),
  lm(Soil_NO3N ~ Management_System / Tillage, data = dat),
  lm(OM_percent ~ Management_System / Tillage, data = dat),
  lm(Moisture_percent ~ Management_System / Tillage, data = dat),
  lm(GLU ~ Management_System / Tillage, data = dat),
  lm(NAG ~ Management_System / Tillage, data = dat),
  lm(PHO ~ Management_System / Tillage, data = dat)
)
names(modlist) <- names(dat)[7:18]


###
# extract anova results 
###
anovalist <- lapply(
  # X = modlist[1:2],
  X = modlist, 
  FUN = function(i) {
    tmp <- as.data.frame(car::Anova(i, type = 2))
    # tmp[, c("Df", "F value", "Pr(>F)")]
    data.frame(
      # "Parameter" = names(modlist)[i],
      "DFn.M" = tmp[1,2],
      "DFd.M" = tmp[3,2],
      "F.M" = tmp[1, 3],
      "P.M" = tmp[1, 4],
      "DFn.T" = tmp[2,2],
      "DFd.T" = tmp[3,2],
      "F.T" = tmp[2, 3],
      "p.T" = tmp[2, 4]
    )
  }
)

(anovadf <- do.call("rbind", anovalist))

# double check that the order of entries are correct. 
car::Anova(modlist[[1]], type = 2)


# estimated marginal means ------------------------------------------------
emmlist <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Management_System / Tillage, type = "response")
    temm <- as.data.frame(temm)
    return(temm)
  }
)
emmlist

# rename "response" to "emmean" so we can bind qPCR data entries with others 
colnames(emmlist[[1]])[3] <- "emmean"
colnames(emmlist[[2]])[3] <- "emmean"
colnames(emmlist[[3]])[3] <- "emmean"

emmdf <- do.call("rbind", emmlist)
emmdf$Parameter <- rep(names(modlist), each = 4)

# comparisons: management system ------------------------------------------
contlistM <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Management_System, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
    return(tcont)
    # tcld <- multcomp::cld(tcont)
    # return(as.data.frame(tcld))
    # return(list("contrasts" = tcont, "cld" = tcld))
  }
)
contlistM

# remove extra column in qPCR entries
contlistM[[3]]
contlistM[[4]]
names(contlistM[[1]])
contlistM[[1]] <- contlistM[[1]][,-5]
contlistM[[2]] <- contlistM[[2]][,-5]
contlistM[[3]] <- contlistM[[3]][,-5]
# rename "ratio" to "estimate" so we can bind qPCR data entries with others 
colnames(contlistM[[1]])[2] <- "estimate"
colnames(contlistM[[2]])[2] <- "estimate"
colnames(contlistM[[3]])[2] <- "estimate"

contdfM <- do.call("rbind", contlistM)

# do.call(
#   "rbind", 
#   lapply(
#     X = modlist, 
#     FUN = function(i) {
#       temm <- emmeans(object = i, specs = ~ Management_System)
#       tcont <- pairs(temm)
#       return(tcont)
#       # tcont <- contrast(temm, method = "pairwise")
#       # tcont <- as.data.frame(tcont)
#       # return(tcont)
#       # tcld <- multcomp::cld(tcont)
#       # return(as.data.frame(tcld))
#       # return(list("contrasts" = tcont, "cld" = tcld))
#     }
#   )      
# )

# comparisons: tillage (within management system) -------------------------
# emmeans(modlist$OM_percent, specs = ~ Tillage)
# emmeans(modlist$OM_percent, specs = ~ Tillage, type = "response")
# emmeans(modlist$OM_percent, specs = ~ Tillage | Management_System)
# emmeans(modlist$OM_percent, specs = ~ Tillage | Management_System, type = "response")
# 
# contrast(emmeans(modlist$OM_percent, specs = ~ Tillage))
# contrast(emmeans(modlist$OM_percent, specs = ~ Tillage | Management_System))
# contrast(emmeans(modlist$OM_percent, specs = ~ Tillage | Management_System),  method = "pairwise")

contlistT <- lapply(
  X = modlist, 
  FUN = function(i) {
    temm <- emmeans(object = i, specs = ~ Tillage | Management_System, type = "response")
    tcont <- contrast(temm, method = "pairwise")
    tcont <- as.data.frame(tcont)
  }
)
contlistT

# remove extra column in qPCR entries
contlistT[[3]]
contlistT[[4]]
names(contlistT[[1]])
contlistT[[1]] <- contlistT[[1]][,-6]
contlistT[[2]] <- contlistT[[2]][,-6]
contlistT[[3]] <- contlistT[[3]][,-6]
# rename "ratio" to "estimate" so we can bind qPCR data entries with others 
colnames(contlistT[[1]])[3] <- "estimate"
colnames(contlistT[[2]])[3] <- "estimate"
colnames(contlistT[[3]])[3] <- "estimate"

contdfT <- do.call("rbind", contlistT)
contdfT$Parameter <- rep(names(modlist), each = 2)


# export output  ----------------------------------------------------------
if (!"ANOVA_output.xlsx" %in% # check to see if excel file exists
    list.files(path = file.path(getwd(), "output"))) {
  
  # create excel file (workbook object) to accept data, if not present in directory 
  wb <- createWorkbook()
  addWorksheet(wb, sheet ="conv_mean")
  addWorksheet(wb, sheet ="anovadf")
  addWorksheet(wb, sheet ="emmdf")
  addWorksheet(wb, sheet ="contdfM")
  addWorksheet(wb, sheet ="contdfT")
  
} else {
  wb <- loadWorkbook(file.path(getwd(), "output", "ANOVA_output.xlsx" ))
  
  removeWorksheet(wb, sheet ="conv_mean")
  removeWorksheet(wb, sheet ="anovadf")
  removeWorksheet(wb, sheet ="emmdf")
  removeWorksheet(wb, sheet ="contdfM")
  removeWorksheet(wb, sheet ="contdfT")
  
  addWorksheet(wb, sheet ="conv_mean")
  addWorksheet(wb, sheet ="anovadf")
  addWorksheet(wb, sheet ="emmdf")
  addWorksheet(wb, sheet ="contdfM")
  addWorksheet(wb, sheet ="contdfT")
}


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


# save to file
saveWorkbook(
  wb, file = file.path(getwd(), "output", "ANOVA_output.xlsx" ),
  overwrite = TRUE
)
