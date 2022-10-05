# Prepare Environment -----------------------------------------------------
library(dplyr)

# Sample Data -------------------------------------------------------------
sampledata <- read.csv(
  file = file.path(getwd(), "data", "sample_data.csv"), 
  header = TRUE
)

# Sample Data: cleanup, format, calculate values --------------------------
str(sampledata)

sampledata$SWRC_ID <- as.numeric(sampledata$SWRC_ID)
sampledata$Date <- as.Date(sampledata$Date)
sampledata <- subset(sampledata, select = -c(Used_for_QPCR))

# remove "SP_" from site names
sampledata$Site <- sapply(stringr::str_split(sampledata$Site, pattern = "SP_"), "[[", 2)

# remove sub-plot labels
table(sampledata$Site)
sampledata$Site[which(substr(sampledata$Site, nchar(sampledata$Site), nchar(sampledata$Site)) %in% c("A", "B", "C", "D"))] <-
  substr(
    sampledata$Site[which(substr(sampledata$Site, nchar(sampledata$Site), nchar(sampledata$Site)) %in% c("A", "B", "C", "D"))],
    1,
    nchar(sampledata$Site[which(substr(sampledata$Site, nchar(sampledata$Site), nchar(sampledata$Site)) %in% c("A", "B", "C", "D"))])-1
  )
table(sampledata$Site)

# obtain system & entry point identifier
sampledata$ID_Sys_EP <- sapply(stringr::str_split(sampledata$Site, pattern = "_"), "[[", 2)
table(sampledata$ID_Sys_EP)

str(sampledata)

# Treatments --------------------------------------------------------------
treats <- read.csv(
  file = file.path(getwd(), "data", "treatments.csv"), 
  header = TRUE
)

# Treatments: cleanup, format, calculate values ---------------------------
str(treats)

treats$ID_Sys_EP <- as.character(treats$ID_Sys_EP)

# factor treatments
treats$Fertility_Source <- factor(
  treats$Fertility_Source,
  levels = c("Synthetic Fertilizer", "Legume")
)
treats$Management_System <- factor(
  treats$Management_System,
  levels = c("Conventional", "Organic")
)
treats$Tillage <- factor(
  treats$Tillage,
  levels = c("Till", "Reduced Till")
)
treats$Cover_Crop <- factor(
  treats$Cover_Crop,
  levels = c("No Cover Crop", "Cover Crop")
)

str(treats)

# Create scaffold for data ------------------------------------------------
str(sampledata)
str(treats)

dat <- left_join(
  sampledata, 
  treats, 
  by = c("ID_Sys_EP")
)

dat <- unique(
  subset(
    dat, 
    select = c(Date, Site, Management_System, Tillage, Cover_Crop)
  )
)

dat <- as_tibble(dat)

str(dat)
table(dat$Date, dat$Site)

# qPCR --------------------------------------------------------------------
(data_files <- list.files(
  file.path(getwd(), "data"),
  full.names = TRUE
))

(fls_qpcr <- data_files[grep("qPCR_", data_files)])
targetvec <- sapply(
  strsplit(gsub("[.]", "_", basename(fls_qpcr)), split = "_"),
  "[[",
  2
)
yearvec <- sapply(
  strsplit(gsub("[.]", "_", basename(fls_qpcr)), split = "_"),
  "[[",
  3
)
yearvec <- as.numeric(yearvec)

cbind(
  basename(fls_qpcr), 
  targetvec, 
  yearvec
)

qpcrls <- vector(mode = "list", length = length(fls_qpcr))
for (i in 1:length(fls_qpcr)) {
  tmp <- read.csv(
    file = fls_qpcr[i], 
    header = TRUE
  )
  
  names(tmp)[which(names(tmp) == "Sample.Name")] <- "SWRC_ID"
  
  # remove standards and negative controls
  tmp <- subset(tmp, Task == "UNKNOWN")
  tmp <- subset(tmp, SWRC_ID != "NEG")
  
  # remove "Omit" samples
  tmp <- subset(tmp, Omit == FALSE)
  
  # convert items to numeric
  tmp$SWRC_ID <- as.numeric(tmp$SWRC_ID)
  tmp$SWRC_ID <- trunc(tmp$SWRC_ID)
  
  # only retain columns we are interested in
  tmp <- subset(
    tmp, select = c(SWRC_ID, Quantity)
  )
  
  tmp$Target <- targetvec[i]
  
  tmp$Year <- yearvec[i]
  
  qpcrls[[i]] <- tmp 
  
  rm(tmp)
}

str(qpcrls[[1]])

# stitch the results together
qpcr <- plyr::ldply(qpcrls, rbind, .id = NULL)
str(qpcr)

ls()
rm(i)
rm(qpcrls)
rm(fls_qpcr)
rm(targetvec)
rm(yearvec)

# qPCR: cleanup, format, calculate values ---------------------------------
rxnvol_uL <- 20
templatevol_uL <- 2
elutionvol_uL <- 100
samplemass_g <- 0.25
qpcr$gene_copies_per_g_soil <- qpcr$Quantity*(1/rxnvol_uL)*(rxnvol_uL/templatevol_uL)*(elutionvol_uL/samplemass_g)
rm(rxnvol_uL)
rm(templatevol_uL)
rm(elutionvol_uL)
rm(samplemass_g)

# append Date and Site to data
qpcr$Date <- sampledata$Date[match(qpcr$SWRC_ID, sampledata$SWRC_ID)]
qpcr$Site <- sampledata$Site[match(qpcr$SWRC_ID, sampledata$SWRC_ID)]

# take the mean of each sample's (triplicate) analytical replicates
str(qpcr)
qpcr <- qpcr %>% group_by(Target, Date, Site) %>% dplyr::summarise(
  copy_number = mean(gene_copies_per_g_soil)
) %>% ungroup()
str(qpcr)

# qPCR: append ------------------------------------------------------------
str(dat)
str(qpcr)

qpcr_wide <- qpcr %>% tidyr::pivot_wider(
  names_from = Target, 
  values_from = copy_number
)
str(qpcr_wide)

dat <- full_join(
  x = dat, 
  y = qpcr_wide,
  by = c("Date", "Site")
)
str(dat)

# Soil --------------------------------------------------------------------
soil <- read.csv(
  file = file.path(getwd(), "data", "Soil_Nit_Denit.csv"), 
  header = TRUE
)

# Soil: cleanup, format, calculate values ---------------------------------
str(soil)

soil$Date <- as.Date(soil$Date)

# remove "SP_" from site names
soil$Site <- sapply(stringr::str_split(soil$Site, pattern = "SP_"), "[[", 2)

# remove sub-plot labels
table(soil$Site)
soil$Site[which(substr(soil$Site, nchar(soil$Site), nchar(soil$Site)) %in% c("A", "B", "C", "D"))] <-
  substr(
    soil$Site[which(substr(soil$Site, nchar(soil$Site), nchar(soil$Site)) %in% c("A", "B", "C", "D"))],
    1,
    nchar(soil$Site[which(substr(soil$Site, nchar(soil$Site), nchar(soil$Site)) %in% c("A", "B", "C", "D"))])-1
  )
table(soil$Site)

str(soil)

# Take the mean of each of the analytical replicates
soil <- soil %>% group_by(Date, Site, parameter) %>% summarize(
  mean_value = mean(value, na.rm = TRUE)
) %>% ungroup()
str(soil)

# Soil: append ------------------------------------------------------------
str(dat)
str(soil)

# get soil into wide format
soil_wide <- soil %>% tidyr::pivot_wider(
  names_from = parameter,
  values_from = mean_value
)
str(soil_wide)

dat <- full_join(
  x = dat, 
  y = soil_wide, 
  by = c("Site", "Date")
)
str(dat)

# EEA ---------------------------------------------------------------------
eea <- read.csv(
  file = file.path(getwd(), "data", "EEA.csv"), 
  header = TRUE
)

# EEA: cleanup, format, calculate values ----------------------------------
str(eea)

eea$SWRC_ID <- as.numeric(eea$SWRC_ID)

# remove "SP_" from site names
eea$Site <- sapply(stringr::str_split(eea$Site, pattern = "SP_"), "[[", 2)

# remove sub-plot labels
table(eea$Site)
eea$Site[which(substr(eea$Site, nchar(eea$Site), nchar(eea$Site)) %in% c("A", "B", "C", "D"))] <-
  substr(
    eea$Site[which(substr(eea$Site, nchar(eea$Site), nchar(eea$Site)) %in% c("A", "B", "C", "D"))],
    1,
    nchar(eea$Site[which(substr(eea$Site, nchar(eea$Site), nchar(eea$Site)) %in% c("A", "B", "C", "D"))])-1
  )
table(eea$Site)

# add date to eea
eea$Date <- sampledata$Date[match(eea$SWRC_ID, sampledata$SWRC_ID)]

# calculate mean for analytical replicates
eea <- eea %>% group_by(Substrate, Site, Date) %>% dplyr::summarise(
  OM_percent = mean(OM_percent), 
  Moisture_percent = mean(Moisture_percent), 
  EEA_umol_hr_gdrysoil = mean(EEA_umol_hr_gdrysoil), 
  EEA_umol_hr_gOM = mean(EEA_umol_hr_gOM)
) %>% ungroup()

str(eea)

# EEA: append -------------------------------------------------------------
str(dat)
str(eea)

# get soil into wide format
eea_wide <- eea %>% tidyr::pivot_wider(
  names_from = Substrate,
  values_from = c(EEA_umol_hr_gdrysoil, EEA_umol_hr_gOM)
)
str(eea_wide)

dat <- full_join(
  x = dat,
  y = eea_wide, 
  by = c("Site", "Date")
)

# save to disk ------------------------------------------------------------
# as .csv
# write.csv(
#   x = dat,
#   file = file.path(getwd(), "data", "dat.csv"),
#   row.names = FALSE
# )
# as RDS
# saveRDS(
#   object = dat,
#   file = file.path(getwd(), "/data/", "dat.RDS")
# )
