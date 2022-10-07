# Prepare Environment -----------------------------------------------------
library(dplyr)

dat_orig <- dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# Collect sampling statistics ---------------------------------------------
str(dat)

dat_long <- dat %>% tidyr::pivot_longer(
  cols = names(dat)[7:21], 
  names_to = "Parameter", 
  values_to = "value"
)

# remove unneeded info for simplicity in the final table
dat_long <- subset(
  dat_long, 
  select = -c(Management_System, Tillage, Cover_Crop, Treatment_Group)
)

# identify which parameters were collected
dat_long$ObsExists <- !is.na(dat_long$value)

# summarize the sample counts
samples_summary <- dat_long %>% group_by(Date, Parameter) %>%  summarise(
  samples = sum(ObsExists)
) %>% tidyr::pivot_wider(
  names_from = Parameter, 
  values_from = samples
)

# condense info from each dataset into a single variable for clarity. 
samples_summary$Nit_Denit_Samples <- 
  ifelse(samples_summary$netMIN > 0, samples_summary$netMIN, 0)
samples_summary$EEA_Samples <- 
  ifelse(samples_summary$GLU_gOM > 0, samples_summary$GLU_gOM, 0)
samples_summary$qPCR_Samples <- 
  ifelse(samples_summary$AOA >0, samples_summary$AOA, 0)

# save to disk ------------------------------------------------------------
write.csv(
  x = samples_summary, 
  file = file.path(getwd(), "output", "Sampling_Table.csv"),
  row.names = FALSE
)
