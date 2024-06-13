

# Code to re-run analyses associated with 
# Freshwater navigation canals support invasive species across spatial scales
# Sexton et al. 2024


# Sections
# 1. Bootstrapped models of invasive richness and occurrences at all spatial scales
# 1a. Fish
# 1b. Macroinvertebrates
# 2. Functional group models (not bootstrapped)
# 2a. Fish
# 2b. Macroinvertebrates



##  1a. Fish ----
library(ggplot2)
library(glmmTMB)

rm(list = ls())



# Create a bootstrapping function for richness
lmer_boot_SR <- function(data, size) {
  
  if (size > nrow(data)) {
    stop("Argument must be < number of rows of data")
  }
  
  sampled_rows <- sample(1:nrow(data), size, replace = FALSE) 
  
  data <- data[sampled_rows, ]
  
  data <- dplyr::mutate(data,
                        Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                        Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                        Ports_scaled        = as.numeric(scale(Ports)),
                        Locks_scaled        = as.numeric(scale(Locks)),
                        Temperature_scaled  = as.numeric(scale(Temperature)),
                        Elevation_scaled    = as.numeric(scale(Elevation)),
                        Discharge_scaled    = as.numeric(scale(Discharge)))
  
  mod <- glmmTMB(SR_rrfy ~ 
                   Ships_Avg_scaled + Canal_Binary + 
                   Ports_scaled + Locks_scaled +
                   Temperature_scaled + 
                   Elevation_scaled +
                   Discharge_scaled, 
                 data = data, family = "tweedie")
  
  
  estimates <- coefficients(summary(mod))
  estimates <- as.data.frame(estimates$cond[,1])
  estimates <- as.data.frame(t(estimates))
  
}



# Now the same with Occasions
lmer_boot_Occ <- function(data, size) {
  
  if (size > nrow(data)) {
    stop("Argument must be < number of rows of data")
  }
  
  sampled_rows <- sample(1:nrow(data), size, replace = FALSE) 
  
  data <- data[sampled_rows, ]
  
  data <- dplyr::mutate(data,
                        Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                        Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                        Ports_scaled        = as.numeric(scale(Ports)),
                        Locks_scaled        = as.numeric(scale(Locks)),
                        Temperature_scaled  = as.numeric(scale(Temperature)),
                        Elevation_scaled    = as.numeric(scale(Elevation)),
                        Discharge_scaled    = as.numeric(scale(Discharge)))
  
  mod <- glmmTMB(Occ_rrfy ~ 
                   Ships_Avg_scaled + Canal_Binary + 
                   Ports_scaled + Locks_scaled +
                   Temperature_scaled + 
                   Elevation_scaled +
                   Discharge_scaled, 
                 data = data, family = "tweedie")
  
  estimates <- coefficients(summary(mod))
  estimates <- as.data.frame(estimates$cond[,1])
  estimates <- as.data.frame(t(estimates))
  
}








###   Catchment Level 6 ----


# Read in biodiversity file
cat6 <- read.csv("../GBIF repo/cat6_fish.csv")
str(cat6)

# Trim df to only complete cases - drop rows with NAs
cat6 <- cat6[complete.cases(cat6), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat6, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_6 <- data.frame("variable" = colnames(estimates),
                            "boot_mean"    = apply(estimates, 2, mean), 
                            "boot_sd"      = apply(estimates, 2, sd),
                            "metric"        = "Richness",
                            "Catchment"     = "6")

# # Now ggplot it
# ggplot(data = boot_est_SR_6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat6, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs6 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurences",
                             "Catchment"  = "6")

# Now ggplot it
# ggplot(data = boot_est_occs6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


# Join them into one df
boot_est_combo_6 <- dplyr::full_join(boot_est_SR_6, boot_est_occs6)

# Plot it
# ggplot(data = boot_est_combo_6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       color = metric, shape = metric)) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)





read.csv("../GBIF repo/")


###   Catchment Level 7 ----

# Read in biodiversity file
cat7 <- read.csv("../GBIF repo/cat7_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat7 <- cat7[complete.cases(cat7), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat7, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_7 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "7")

# Now ggplot it
# ggplot(data = boot_est_SR_7, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat7, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs7 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "7")

# Now ggplot it
# ggplot(data = boot_est_occs7, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


# Join them into one df
boot_est_combo_7 <- dplyr::full_join(boot_est_SR_7, boot_est_occs7)

# Plot it
# ggplot(data = boot_est_combo_7, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       color = metric, shape = metric)) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_6, boot_est_SR_7)

# Now one with just the Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_occs6, boot_est_occs7)


# Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)

# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)






###   Catchment Level 8 ----

# Read in biodiversity file
cat8 <- read.csv("../GBIF repo/cat8_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat8 <- cat8[complete.cases(cat8), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat8, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_8 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "8")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat8, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs8 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "8")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_8)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs8)

# Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) +
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd),
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment),
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) +
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd),
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment),
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)







###   Catchment Level 9 ----

# Read in biodiversity file
cat9 <- read.csv("../GBIF repo/cat9_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat9 <- cat9[complete.cases(cat9), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat9, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_9 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "9")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat9, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs9 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "9")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_9)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs9)

# Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)






###   Catchment Level 10 ----

# Read in biodiversity file
cat10 <- read.csv("../GBIF repo/cat10_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat10 <- cat10[complete.cases(cat10), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat10, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_10 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "10")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat10, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs10 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "10")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_10)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs10)

# Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 






###   Catchment Level 11 ----

# Read in biodiversity file
cat11 <- read.csv("../GBIF repo/cat11_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat11 <- cat11[complete.cases(cat11), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat11, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_11 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "11")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat11, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs11 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "11")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_11)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs11)

# Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)








###   Catchment Level 12 ----

# Read in biodiversity file
cat12 <- read.csv("../GBIF repo/cat12_fish.csv")

# Trim df to only complete cases - drop rows with NAs
cat12 <- cat12[complete.cases(cat12), ]

# Now run it 999 times with just 700 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat12, size = 700))
}
beepr::beep()

# New estimate df for just the averaged values
boot_est_SR_12 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "12")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat12, size = 700))
}
beepr::beep()


# New estimate df for just the averaged values
boot_est_occs12 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "12")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_12)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs12)


### Plotting ----

# Plot it
ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)

ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)


# Reorder catchment column so that it goes in the order I want
str(boot_est_Occ_combo)
boot_est_Occ_combo <- dplyr::mutate(boot_est_Occ_combo,
                                    Catchment = forcats::fct_relevel(Catchment,
                                                                     "6", "7", "8",
                                                                     "9", "10", "11", "12"))

boot_est_SR_combo <- dplyr::mutate(boot_est_SR_combo,
                                   Catchment = forcats::fct_relevel(Catchment,
                                                                    "6", "7", "8",
                                                                    "9", "10", "11", "12"))



# Reorder variable column so that it goes in the order I want
str(boot_est_Occ_combo)
boot_est_Occ_combo <- dplyr::mutate(boot_est_Occ_combo,
                                    variable = forcats::fct_relevel(variable,
                                                                    "Ships_Avg_scaled",
                                                                    "Canal_Binary",
                                                                    "Ports_scaled",
                                                                    "Locks_scaled",
                                                                    "Temperature_scaled",
                                                                    "Elevation_scaled",
                                                                    "Discharge_scaled"))

boot_est_SR_combo <- dplyr::mutate(boot_est_SR_combo,
                                   variable = forcats::fct_relevel(variable,
                                                                   "Ships_Avg_scaled",
                                                                   "Canal_Binary",
                                                                   "Ports_scaled",
                                                                   "Locks_scaled",
                                                                   "Temperature_scaled",
                                                                   "Elevation_scaled",
                                                                   "Discharge_scaled"))





occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)

sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)


ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")




# Drop the Intercept rows
boot_est_SR_combo  <- dplyr::filter(boot_est_SR_combo, variable != "(Intercept)")
boot_est_Occ_combo <- dplyr::filter(boot_est_Occ_combo, variable != "(Intercept)")



occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0) +
  ggtitle("Occurrences") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge"))


sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)+
  ggtitle("Richness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge"))


ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")


# Add in a phylopic 
library(rphylopic)
salmo <- pick_phylopic(name = "Salmo trutta", n = 1)


occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0) +
  ggtitle("Occurrences") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge")) +
  add_phylopic(img = salmo, x = 6.5, y = 1.4, ysize = 0.15)
occs_p

sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)+
  ggtitle("Richness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge")) +
  add_phylopic(img = salmo, x = 6.8, y = 0.75, ysize = 0.06)
sr_p


ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")









# 1b. Macroinvertebrates ----


# Create a function for richness
lmer_boot_SR <- function(data, size) {
  
  if (size > nrow(data)) {
    stop("Argument must be < number of rows of data")
  }
  
  sampled_rows <- sample(1:nrow(data), size, replace = FALSE) 
  
  data <- data[sampled_rows, ]
  
  data <- dplyr::mutate(data,
                        Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                        Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                        Ports_scaled        = as.numeric(scale(Ports)),
                        Locks_scaled        = as.numeric(scale(Locks)),
                        Temperature_scaled  = as.numeric(scale(Temperature)),
                        Elevation_scaled    = as.numeric(scale(Elevation)),
                        Discharge_scaled    = as.numeric(scale(Discharge)))
  
  mod <- glmmTMB(SR_rrfy ~ 
                   Ships_Avg_scaled + Canal_Binary + 
                   Ports_scaled + Locks_scaled +
                   Temperature_scaled + 
                   Elevation_scaled +
                   Discharge_scaled, 
                 data = data, family = "tweedie")
  
  
  estimates <- coefficients(summary(mod))
  estimates <- as.data.frame(estimates$cond[,1])
  estimates <- as.data.frame(t(estimates))
  
  
}



# Now the same for Occasions
lmer_boot_Occ <- function(data, size) {
  
  if (size > nrow(data)) {
    stop("Argument must be < number of rows of data")
  }
  
  sampled_rows <- sample(1:nrow(data), size, replace = FALSE) 
  
  data <- data[sampled_rows, ]
  
  data <- dplyr::mutate(data,
                        Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                        Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                        Ports_scaled        = as.numeric(scale(Ports)),
                        Locks_scaled        = as.numeric(scale(Locks)),
                        Temperature_scaled  = as.numeric(scale(Temperature)),
                        Elevation_scaled    = as.numeric(scale(Elevation)),
                        Discharge_scaled    = as.numeric(scale(Discharge)))
  
  mod <- glmmTMB(Occ_rrfy ~ 
                   Ships_Avg_scaled + Canal_Binary + 
                   Ports_scaled + Locks_scaled +
                   Temperature_scaled + 
                   Elevation_scaled +
                   Discharge_scaled, 
                 data = data, family = "tweedie")
  
  estimates <- coefficients(summary(mod))
  estimates <- as.data.frame(estimates$cond[,1])
  estimates <- as.data.frame(t(estimates))
  
}





###   Catchment Level 6 ----

# Read in biodiversity file
cat6 <- read.csv("../GBIF repo/cat6_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat6 <- cat6[complete.cases(cat6), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat6, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_6 <- data.frame("variable" = colnames(estimates),
                            "boot_mean"    = apply(estimates, 2, mean), 
                            "boot_sd"      = apply(estimates, 2, sd),
                            "metric"        = "Richness",
                            "Catchment"     = "6")

# # Now ggplot it
# ggplot(data = boot_est_SR_6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat6, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs6 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurences",
                             "Catchment"  = "6")

# # Now ggplot it
# ggplot(data = boot_est_occs6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


# Join them into one df
boot_est_combo_6 <- dplyr::full_join(boot_est_SR_6, boot_est_occs6)

# # Plot it
# ggplot(data = boot_est_combo_6, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       color = metric, shape = metric)) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)






###   Catchment Level 7 ----

# Read in biodiversity file
cat7 <- read.csv("../GBIF repo/cat7_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat7 <- cat7[complete.cases(cat7), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat7, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_7 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "7")

# # Now ggplot it
# ggplot(data = boot_est_SR_7, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat7, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs7 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "7")

# # Now ggplot it
# ggplot(data = boot_est_occs7, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd))) + 
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = 0)


# Join them into one df
boot_est_combo_7 <- dplyr::full_join(boot_est_SR_7, boot_est_occs7)

# Plot it
ggplot(data = boot_est_combo_7, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      color = metric, shape = metric)) + 
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0)


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_6, boot_est_SR_7)

# Now one with just the Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_occs6, boot_est_occs7)


# # Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)






###   Catchment Level 8 ----

# Read in biodiversity file
cat8 <- read.csv("../GBIF repo/cat8_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat8 <- cat8[complete.cases(cat8), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat8, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_8 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "8")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat8, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs8 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "8")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_8)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs8)

# # Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)







###   Catchment Level 9 ----

# Read in biodiversity file
cat9 <- read.csv("../GBIF repo/cat9_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat9 <- cat9[complete.cases(cat9), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat9, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_9 <- data.frame("variable"    = colnames(estimates),
                            "boot_mean"  = apply(estimates, 2, mean), 
                            "boot_sd"    = apply(estimates, 2, sd),
                            "metric"      = "Richness",
                            "Catchment"   = "9")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat9, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs9 <- data.frame("variable"  = colnames(estimates),
                             "boot_mean" = apply(estimates, 2, mean), 
                             "boot_sd"   = apply(estimates, 2, sd),
                             "metric"     = "Occurrences",
                             "Catchment"  = "9")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_9)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs9)

# # Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)






###   Catchment Level 10 ----

# Read in biodiversity file
cat10 <- read.csv("../GBIF repo/cat10_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat10 <- cat10[complete.cases(cat10), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat10, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_10 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "10")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat10, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs10 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "10")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_10)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs10)

# # Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)





###   Catchment Level 11 ----

# Read in biodiversity file
cat11 <- read.csv("../GBIF repo/cat11_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat11 <- cat11[complete.cases(cat11), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat11, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_11 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "11")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat11, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs11 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "11")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_11)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs11)

# # Plot it
# ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)
# 
# ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
#   geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
#                       ymax = (boot_mean + boot_sd),
#                       fill = Catchment, color = Catchment), 
#                   position = position_dodge(width = 1), size=1.5) +
#   geom_hline(yintercept = 0)








###   Catchment Level 12 ----

# Read in biodiversity file
cat12 <- read.csv("../GBIF repo/cat12_invert.csv")

# Trim df to only complete cases - drop rows with NAs
cat12 <- cat12[complete.cases(cat12), ]

# Now run it 999 times with just 650 rows
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_SR(cat12, size = 650))
}

# New estimate df for just the averaged values
boot_est_SR_12 <- data.frame("variable"    = colnames(estimates),
                             "boot_mean"  = apply(estimates, 2, mean), 
                             "boot_sd"    = apply(estimates, 2, sd),
                             "metric"      = "Richness",
                             "Catchment"   = "12")

## Occasions
estimates <- data.frame()
for (i in 1:999) {
  estimates <- rbind(estimates, lmer_boot_Occ(cat12, size = 650))
}


# New estimate df for just the averaged values
boot_est_occs12 <- data.frame("variable"  = colnames(estimates),
                              "boot_mean" = apply(estimates, 2, mean), 
                              "boot_sd"   = apply(estimates, 2, sd),
                              "metric"     = "Occurrences",
                              "Catchment"  = "12")


# Create a df with just the occurrences and one with just the SRs
boot_est_SR_combo <- dplyr::full_join(boot_est_SR_combo, boot_est_SR_12)

# and Occs
boot_est_Occ_combo <- dplyr::full_join(boot_est_Occ_combo, boot_est_occs12)


### Plotting ----
# Plot it
ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)

ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)


# Reorder catchment column so that it goes in the order I want
str(boot_est_Occ_combo)
boot_est_Occ_combo <- dplyr::mutate(boot_est_Occ_combo,
                                    Catchment = forcats::fct_relevel(Catchment,
                                                                     "6", "7", "8",
                                                                     "9", "10", "11", "12"))

boot_est_SR_combo <- dplyr::mutate(boot_est_SR_combo,
                                   Catchment = forcats::fct_relevel(Catchment,
                                                                    "6", "7", "8",
                                                                    "9", "10", "11", "12"))



# Reorder variable column so that it goes in the order I want
str(boot_est_Occ_combo)
boot_est_Occ_combo <- dplyr::mutate(boot_est_Occ_combo,
                                    variable = forcats::fct_relevel(variable,
                                                                    "Ships_Avg_scaled",
                                                                    "Canal_Binary",
                                                                    "Ports_scaled",
                                                                    "Locks_scaled",
                                                                    "Temperature_scaled",
                                                                    "Elevation_scaled",
                                                                    "Discharge_scaled"))

boot_est_SR_combo <- dplyr::mutate(boot_est_SR_combo,
                                   variable = forcats::fct_relevel(variable,
                                                                   "Ships_Avg_scaled",
                                                                   "Canal_Binary",
                                                                   "Ports_scaled",
                                                                   "Locks_scaled",
                                                                   "Temperature_scaled",
                                                                   "Elevation_scaled",
                                                                   "Discharge_scaled"))





occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)

sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)


ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")


# write.csv(boot_est_SR_combo, "../GBIF repo/boot_est_SR_combo.csv")
# write.csv(boot_est_Occ_combo, "../GBIF repo/boot_est_Occ_combo.csv")


# Drop the Intercept rows
boot_est_SR_combo  <- dplyr::filter(boot_est_SR_combo, variable != "(Intercept)")
boot_est_Occ_combo <- dplyr::filter(boot_est_Occ_combo, variable != "(Intercept)")



occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0) +
  ggtitle("Occurrences") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge"))


sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)+
  ggtitle("Richness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7))) +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge"))


ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")

# Add phylopic
library(rphylopic)
mim_pic <- pick_phylopic(name = "Anax imperator", n = 2)

occs_p <- ggplot(data = boot_est_Occ_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0) +
  ggtitle("Occurrences") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) +  
  theme(legend.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7)),
                     labels = c("6 (Large Grain)", "7", "8", "9", "10", "11", "12 (Fine Grain)"),
                     name = "Sub-Catchment Level") +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge")) +
  add_phylopic(img = mim_pic, x = 6.7, y = 1.5, ysize = 0.3) +
  guides(fill = "none")
occs_p

sr_p <- ggplot(data = boot_est_SR_combo, aes(x = variable, y = boot_mean)) + 
  geom_pointrange(aes(ymin = (boot_mean - boot_sd), 
                      ymax = (boot_mean + boot_sd),
                      fill = Catchment, color = Catchment), 
                  position = position_dodge(width = 1), size=1.5) +
  geom_hline(yintercept = 0)+
  ggtitle("Richness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 28)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 16, angle = 15)) +
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  ylab("Estimate") +
  scale_color_manual(values = c(paletteer::paletteer_dynamic("cartography::green.pal", 7)),
                     labels = c("6 (Large Grain)", "7", "8", "9", "10", "11", "12 (Fine Grain)"),
                     name = "Sub-Catchment Level") +
  scale_x_discrete(labels=c("Ships_Avg_scaled" = "Shipping",
                            "Canal_Binary" = "Canals",
                            "Ports_scaled" = "Ports",
                            "Locks_scaled" = "Locks",
                            "Temperature_scaled" = "Temperature",
                            "Elevation_scaled" = "Elevation",
                            "Discharge_scaled" = "Discharge")) +
  add_phylopic(img = mim_pic, x = 6.9, y = 1, ysize = 0.17) +
  guides(fill = "none")
sr_p

ggpubr::ggarrange(sr_p, occs_p,
                  ncol = 2, nrow = 1,
                  common.legend = T,
                  legend = "right")


























# 2. Functional Models


# 2a. Fish Functional ----

library(tidyverse)
library(glmmTMB)
library(sjPlot)


# Read in occurrences
fish_flow_occ <- read.csv("../GBIF repo/cat12_fish_flows.csv")
str(fish_flow_occ)
fish_flow_occ <- dplyr::select(fish_flow_occ, -x)


# Read in richness
fish_flow_sr <- read.csv("../GBIF repo/cat12_fish_flows_sr.csv")
str(fish_flow_sr)

# Send it wide
fish_flow_sr_wide <- tidyr::spread(fish_flow_sr, flow, rich)
fish_flow_sr_wide <- dplyr::select(fish_flow_sr_wide, -V1)
fish_flow_sr_wide[is.na(fish_flow_sr_wide)] <- 0
str(fish_flow_sr_wide)
fish_flow_sr_wide <- dplyr::rename(fish_flow_sr_wide,
                                   eury_sr  = eury,
                                   limno_sr = limno,
                                   rheo_sr  = rheo)


# Join them
fish_flow <- dplyr::full_join(fish_flow_sr_wide, fish_flow_occ)

# Read in full model df
fish_12 <- read.csv("../GBIF repo/cat12_fish.csv")
fish_12 <- fish_12[complete.cases(fish_12), ]

# Join them
fish_flow <- dplyr::full_join(fish_12, fish_flow)
fish_flow[is.na(fish_flow)] <- 0



# Rarefy the values
str(fish_flow)
fish_flow <- mutate(fish_flow,
                    eury_sr = eury_sr / Occsqrtd,     
                    limno_sr = limno_sr / Occsqrtd,    
                    rheo_sr = rheo_sr / Occsqrtd,     
                    eury_occ = eury_occ / Occsqrtd,    
                    limno_occ = limno_occ / Occsqrtd,   
                    rheo_occ = rheo_occ / Occsqrtd
)
fish_flow <- filter(fish_flow, Occsqrtd != 0)  



# scale the predictors
fish_flow <- dplyr::mutate(fish_flow,
                           Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                           Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                           Ports_scaled        = as.numeric(scale(Ports)),
                           Locks_scaled        = as.numeric(scale(Locks)),
                           Temperature_scaled  = as.numeric(scale(Temperature)),
                           Elevation_scaled    = as.numeric(scale(Elevation)),
                           Discharge_scaled    = as.numeric(scale(Discharge)))




# Run the models
str(fish_flow)

# Richness
# Eury
eury_rich <- glmmTMB(eury_sr ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = fish_flow, family = "tweedie")
summary(eury_rich)
# Limno
limno_rich <- glmmTMB(limno_sr ~ 
                        Ships_Avg_scaled + Canal_Binary + 
                        Ports_scaled + Locks_scaled +
                        Temperature_scaled + 
                        Elevation_scaled +
                        Discharge_scaled, 
                      data = fish_flow, family = "tweedie")
summary(limno_rich)
# Eury
rheo_rich <- glmmTMB(rheo_sr ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = fish_flow, family = "tweedie")
summary(rheo_rich)


# Visualize

plot_model(eury_rich, show.values = T)
plot_model(limno_rich, show.values = T)
plot_model(rheo_rich, show.values = T)

plot_models(eury_rich, 
            limno_rich,
            rheo_rich, 
            show.values = T)

summary(eury_rich)




# Occurrences
# Eury
eury_occ <- glmmTMB(eury_occ ~ 
                      Ships_Avg_scaled + Canal_Binary + 
                      Ports_scaled + Locks_scaled +
                      Temperature_scaled + 
                      Elevation_scaled +
                      Discharge_scaled, 
                    data = fish_flow, family = "tweedie")
summary(eury_occ)
# Limno
limno_occ <- glmmTMB(limno_occ ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = fish_flow, family = "tweedie")
summary(limno_occ)
# Eury
rheo_occ <- glmmTMB(rheo_occ ~ 
                      Ships_Avg_scaled + Canal_Binary + 
                      Ports_scaled + Locks_scaled +
                      Temperature_scaled + 
                      Elevation_scaled +
                      Discharge_scaled, 
                    data = fish_flow, family = "tweedie")
summary(rheo_occ)


# Visualize
plot_model(eury_occ, show.values = T)
plot_model(limno_occ, show.values = T)
plot_model(rheo_occ, show.values = T)

plot_models(eury_occ, 
            limno_occ,
            rheo_occ, 
            show.values = T)









# 2b. Macroinvert Functional ----

library(tidyverse)
library(glmmTMB)
library(sjPlot)


# Read in taxa split values
cat12_invert_taxa <- read.csv("../GBIF repo/cat12_invert_taxasplit.csv")

# Trim to only Molluscs (MOL), Crustaceans (ARTHCRU), & Insect (ARTHINS) 
cat12_invert_taxa <- dplyr::select(cat12_invert_taxa, Catchment_ID, 
                                   sumSR, sumOcc,
                                   SR_MOLexotic,     Occ_MOLexotic,
                                   SR_ARTHCRUexotic, Occ_ARTHCRUexotic,
                                   SR_ARTHINSexotic, Occ_ARTHINSexotic)

# Add in predictor data
cat12_preds <- read.csv("../GBIF repo/cat12_invert.csv")
cat12_preds <- cat12_preds[complete.cases(cat12_preds), ]
cat12_preds <- dplyr::select(cat12_preds, -sumSR, -sumOcc,
                             -SR_rrfy, -Occ_rrfy, -NEXT_DOWN)
# Join them
cat12_preds$HYBAS_ID <- as.character(cat12_preds$HYBAS_ID)
cat12_invert_taxa$Catchment_ID <- as.character(cat12_invert_taxa$Catchment_ID)
cat12_invert_taxa <- dplyr::left_join(cat12_preds, cat12_invert_taxa,
                                      by = c("HYBAS_ID" = "Catchment_ID"))

# Rarefy the values
cat12_invert_taxa <- cat12_invert_taxa %>% 
  mutate(across(starts_with("SR_")) / Occsqrtd)
cat12_invert_taxa <- cat12_invert_taxa %>% 
  mutate(across(starts_with("Occ_")) / Occsqrtd)

# scale the predictors
cat12_invert_taxa <- dplyr::mutate(cat12_invert_taxa,
                                   Ships_Avg_scaled    = as.numeric(scale(Ships_Avg)),
                                   Canal_Length_scaled = as.numeric(scale(Canal_Length)),
                                   Ports_scaled        = as.numeric(scale(Ports)),
                                   Locks_scaled        = as.numeric(scale(Locks)),
                                   Temperature_scaled  = as.numeric(scale(Temperature)),
                                   Elevation_scaled    = as.numeric(scale(Elevation)),
                                   Discharge_scaled    = as.numeric(scale(Discharge)))


# Run the models

# Richness
mod12_mol <- glmmTMB(SR_MOLexotic ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = cat12_invert_taxa, family = "tweedie")
summary(mod12_mol)
mod12_mol$fit

mod12_cru <- glmmTMB(SR_ARTHCRUexotic ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = cat12_invert_taxa, family = "tweedie")
summary(mod12_cru)

mod12_ins <- glmmTMB(SR_ARTHINSexotic ~ 
                       Ships_Avg_scaled + Canal_Binary + 
                       Ports_scaled + Locks_scaled +
                       Temperature_scaled + 
                       Elevation_scaled +
                       Discharge_scaled, 
                     data = cat12_invert_taxa, family = "tweedie")
summary(mod12_ins)



# Occurrences
mod12_molOcc <- glmmTMB(Occ_MOLexotic ~ 
                          Ships_Avg_scaled + Canal_Binary + 
                          Ports_scaled + Locks_scaled +
                          Temperature_scaled + 
                          Elevation_scaled +
                          Discharge_scaled, 
                        data = cat12_invert_taxa, family = "tweedie")
summary(mod12_molOcc)
mod12_cruOcc <- glmmTMB(Occ_ARTHCRUexotic ~ 
                          Ships_Avg_scaled + Canal_Binary + 
                          Ports_scaled + Locks_scaled +
                          Temperature_scaled + 
                          Elevation_scaled +
                          Discharge_scaled, 
                        data = cat12_invert_taxa, family = "tweedie")
summary(mod12_cruOcc)
mod12_insOcc <- glmmTMB(Occ_ARTHINSexotic ~ 
                          Ships_Avg_scaled + Canal_Binary + 
                          Ports_scaled + Locks_scaled +
                          Temperature_scaled + 
                          Elevation_scaled +
                          Discharge_scaled, 
                        data = cat12_invert_taxa, family = "tweedie")
summary(mod12_insOcc)


# Visualize
sjPlot::plot_model(mod12_molOcc, show.values = T)
sjPlot::plot_model(mod12_cruOcc, show.values = T)
sjPlot::plot_model(mod12_insOcc, show.values = T)

sjPlot::plot_models(mod12_mol,    mod12_cru,    mod12_ins,
                    mod12_molOcc, mod12_cruOcc, mod12_insOcc, 
                    show.values = T)

sjPlot::plot_models(mod12_molOcc, mod12_cruOcc, mod12_insOcc, 
                    show.values = T)






































