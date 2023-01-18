# Load required packages


# Define arguments for function
mlra <- "035X" # Set the MLRA of interest
# Do you want to subset the MLRA by a state? If yes, set stateset = TRUE. If no, stateset = FALSE.
stateset = TRUE
# If statset = TRUE, specify state
state <- "NM"


# Extend timeout
options(timeout = 4000000)


# Run SiteKey function
SiteKey <- SiteKeyBuild(mlra = "035X", stateset = TRUE, state = "NM")
# Save to csv if desired
write.csv(SiteKey, paste("SiteKeyOutput_MLRA", mlra, ".csv", sep = ""), row.names = FALSE)


