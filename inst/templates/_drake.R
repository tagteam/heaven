# This file needs to be run everytime the R session is restarted
source("./packages.R")

# Load all function files
sapply(list.files("./R", full.names = TRUE), source)

# _drake.R must end with a call to drake_config().
drake_config(plan)