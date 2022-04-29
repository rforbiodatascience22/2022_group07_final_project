# Install libraries -------------------------------------------------------
if (!"ggthemes" %in% installed.packages()) install.packages("ggthemes")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"mltools" %in% installed.packages()) install.packages("mltools")

# Load libraries ----------------------------------------------------------
library("ggthemes")
library("tidyverse")
library("mltools")

# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis_i.R")