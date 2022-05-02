# Install libraries -------------------------------------------------------
if (!"ggthemes" %in% installed.packages()) install.packages("ggthemes")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"mltools" %in% installed.packages()) install.packages("mltools")
if (!"broom" %in% installed.packages()) install.packages("broom")
if (!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")


# Load libraries ----------------------------------------------------------
library("ggthemes")
library("tidyverse")
library("mltools")
library("broom")
library("RColorBrewer")

# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis_i.R")