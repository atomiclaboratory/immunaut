# Install necessary packages
install.packages('Rcpp', repos='http://cran.us.r-project.org') # Install Rcpp from CRAN

# Install roxygen from GitHub and load requisite libraries
devtools::install_github("klutometis/roxygen")
library(roxygen2)
library(knitr)
library(Rcpp)

# Set the working directory to the location of your R package
setwd("/mnt/data/projects/atomic_laboratory/immunaut_r_package/R-package/")

# Load all objects, prepare package for lazy loading, build binary package, check package
devtools::load_all()
devtools::document()
devtools::build()
devtools::check()

# Uncomment the following if 'data-raw' directory and 'README.Rmd' file need to be created
# usethis::use_data_raw()
# devtools::use_readme_rmd()

# Uncomment following line if you need to regenerate data from the CSV file
# source("data-raw/DATASET.R")

# Process roxygen2 comments and convert them to .Rd format
roxygen2::roxygenise()

# Release the package, render README, install the package
devtools::release()
rmarkdown::render("README.Rmd")
devtools::install()

# Load the package, access the generated data, print the first few lines of the dataset
library("immunaut")

data <- read.csv("data-raw/testing_data.csv")

settings <- list()
settings$fileHeader <- immunaut::generate_file_header(data)

colnames(data) <- settings$fileHeader$remapped

settings$selectedColumns <- file_header$remapped[grep(".*V21.*", file_header$original, ignore.case = TRUE)]
settings$preProcessDataset <- c("medianImpute", "center", "scale", "zv", "nzv")
settings$clusterType <- "Hierarchical"
           
devtools::load_all()
data_cluster <- immunaut(data, settings)

settings$fileHeader <- rbind(settings$fileHeader, data.frame(original = "cluster", remapped = "cluster"))
# Create a named vector for mapping
mapping <- setNames(settings$fileHeader$original, settings$fileHeader$remapped)

# Use the mapping to replace the column names
colnames(data_cluster) <- mapping[colnames(data_cluster)]







