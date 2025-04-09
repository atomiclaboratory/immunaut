# data-raw/DATASET.R

# Load necessary library
library(data.table)

# 1) Load the originalData.csv into immunautDemo
immunautDemo <- fread(
    "./data-raw/originalData.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE,
    data.table = FALSE
)

# 2) Save immunautDemo as an .rda in your package data/ directory
usethis::use_data(immunautDemo, overwrite = TRUE)

# 3) Load the LAIV_Integrated_Dataset.csv into immunautLAIV
immunautLAIV <- fread(
    "./data-raw/LAIV_Integrated_Dataset.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE,
    data.table = FALSE
)

# 4) Save immunautLAIV as an .rda in your package data/ directory
usethis::use_data(immunautLAIV, overwrite = TRUE)
