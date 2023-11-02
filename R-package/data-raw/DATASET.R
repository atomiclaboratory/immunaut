# Load the necessary library
library(data.table)

# Read the CSV data file using fread function from data.table package
# The .csv file is assumed to be in the "./data-raw/" directory of the current working directory
# The argument stringsAsFactors is set to FALSE to ensure character data does not convert to factors
# The argument data.table is set to FALSE to load the data as a data frame
immunautDemo <- fread("./data-raw/originalData.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE, data.table = FALSE)

# Save the data object as an .rda file within the package using usethis::use_data()
# The argument overwrite is set to TRUE to overwrite the existing .rda file, if it exists
usethis::use_data(immunautDemo, overwrite = TRUE)
