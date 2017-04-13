library(readr)
library(readxl)

# Pretend to read samp (check that names match)
samp <- read_excel(input_files[[1]])

# Eliminate spaces from variable names (old style)
names(samp) <- gsub(" ", "_", names(samp))

