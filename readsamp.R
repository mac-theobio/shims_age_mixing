library(readr)
library(readxl)

typ <- read_csv(input_files[[1]], trim_ws=TRUE)

# Pretend to read samp (check that names match)
samp <- read_excel(input_files[[2]])
stopifnot(identical(typ$name, names(samp)))

# Really read samp, using types from table
samp <- read_excel(input_files[[2]], col_types=typ$type)

