library(readr)
library(readxl)
library(dplyr)

typ <- read_csv(input_files[[1]], trim_ws=TRUE)
print(typ$name)

# Pretend to read samp (check that names match)
samp <- read_excel(input_files[[2]])

names(samp) <- gsub(" ", "_", names(samp))
ssamp <- (samp %>% transmute(
	uid = Uid
	, Sexprt_Num = Sexprt_Num
	, Sexprt_Cat = Sexprt_Cat
))
head(ssamp)

# stopifnot(identical(typ$name, names(samp)))

# Really read samp, using types from table
samp <- read_excel(input_files[[2]], col_types=typ$type)

# Eliminate spaces from variable names (old style)
names(samp) <- gsub(" ", "_", names(samp))

samp <- (samp %>% transmute(
	uid = Uid
	, Sexprt_Num = Sexprt_Num
	, Sexprt_Cat = Sexprt_Cat
))
head(samp)
