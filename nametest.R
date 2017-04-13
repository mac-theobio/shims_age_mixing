library(dplyr)
library(readr)

samp <- read_csv(input_files[[1]])

samp <- (samp
	%>% mutate(num = as.numeric(num))
)

print(samp)

dir <- read_csv(input_files[[1]], col_types = cols(
	"c", "d"
))

print(dir)
