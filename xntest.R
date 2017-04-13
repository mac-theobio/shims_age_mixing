library(dplyr)
library(readxl)

samp <- read_excel(input_files[[1]])
print(samp)

samp <- (samp
	%>% mutate(num = as.numeric(num))
)

print(samp)

dir <- read_excel(input_files[[1]], col_types = c("text", "numeric"))

print(dir)
