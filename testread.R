library(readxl)
# Read the data 
samp <- read_excel(input_files[[1]])
names(samp)
head(samp)

write.csv(names(samp), csvname, quote=FALSE, row.names=FALSE)


