# This file was generated automatically by wrapR.pl
# You probably don't want to edit it


input_files <- c("SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx")
rtargetname <- "testread"
pdfname <- ".testread.Rout.pdf"
csvname <- "testread.Rout.csv"
rdsname <- "testread.Rds"
pdf(pdfname)
# End RR preface

# Generated using wrapR file testread.wrapR.r
source('testread.R', echo=TRUE)
# Wrapped output file testread.wrapR.rout
# Begin RR postscript
warnings()
proc.time()

# If you see this in your terminal, the R script testread.wrapR.r (or something it called) did not close properly
save.image(file=".testread.RData")

