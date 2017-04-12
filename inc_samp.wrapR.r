# This file was generated automatically by wrapR.pl
# You probably don't want to edit it


input_files <- c("sample.csv", "SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx")
rtargetname <- "inc_samp"
pdfname <- ".inc_samp.Rout.pdf"
csvname <- "inc_samp.Rout.csv"
rdsname <- "inc_samp.Rds"
pdf(pdfname)
# End RR preface

# Generated using wrapR file inc_samp.wrapR.r
source('readsamp.R', echo=TRUE)
# Wrapped output file inc_samp.wrapR.rout
# Begin RR postscript
warnings()
proc.time()

# If you see this in your terminal, the R script inc_samp.wrapR.r (or something it called) did not close properly
save.image(file=".inc_samp.RData")

