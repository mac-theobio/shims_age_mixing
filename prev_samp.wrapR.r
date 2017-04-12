# This file was generated automatically by wrapR.pl
# You probably don't want to edit it


input_files <- c("prev_samp.csv", "SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx")
rtargetname <- "prev_samp"
pdfname <- ".prev_samp.Rout.pdf"
csvname <- "prev_samp.Rout.csv"
rdsname <- "prev_samp.Rds"
pdf(pdfname)
# End RR preface

# Generated using wrapR file prev_samp.wrapR.r
source('readsamp.R', echo=TRUE)
# Wrapped output file prev_samp.wrapR.rout
# Begin RR postscript
warnings()
proc.time()

# If you see this in your terminal, the R script prev_samp.wrapR.r (or something it called) did not close properly
save.image(file=".prev_samp.RData")

