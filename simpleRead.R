library(dplyr)
library(readxl)

samp <- read_excel(input_files[[1]])

# Eliminate spaces from variable names (old style)
names(samp) <- make.names(names(samp))
names(samp) <- gsub("[.]", "_", names(samp))

# Note: there are variables MISSING from this list (in addition to those commented out)

samp <- transmute(samp
	, Uid=Uid
	# , Uhid=Uhid
	, SHIMS_EA=SHIMS_EA
	, Region=Region
	, Urban=Urban
	, REQ_Erdt=REQ_Erdt
	, Gender=Gender
	, Age=as.numeric(Age)
	# , Age_Grp=Age_Grp
	, Education=Education
	, Marital=Marital
	, Sex_Age=as.numeric(Sex_Age)
	, Sexprt_Num=as.numeric(Sexprt_Num)
	# , Sexprt_Cat=Sexprt_Cat
	, Preg=Preg
	, Circum=Circum
	, Hiv_Testhistory=Hiv_Testhistory
	, Hivstatus_T1=Hivstatus_T1
	, Aware_Hiv_Art=Aware_Hiv_Art
	# , Wt_Poststratification=as.numeric(Wt_Poststratification)
	# , Wt_Design=as.numeric(Wt_Design)
	# , Wt_Population=as.numeric(Wt_Population)
	# , Wt_Cohort=as.numeric(Wt_Cohort)
	# , Serial=Serial
)

head(samp)
