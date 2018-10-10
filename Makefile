# shims_age_mixing
### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

# stuff

Sources = Makefile README.md LICENSE.md
Ignore += .gitignore

ms = makestuff
-include $(ms)/os.mk

# -include $(ms)/perl.def

msrepo = https://github.com/dushoff
Ignore += $(ms)
Makefile: $(ms) $(ms)/Makefile
$(ms):
	git clone $(msrepo)/$(ms)

######################################################################

## Content

Sources += $(wildcard *.R)

### Emanuel's code (print statements added)

shims1.Routput: 
shims1.Rout: shims1.R


### Reading and checking the data

testread.Rout: SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx testread.R

### Not looking at Incidence sample right now
prev.Rout:  SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx simpleRead.R
	$(run-R)

prev_partner_hist.Rout: prev.Rout prev_partner_hist.R

######################################################################

### Examine distributions of reported numbers of sex partners

######################################################################

### Issues

Sources += nametest.csv
nametest.Rout: nametest.csv nametest.R

xntest.Rout: nametest.xlsx xntest.R

### Also, probably not going forward with _samp.csv, since read_excel behaved disappointingly
## Now I think the problem is that the xlsx file is also a mess; still not sure why we shouldn't get the same behaviour from numeric as as.numeric
Sources += inc_samp.csv prev_samp.csv
inc_samp.Rout: inc_samp.csv SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx readsamp.R
	$(run-R)
prev_samp.Rout: prev_samp.csv SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx readsamp.R
	$(run-R)

######################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

# -include $(ms)/wrapR.mk

