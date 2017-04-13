# shims_age_mixing
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: xntest.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md
include stuff.mk
# include $(ms)/perl.def

##################################################################

## Content

Sources += $(wildcard *.xlsx)
Sources += $(wildcard *.R)

### Emanuel's code (print statements added0

shims1.Routput: 
shims1.Rout: shims1.R


### Reading and checking the data

testread.Rout: SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx testread.R

### Not looking at Incidence sample right now
### Also, probably not going forward with _samp.csv, since read_excel behaved disappointingly
Sources += inc_samp.csv
inc_samp.Rout: inc_samp.csv SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx readsamp.R
	$(run-R)

prev_samp.Rout: SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx readsamp.R
	$(run-R)

prev_partner_hist.Rout: prev_samp.Rout prev_partner_hist.R

######################################################################

### Examine distributions of reported numbers of sex partners

######################################################################

### Issues

nametest.Rout: nametest.csv nametest.R

xntest.Rout: nametest.xlsx xntest.R

######################################################################

wpush:
	$(MAKE) sync
	git add -f *.wrapR.r
	git commit -m "Pushing wrapR files"
	git push

######################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
