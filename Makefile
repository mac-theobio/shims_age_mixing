# shims_age_mixing
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: inc_samp.Rout 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md
include stuff.mk
# include $(ms)/perl.def

##################################################################

## Content

Sources += $(wildcard *.xlsx)
Sources += $(wildcard *.R)
Sources += $(wildcard *.wrapR.r)

shims1.Routput: 
shims1.Rout: shims1.R

testread.Rout: SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx testread.R
inc_samp.Rout: sample.csv SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx readsamp.R
	$(run-R)

######################################################################

### Makestuff

## Change this name to download a new version of the makestuff directory
# Makefile: start.makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
