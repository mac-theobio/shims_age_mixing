# shims_age_mixing
### Hooks for the editor to set the default target
current: target

target pngtarget pdftarget vtarget acrtarget: wpush 

##################################################################

# make files

Sources = Makefile .gitignore README.md stuff.mk LICENSE.md
include stuff.mk
# include $(ms)/perl.def

##################################################################

## Content

Sources += $(wildcard *.xlsx)
Sources += $(wildcard *.R)

shims1.Routput: 
shims1.Rout: shims1.R

testread.Rout: SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx testread.R

Sources += inc_samp.csv
inc_samp.Rout: inc_samp.csv SAMPLE_INCIDENCE_2017-03-18_05-08-18.xlsx readsamp.R
	$(run-R)

Sources += prev_samp.csv
prev_samp.Rout: prev_samp.csv SAMPLE_PREVALENCE_2017-03-18_05-08-09.xlsx readsamp.R
	$(run-R)

######################################################################

wpush:
	$(MAKE) sync
	git add -f *.wrapR.r
	git commit -m "Pushing wrapR" files
	git push

######################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk
# -include $(ms)/oldlatex.mk
