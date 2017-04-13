library(ggplot2)
theme_set(theme_bw())

print(ggplot(samp, aes(x=Sexprt_Num))
	+ geom_histogram()
	+ facet_wrap(~ Gender)
)
