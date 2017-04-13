library(ggplot2)

print((samp$Sexprt_Num))

print(ggplot(samp, aes(x=Sexprt_Num))
	+ geom_histogram()
)


