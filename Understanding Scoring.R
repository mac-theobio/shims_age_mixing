
# Scoring Trial 1
# creating a vector of age differences
mmvar <- seq(min(DT.reldata.men$Age.difference),max(DT.reldata.men$Age.difference), length.out = 101)
mmvar.df <- data.frame(Age.difference = mmvar)


M1.beta <- condom.M1$beta
Threshold1 <- condom.M1$alpha[1]
Threshold2 <- condom.M1$alpha[2]

mmvar.beta <- mmvar * M1.beta
transformed <- plogis(mmvar.beta - Threshold1) + plogis(mmvar.beta - Threshold2)
transformed.mine <- (1 - plogis(Threshold1 - mmvar.beta))+ (1-plogis(Threshold2 - mmvar.beta))
plot(mmvar.beta)
# tidycond.1b$fit == transformed

# Scoring Trial 2
# spline term

mmvar.spline <- model.matrix(delete.response(terms(condom.M2)), mmvar.df) # creating a design matrix using new dataframe

M2.beta <- as.matrix(c("Intercept" = 0, condom.M2$beta))

mmvar.spline.beta <- mmvar.spline %*% M2.beta 

Threshold1 <- condom.M2$alpha[1]
Threshold2 <- condom.M2$alpha[2]

transformed.2 <- plogis(mmvar.spline.beta - Threshold1) + plogis(mmvar.spline.beta - Threshold2)

# Predictions.condom.M2$fit == transformed.2 # check if we get same results