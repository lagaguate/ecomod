
# Log-linear model: parameter estimation				
				
# Set up a Log-linear model to standardise CPUE indices 
# (Cpue  RV surveys data for Baltic cod in 25 - 32 
# subdivisions 1990 -1996).  				
# A2	Year	Month	SD	LN(A2)

cod = read.table( file.path( project.direcotyr("model.fishery.general"), "src", "standardised.cpue.data"), header=T)
lm.cod = lm( LN.A2 ~ as.factor(Year)+as.factor(Month)+as.factor(SD) -1, data=cod)
summary(lm.cod)


