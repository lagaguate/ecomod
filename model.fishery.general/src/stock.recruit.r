# based upon FAO tutorials
# data for fit of Beverton and Holt, and Ricker models to the Baltic Cod
# Subdivisions 25-32 estimates of R(age2) and SSB.
# The data on Recruitment and SSB are the output of an 
# ICA analysis of the data for Baltic Cod Subdivisions 25-32


cod = read.table( file.path( project.codedirectory("model.fishery.general"), "src", "stock.recruit.data"), header=T)

# reformat data such that each SSB is associated with the correct R.age2

time.diff = 2 # no. year different between SSB and R
years = range(cod$Year)
nyears = nrow(cod)

sr = array(dim=c(nyears+time.diff, 3) )
sr[, 1] = c( (years[1]-time.diff) : years[2] ) 
sr[(1:nyears)+time.diff, 2] = cod$SSB
sr[(1:nyears), 3] = cod$R.age2
sr = as.data.frame(sr)
names(sr) = c("yr", "ssb", "rec")


# Beverton-Holt: 
#   R = A * SSB / (SSB+B)
nls.BH = nls(rec ~ A * ssb / (ssb+B), data=sr, start=list(A=4e5, B=2e5),na.action="na.exclude")
summary(nls.BH)

# Ricker: 
#   R = A * SSB * exp(-SSB/K)
nls.Ricker = nls(rec ~ A * ssb * exp(-ssb/K), data=sr, start=list(A=1, K=1e6),na.action="na.exclude")
summary(nls.Ricker)




