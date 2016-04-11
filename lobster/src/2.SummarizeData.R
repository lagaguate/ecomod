    
    loadfunctions( "lobster", functionname="initialise.local.environment.r") 
  

    p$lfas = c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34") # specify lfas for data summary
    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

   

    ## Carapace Length Frequency Plots

    # Lobster Survey
	CarapaceLengthFrequencies(LFAs='34', DS='LobsterSurvey', fn='v1')
	CarapaceLengthFrequencies(LFAs='34', DS='LobsterSurvey', Yrs=2010:2015, rel=F, ymax=5, bins=seq(0,200,1), pdf=F)

	# Scallop Survey
	CarapaceLengthFrequencies(LFAs='34', DS='ScallopSurvey')
	CarapaceLengthFrequencies(LFAs='34', DS='ScallopSurvey', Yrs=2010:2015, rel=F, ymax=1, bins=seq(0,200,1), fn='v2')
	
	
	# at Sea Sampling
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='atSea', by="LFA", fn='byLFA')
	CarapaceLengthFrequencies(LFAs= '34', DS='atSea', by=c("Q","SEX"), fn='34')
	
	
	# Port sampling
	CarapaceLengthFrequencies(LFAs= c("27", "28", "29", "30", "31", "32", "33", "34"), DS='port', Yrs=2007:2015, by="LFA", fn='byLFA')
	CarapaceLengthFrequencies(LFAs='34', DS='port', Yrs=2010:2015, by="SEX", bins=seq(0,200,1), fn='34')
	
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10))
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=c(seq(0,70,10),75,seq(80,200,10)))


