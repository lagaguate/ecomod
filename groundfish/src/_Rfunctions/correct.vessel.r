
  correct.vessel = function (x) {
     # correct for different survey vessels (after, L.P Fanning 1985):
      #   to obtain Alfred Needler comparable units:
      # Lady Hammond (1982) and Alfred Needler (1983 to present) used a Western IIA Otter Trawl
      # whereas The A.T. Cameron used a Yankee 36 ft trawl between 1970 to 1981
        cf = NULL
        x$cfvessel = 1  # initialise

      # vessel change correction factors apply to these years:
        HAM=1   #  Lady Hammond (1979 - 1981)
        ATC=2   #  A.T. Cameron (1982 - 1983)

      # species codes used by the database
        cod=10
        haddock=11
        whitehake=12
        silverhake=19
        plaicelarge=40
        plaicesmall=40
        witch=41
        yellowtail=42
        winterflounder=43

        cf$cod[HAM]         = 0.8
        cf$haddock[HAM]     = 1.0
        cf$whitehake[HAM]   = 1.0
        cf$silverhake[HAM]  = 1.0
        cf$plaicesmall[HAM] = 1   # <=28cm
        cf$plaicelarge[HAM] = 1   # > 28cm
        cf$witch[HAM]       = 0.8
        cf$yellowtail[HAM]  = 0.8
        cf$winterflounder[HAM] = 1.0

        cf$cod[ATC]         = 0.8
        cf$haddock[ATC]     = 1.2
        cf$whitehake[ATC]   = 1.0
        cf$silverhake[ATC]  = 1.0
        cf$plaicesmall[ATC] = 0.7
        cf$plaicelarge[ATC] = 1.0
        cf$witch[ATC]       = 0.8
        cf$yellowtail[ATC]  = 0.8
        cf$winterflounder[ATC] = 1.0
      attach (x)
        x$cfvessel[ which( (substring(id,1,3)=="HAM" & spec==cod)) ] = cf$cod[HAM]
        x$cfvessel[ which((substring(id,1,3)=="HAM" & spec==witch)) ] = cf$witch[HAM]
        x$cfvessel[ which((substring(id,1,3)=="HAM" & spec==yellowtail)) ] = cf$yellowtail[HAM]
        x$cfvessel[ which((substring(id,1,3)=="ATC" & spec==cod)) ] = cf$cod[ATC]
        x$cfvessel[ which((substring(id,1,3)=="ATC" & spec==haddock)) ] = cf$haddock[ATC]
        x$cfvessel[ which((substring(id,1,3)=="ATC" & spec==plaicesmall && len<=28)) ] = cf$plaicesmall[ATC]
        x$cfvessel[ which((substring(id,1,3)=="ATC" & spec==witch)) ] = cf$witch[ATC]
        x$cfvessel[ which((substring(id,1,3)=="ATC" & spec==yellowtail)) ] = cf$yellowtail[ATC]
      detach (x)
    return (x)
  }


