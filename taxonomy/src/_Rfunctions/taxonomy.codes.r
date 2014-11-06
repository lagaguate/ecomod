
  taxonomy.codes = function( taxa=NULL, method=c("direct", "complete"), outcode="spec" ) {
    
    # return the associated species list that matches required criteria or keyword 
    out = NULL

    if ( ("direct" %in% method) & (outcode!="tx.rowindex") ) {

      groundfish.codes  = list(
        cod               = 10 ,
        atlantic.cod      = 10 ,
        haddock           = 11 ,
        white.hake        = 12 ,
        silver.hake       = 14 ,
        halibut           = 30 ,
        american.plaice   = 40 ,
        winter.flounder   = 43 ,
        capelin           = 64 ,
        herring           = 60  ,
        mackerel          = 70 ,
        thornyskate       = 201 ,
        thorny.skate       = 201 ,
        atlanticwolffish  = 50 ,
        atlantic.wolffish  = 50 ,
        spiny.dogfish     = 220 ,
        sandlance         = 610 ,
        northernshrimp    = 2211 ,
        jonahcrab         = 2511 ,
        bluecrab          = 2512 ,
        atlanticrockcrab  = 2513 ,
        portlyspidercrab  = 2519 ,
        lessertoadcrab    = 2521 ,
        northernstonecrab = 2523 ,
        snowcrab          = 2526 ,
        toadcrab          = 2527 ,
        porcupinestonecrab = 2528 , 
        greencrab         = 2531 ,
        redcrab           = 2532 ,
        hermitcrab        = 2559 ,
        brittlestar       = 6200 ,
        basketstar        = 6300 ,
        lobster           = c(2550, 2551, 2552, 2553),
        arcticsurfclam    = 4355,
        oceanquahaug      = 4304,
        seascallops       = 4321,
        northernpropellerclam = 4312,
        squid             = c(4511, 4512, 4514),
        forage.fish       = c(60, 64, 610) ,
        demersal          = c(324, 382, 333, 45, 385, 196, 325, 8, 277, 805, 816, 693, 198,
                    345, 44, 282, 387, 379, 388, 141, 142, 42, 386, 140, 40, 344, 43, 30, 41, 49, 31, 568, 143, 346 ) ,
        fish.ammodytidae = c(610, 599, 590, 611), # sand lance
        fish.pleuronectiformes = c(324, 382, 333, 45, 385, 196, 325, 8, 277, 805, 816, 693, 198,
          345, 44, 282, 387, 379, 388, 141, 142, 42, 386, 140, 40, 344, 43, 30, 41, 49, 31, 568, 143, 346 ), # flounders
        fish.gadidae = c(10, 118, 17, 117, 11, 251, 16, 57, 110 ),
        fish.merlucciidae.phycidae = c(12, 193, 13, 111, 112,19, 14, 35 ), # hakes
        fish.rajidae = c(212, 204, 591, 210, 202, 1023, 208, 1007, 1021, 200, 589, 981, 201, 205, 967,
          1022, 217, 969, 206, 207, 211, 209, 203 ), # skates and rays
        redfish           = c(20, 21, 23) ,
        wolffish          = c(52, 51, 59, 50) , 
        fish.anarhichadidae = c(52, 51, 59, 50), # wolffish
        scallops.pectinidae = c(4320, 4324, 4321, 4322, 4336, 4325), # Pectinidae
        quahaug.arcticidae = c(4304 ),
        clam.Mactridae = c(4317, 4355), # surf clam
        clam.Hiatellidae = c(4327, 4319, 4312 ), # propeller clam
        shrimps.pandalidae = c(2100, 2213, 2215, 2212, 2210, 2214, 2211, 2200 ), # Pandalidae
        crabs.lithodidae = c(2523, 2525, 2528),
        crabs.majidae =c(2526, 2527, 2520, 2521, 2519),
        crabs.cancridae = c(2511, 2509, 2524, 2513),
        invert.lobster = c(2550, 2552, 2551, 2553),
        seastars.asteroidea =c(6100, 6111, 6134, 6110, 6132, 6113, 6133, 6135, 6128, 6115, 6119, 6129, 
          6130, 6125, 6131, 6121, 6123, 6127, 6117)
      )

      gg = names( groundfish.codes ) 
      mm = NULL
      for ( i in taxa) {
        nn = NULL
        nn = which( gg == i ) 
        if (length(nn)==1) mm = c( mm, groundfish.codes[[i]] )
      }
      txi = NULL
      if (!is.null(mm) ) {
        oo = sort( unique( mm ) ) 
        if (outcode == "spec" ) txi = oo  #no more to do 
        if (outcode == "spec.parsimonious" ) {
          txi = taxonomy.recode( from="spec", to="parsiminious", tolookup=oo )
          txi = sort( unique( txi ) ) 
        }
      }
      
      ## note ... no return statement here on purpose as the next if state needs to be processed as well
    
    }

  
    # -------------


    if ( "complete" %in% method ) {
      spid = NULL
      mm = NULL
      tx=taxonomy.db("parsimonious") 

      for (tg in taxa) { 
        browser()
        spid = switch(tg,
          taxalevel         = which( tx$rank_id==id )  ,
          speciesandgenera  = which( tolower(tx$rank) %in% c("species", "genus") )  , 
          namesci           = which( tx$name.scientific==id ) , 
          namecom           = which( tx$name.common==id ) , 
          family            = which( tx$rank_id >= itis.code.extract( DS="taxon.unit.types", value="family" ))  , 
          family.or.genera  = which( tolower(tx$rank) %in% c("family", "genus") )  , 
          commercial        = which( tx$exploited=="yes" ) ,
          noncommercial     = which( tx$exploited=="no" ) ,
          all               = 1:nrow(tx),
          allfish           = which( 
              tx$class %in% c("Actinopterygii", "Chondrichthyes", "Myxini") | 
              tx$superclass %in% c("Osteichthyes", "Agnatha" ) | 
              tx$general.taxonomic.group =="fish"  
            ) ,  
          demersal          = c( 
              grep( "demersal", tx$habitat.vertical,  ignore.case=T ) ,
              which( tx$order == "Pleuronectiformes" | tx$specific.taxonomic.group == "Flatfish" )) , 
          small.demersal    = intersect( which( tx$size.max.len.cm <= 30) , taxonomy.codes( taxa="demersal", outcode="tx.rowindex" ) ) ,
          med.demersal      = intersect( which( tx$size.max.len.cm > 30 & tx$size.max.len.cm < 100 ) , taxonomy.codes( taxa="demersal", outcode="tx.rowindex" ) ) ,
          large.demersal    = intersect( which( tx$size.max.len.cm >= 100)  , taxonomy.codes( taxa="demersal", outcode="tx.rowindex" ) ) ,
          
          pelagic           = intersect( grep( "pelagic", tx$habitat.vertical, ignore.case=T )  , taxonomy.codes( taxa="allfish", outcode="tx.rowindex" ) ), 

          small.pelagic     = intersect( which( tx$size.max.len.cm <= 30) , taxonomy.codes( taxa="pelagic", outcode="tx.rowindex" ) ) , 
          large.pelagic     = intersect( which( tx$size.max.len.cm >= 100) , taxonomy.codes( taxa="pelagic", outcode="tx.rowindex" ) ) , 
         
          living.only = taxonomy.codes( taxa="all", outcode="tx.rowindex"  ), #synomyms ..
          alltaxa     = taxonomy.codes( taxa="all", outcode="tx.rowindex"  ),
          coAll       = taxonomy.codes( taxa="all", outcode="tx.rowindex"  ),
          coFish      = taxonomy.codes( taxa="allfish", outcode="tx.rowindex"  ),
          # coInvert  = taxonomy.codes( taxa="invertebrates", outcode="tx.rowindex"  ),
          coElasmo    = taxonomy.codes( taxa="elasmobranchs", outcode="tx.rowindex"  ),

          coGadoid        = taxonomy.codes( taxa="gadoid", outcode="tx.rowindex"  ),
          coDemersal      = taxonomy.codes( taxa="demersal", outcode="tx.rowindex"  ),
          coPelagic       = taxonomy.codes( taxa="pelagic", outcode="tx.rowindex"  ),
          coSmall         = which( tx$size.max.len.cm <= 30)  ,
          coLarge         = which( tx$size.max.len.cm >= 100) ,
          coSmallPelagic  = taxonomy.codes( taxa="small.pelagic", outcode="tx.rowindex"  ),
          coLargePelagic  = taxonomy.codes( taxa="large.pelagic", outcode="tx.rowindex"  ),
          coSmallDemersal = taxonomy.codes( taxa="small.demersal", outcode="tx.rowindex"  ),
          coLargeDemersal = taxonomy.codes( taxa="small.demersal" , outcode="tx.rowindex" ),
          coFlatfish      = taxonomy.codes( taxa="flatfish", outcode="tx.rowindex"  ),
          flatfish          = which( tx$order == "Pleuronectiformes" | tx$specific.taxonomic.group == "Flatfish" ) ,
          gadoid            = which( tx$order=="Gadiformes" | tx$specific.taxonomic.group =="Gadoid" )  ,
          elasmobranchs     = which( tx$subclass=="Elasmobranchii" )  ,
          chrondrichthyes   = which( tx$class=="Chondrichthyes" )  ,
          crabs             = which( tx$infraorder %in% c("Brachyura", "Anomura" ) )  ,
          crabs.oregoniidae = which( tx$family %in% c("Oregoniidae" ) )  ,
          crabs.portunidae  = which( tx$family %in% c("Portunidae" ) )  ,
          crabs.lithodidae  = which( tx$family %in% c("Lithodidae" ) )  ,
          crabs.cancer      = which( tx$family %in% c("Cancridae" ) )  ,
          skates            = which( tx$order == "Rajiformes")         )
        
        mm = c( mm, spid )
      
      }
      
      oo = NULL
      if (!is.null( mm ) ) oo = sort( unique( mm ) ) 
      if (outcode == "tx.rowindex" ) out = oo  # nothing else to do
      if (outcode == "spec" )    out = c( txi, tx$spec[ oo ]  )
      if (outcode == "spec.parsimonious" ) out = c( txi, tx$spec.parsiminious[ oo ] ) 
      
      out = sort( unique(out) )

      return (out)
    }

    
    # -------------
    
    
    if ( metthod %in% c( "maxresolved", "parsimonious") ) {
      out = taxonomy.db( "parsimonious" )$spec.parsimonious  # full species list for bio using internal codes only
      out = sort( unique(out ) )
      return (out)
    }
   
  
  }



