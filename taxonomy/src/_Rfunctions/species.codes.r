

  species.codes = function (txgrp=NULL, tx=NULL) {  
    
    # a lookup of table / translation table for various groups of species of interest
    # output: species code
    if (is.null( tx )) tx = taxa.db("complete")

    out = NULL
    
    for (tg in txgrp) { 
      
      spid = switch(tg,
        
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

        taxalevel         = tx$spec[ which( tx$rank_id==id ) ] ,
        speciesandgenera  = tx$spec[ which( tolower(tx$rank) %in% c("species", "genus") ) ] , 
        namesci           = tx$spec[ which( tx$name.scientific==id ) ], 
        namecom           = tx$spec[ which( tx$name.common==id ) ], 
        family            = tx$spec[ which( tx$rank_id >= itis.code.extract( DS="taxon.unit.types", value="family" )) ] , 
        family.or.genera  = tx$spec[ which( tolower(tx$rank) %in% c("family", "genus") ) ] , 
        commercial        = tx$spec[ which( tx$exploited=="yes" )] ,
        noncommercial     = tx$spec[ which( tx$exploited=="no" )] ,

        all               = tx$spec ,
        allfish           = tx$spec[ which( 
            tx$class %in% c("Actinopterygii", "Chondrichthyes", "Myxini") | 
            tx$superclass %in% c("Osteichthyes", "Agnatha" ) | 
            tx$general.taxonomic.group =="fish"  
          ) ] ,  
        
        forage.fish       = c(60, 64, 610) ,
        fish.ammodytidae = c(610, 599, 590, 611), # sand lance
        
        demersal          = c( tx$spec[ grep( "demersal", tx$habitat.vertical,  ignore.case=T ) ],
                tx$spec[ which( tx$order == "Pleuronectiformes" | tx$specific.taxonomic.group == "Flatfish" ) ],
                c(324, 382, 333, 45, 385, 196, 325, 8, 277, 805, 816, 693, 198,
                  345, 44, 282, 387, 379, 388, 141, 142, 42, 386, 140, 40, 344, 43, 30, 41, 49, 31, 568, 143, 346 ) ), 
        small.demersal    = intersect( tx$spec[ which( tx$size.max.len.cm <= 30) ] , species.codes( "demersal", tx) ) ,
        med.demersal      = intersect( tx$spec[ which( tx$size.max.len.cm > 30 & tx$size.max.len.cm < 100 ) ] , species.codes("demersal", tx) ) ,
        large.demersal    = intersect( tx$spec[ which( tx$size.max.len.cm >= 100) ] , species.codes( "demersal", tx) ) ,
        
        pelagic           = intersect( tx$spec[ grep( "pelagic", tx$habitat.vertical, ignore.case=T ) ] , species.codes("allfish", tx) ), 

        small.pelagic     = intersect( tx$spec[ which( tx$size.max.len.cm <= 30) ], species.codes( "pelagic", tx) ) , 
        large.pelagic     = intersect( tx$spec[ which( tx$size.max.len.cm >= 100) ], species.codes( "pelagic", tx) ) , 
        
        
        flatfish          = tx$spec[ which( tx$order == "Pleuronectiformes" | tx$specific.taxonomic.group == "Flatfish" ) ],
        fish.pleuronectiformes = c(324, 382, 333, 45, 385, 196, 325, 8, 277, 805, 816, 693, 198,
          345, 44, 282, 387, 379, 388, 141, 142, 42, 386, 140, 40, 344, 43, 30, 41, 49, 31, 568, 143, 346 ), # flounders

        gadoid            = tx$spec[ which( tx$order=="Gadiformes" | tx$specific.taxonomic.group =="Gadoid" ) ] ,
        fish.gadidae = c(10, 118, 17, 117, 11, 251, 16, 57, 110 ),
        fish.merlucciidae.phycidae = c(12, 193, 13, 111, 112,19, 14, 35 ), # hakes
        
        elasmobranchs     = tx$spec[ which( tx$subclass=="Elasmobranchii" ) ] ,
        chrondrichthyes   = tx$spec[ which( tx$class=="Chondrichthyes" ) ] ,
        
        crabs             = tx$spec[ which( tx$infraorder %in% c("Brachyura", "Anomura" ) ) ] ,
        crabs.oregoniidae = tx$spec[ which( tx$family %in% c("Oregoniidae" ) ) ] ,
        crabs.portunidae  = tx$spec[ which( tx$family %in% c("Portunidae" ) ) ] ,
        crabs.lithodidae  = tx$spec[ which( tx$family %in% c("Lithodidae" ) ) ] ,
        crabs.cancer      = tx$spec[ which( tx$family %in% c("Cancridae" ) ) ] ,
     
        skates            = tx$spec[ which( tx$order == "Rajiformes") ],
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
          6130, 6125, 6131, 6121, 6123, 6127, 6117),
        
        spec              = id 
      )
      out = c( out, spid )
    }
   
    out = sort( unique( out) ) 
    out = taxa.specid.correct( out )  # recode to internally consistent codes

    return (out ) 
  }


