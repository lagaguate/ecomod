
#### Import lobster data from various databases

    
loadfunctions( "lobster", functionname="initialise.local.environment.r") 

##### lumped function lobster.db

        # run in windows emvironment
        lobster.db()


	# load .RData objects

        lobster.db( DS="logs")		# Inshore logs summary documents
        lobster.db( DS="logs41")	# Offshore logs monitoring documents
        lobster.db( DS="atSea")		# at Sea sampling from materialized view
        lobster.db( DS="cris")		# CRIS database
        lobster.db( DS="port")		# Port Sampling
        lobster.db( DS="vlog")		# Voluntary logs
        lobster.db( DS="fsrs")		# FSRS recruitment traps
        lobster.db( DS="scallop")	# scallop survey bycatch
        lobster.db( DS="survey")	# ITLS Lobster Survey


#### FSRS recruitment traps only

recruitment.trap.db('raw.redo')
