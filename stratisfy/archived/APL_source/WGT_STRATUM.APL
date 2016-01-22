R„WGT_STRATUM DATA;TMP;tmp
© Compute set results, stratum total and stratum mean
© DATA <=> stratum data [LABEL UNITS (NESTED SET DATA)]
© R <=> stratum results [(NESTED SET RESULTS) LABEL TOTAL MEAN]
© TOTAL:sum numbers at length, scaled up to trawlable units, divided by number of sets 
© MEAN: sum numbers at length divided by number of sets


:IF 0¹½†(3œDATA)
    R„(1œDATA) (Ð)   (Ð)             (Ð)     (Ð)     (,[0.5]Ð Ð) ª…0
:ENDIF

TMP„œ WGT_SET ¨›[2]3œDATA
R„ (2œDATA) STRATUM_RESULTS (1œDATA)(TMP)