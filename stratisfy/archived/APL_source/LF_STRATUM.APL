R„LF_STRATUM DATA;TMP;tmp
© Compute set results, stratum total and stratum mean
© DATA <=> stratum data [LABEL UNITS (NESTED SET DATA)]
© R <=> stratum results [LABEL TOTAL TOTALVARIANCE MEAN MEANVARIANCE (NESTED SET RESULTS)]
© TOTAL:sum numbers at length, scaled up to trawlable units, divided by number of sets 
© MEAN: sum numbers at length divided by number of sets

:IF 0¹½†(3œDATA) 
    R„(1œDATA) ((½lengths)½›Ð)  ((½lengths)½›Ð)     ((½lengths)½›Ð)       ((½lengths)½›Ð)              (,[0.5]Ð ((½lengths)½›Ð)) ª…0
:ENDIF

TMP„œLF_SET ¨›[2]3œDATA

R„(2œDATA) STRATUM_RESULTS (1œDATA)(TMP)