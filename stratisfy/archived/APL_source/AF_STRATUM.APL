R�AF_STRATUM STRATUM;TMP;AREA;tmp;UNITS
� Compute set results, stratum total and stratum mean
� STRATUM <=> stratum length results [(NESTED SET DATA) LABEL STRATUM_TOTAL STRATUM_MEAN MEANVARIANCE]
� R <=> stratum results [LABEL TOTAL TOTALVARIANCE MEAN MEANVARIANCE (NESTED SET RESULTS)]
� TOTAL:sum numbers at length, scaled up to trawlable units, divided by number of sets 
� MEAN: sum numbers at length divided by number of sets
� Called by AF_TOTAL

:IF 0���6�STRATUM
   R�(1�STRATUM)  (19���) (19���) (19���) (19���) (,[0.5] � (19���))
   �0
:ENDIF

TMP��AF_SET ��[2]6�STRATUM � pass results for each set
UNITS�((strata��(1�STRATUM))[;1])/(data[;2])� area from strata(global) to calculate trawlable units

� set and af; (stratum; stratum abundance; stratum average per tow)
R�UNITS STRATUM_RESULTS (1�STRATUM)(TMP)