R�LF_STRATUM DATA;TMP;tmp
� Compute set results, stratum total and stratum mean
� DATA <=> stratum data [LABEL UNITS (NESTED SET DATA)]
� R <=> stratum results [LABEL TOTAL TOTALVARIANCE MEAN MEANVARIANCE (NESTED SET RESULTS)]
� TOTAL:sum numbers at length, scaled up to trawlable units, divided by number of sets 
� MEAN: sum numbers at length divided by number of sets

:IF 0���(3�DATA) 
    R�(1�DATA) ((�lengths)���)  ((�lengths)���)     ((�lengths)���)       ((�lengths)���)              (,[0.5]� ((�lengths)���)) ��0
:ENDIF

TMP��LF_SET ��[2]3�DATA

R�(2�DATA) STRATUM_RESULTS (1�DATA)(TMP)