R�WGT_STRATUM DATA;TMP;tmp
� Compute set results, stratum total and stratum mean
� DATA <=> stratum data [LABEL UNITS (NESTED SET DATA)]
� R <=> stratum results [(NESTED SET RESULTS) LABEL TOTAL MEAN]
� TOTAL:sum numbers at length, scaled up to trawlable units, divided by number of sets 
� MEAN: sum numbers at length divided by number of sets


:IF 0���(3�DATA)
    R�(1�DATA) (�)   (�)             (�)     (�)     (,[0.5]� �) ��0
:ENDIF

TMP�� WGT_SET ��[2]3�DATA
R� (2�DATA) STRATUM_RESULTS (1�DATA)(TMP)