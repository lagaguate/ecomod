R„UNITS TOTAL_RESULTS DATA;varindex;avgindex;I

varindex„avgindex„0½0

:FOR I :IN ¼½DATA[;6]
    :IF Ð­œ(œDATA[I;6])[1;1]
    :ELSEIF 1=†½œ(DATA)[I;6]
        avgindex„avgindex,I
    :ELSE
        avgindex„avgindex,I
        varindex„varindex,I
    :ENDIF
:ENDFOR

variance„¹+/DATA[varindex;3]    

© TOTAL                TOTALVAR      MEAN                                                      MEANVARIANCE                       NESTED DATA     
R„(¹+/TMP[avgindex;2]) (variance)    (¹+/(UNITS[avgindex]÷+/UNITS[avgindex])×DATA[avgindex;4]) (variance÷((+/UNITS[varindex])*2)) DATA 