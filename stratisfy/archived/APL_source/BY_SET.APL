R„BY_SET DATA;I;TEMP
© DATA <=> element 2, 3 or 4 of results after analysis
© e.g. DATA is 2œresults1999

DATA„5œDATA
TEMP„BY_ROW ¨DATA[;6]
R„ (0,4+—š0 1‡œ½¨TEMP) ½0
:for I :in ¼½TEMP
    :if ~Ð­†IœTEMP
        R„R®((›Iœ((DATA)[;1])),(œdata[I;3])[;3 4 5],IœTEMP)
    :endif
:endfor