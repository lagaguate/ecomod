R„SETNO_DIST BOOL;TMP
© result contains setno, dist, lat, long, sampwgt, totwgt and unitarea
:IF 0¬+/BOOL
    R„BOOL/[1]inf[;2 3 4 5 6]
    TMP„(cat[;1])¼R[;1]
    R„R,(cat®0 0 0)[TMP;2 3]
:ELSE
    R„,[0.5](Ð) (Ð) (Ð) (Ð) (Ð) (Ð) 
:ENDIF