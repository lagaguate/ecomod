R„DETAIL_LF SETS;BOOL
© Create nested array of detail lf records by set in a stratum
© SETS <=> set info matrix [record;SET# DISTANCE SLAT SLONG UNITAREA SAMPLEDWT TOTALWT]
© R <=> nested result by set
:IF SETS­,[0.5](Ð) (Ð) (Ð) (Ð) (Ð) (Ð) ©If there is no set lf information.
    R„›,[0.5]›(Ð Ð Ð)
:ELSE
    BOOL„(,0 ¯6‡SETS)°.­lf[;1] © rows of detail by sets
    BOOL„›[2]BOOL © nest
    R„SET_LF ¨BOOL © put LF data into R for each set
:ENDIF