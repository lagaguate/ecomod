ALKT„MAKE_ALK DATA;TARG;ALKT;J
© Makes age length keys by sex or combined
© DATA <=> matrix [record;SEX LENGTH AGE COUNT]
© ALKT <=> age length key
© called by STRATIFIED
:if sexed='Y'
    ALKT„0 17½0
    :for J :in 0 1 2 © do for unspecified, male and female
        :if 0<1†½TARG„(J=DATA[;1])šDATA
            ALKT„ALKT,[1]MAKE‘ALK TARG[;2 3 4] © Make LF by sex
        :else
            ALKT„ALKT,[1]((½Z2MIN‡ALL_LEN),17)½0 © put zeroes
        :endif
    :endfor
:else
    ALKT„MAKE‘ALK DATA[;2 3 4] © Make combined LF
:endif