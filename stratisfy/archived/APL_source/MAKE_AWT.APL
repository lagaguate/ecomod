ALW�MAKE_AWT DATA;TARG;J
� Makes average weight at age and length by sex or combined
� DATA <=> matrix [record;SEX LENGTH AGE COUNT FWT]
� ALW <=> age length weight
� called by CREATE_OUTPUT
:if sexed='Y'
    ALW�0 17�0
    :for J :in 0 1 2 � do for unspecified, male and female
        :if 0<1��TARG�(J=DATA[;1])�DATA
            ALW�ALW,[1]MAKE�AWT TARG[;2 3 4 5] � Make LF by sex
        :else
            ALW�ALW,[1]((�Z2MIN�ALL_LEN),17)�0 � put zeroes
        :endif
    :endfor
:else
    ALW�MAKE�AWT DATA[;2 3 4 5] � Make combined LF
:endif