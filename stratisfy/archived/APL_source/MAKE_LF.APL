LFT„MAKE_LF DATA;TARG;J
© Makes length frequencies by sex or combined
© DATA <=> matrix [record;SEX LENGTH COUNT]
© LFT <=> length frequency
© called by LF_SET
:if sexed='Y'

    LFT„¼0
    :for J :in 0 1 2 © do for unspecified, male and female
        :if 0<1†½TARG„(J=DATA[;1])šDATA

            LFT„LFT,MAKE‘LF TARG[;2 3] © Make LF by sex
        :else

            LFT„LFT,(½lengths)½0 © put zeroes
        :endif
    :endfor
:else
    LFT„MAKE‘LF DATA[;2 3] © Make combined LF
:endif