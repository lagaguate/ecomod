LFT�MAKE_LF DATA;TARG;J
� Makes length frequencies by sex or combined
� DATA <=> matrix [record;SEX LENGTH COUNT]
� LFT <=> length frequency
� called by LF_SET
:if sexed='Y'

    LFT��0
    :for J :in 0 1 2 � do for unspecified, male and female
        :if 0<1��TARG�(J=DATA[;1])�DATA

            LFT�LFT,MAKE�LF TARG[;2 3] � Make LF by sex
        :else

            LFT�LFT,(�lengths)�0 � put zeroes
        :endif
    :endfor
:else
    LFT�MAKE�LF DATA[;2 3] � Make combined LF
:endif