ALK�MAKE�ALK ALK;ALLPAIRS;MAXAGE;�IO
� Creates age length key
� ALK(right arguement) <=> matrix [record;LENGTH AGE COUNT]
� ALK(result) <=> matrix [AGE;LENGTH]
� Called by MAKE_ALK
�IO�1
:if lgrp�0.5 � if length group is not 0.5cm
    ALK[;1]��0.5+ALK[;1] � round to nearest cm
:endif
MAXAGE��/,ALK[;2] � MAXIMUM AGE
ALLPAIRS�ALL_LEN PAIR 0,�MAXAGE � ALL POSSIBLE LEN/AGE PAIRS, including age 0
ALK�1 2 COLSUM (ALLPAIRS,0)�ALK � COUNTS FOR ALL POSSIBLE PAIRS
ALK�((�ALL_LEN),1+MAXAGE)�ALK[;3] � RESHAPE INTO MATRIX, allowing for age 0
:if lgrp�0.5 � if length group is not 0.5cm
  ALK��+/(((��ALK)�1,lgrp),lgrp)��ALK � GROUP BY LGRP
:endif
ALK�(Z2MIN,0)�ALK � drop lgrps from 0 to min
ALK�(((1��ALK),16)�ALK),+/0 16�ALK � MAKE AGE 16+ GROUP, first column is age 0