LF�MAKE�LF LF;�IO
� Creates length frequency
� LF(right arguement) <=> matrix [record;LENGTH COUNT]
� LF(result) vector [LENGTH], including lengths with 0 count
� Called by MAKE_LF
�IO�1
:if lgrp�0.5 � if length group is not 0.5cm
    LF[;1]��0.5+LF[;1] � round to nearest cm
:endif
LF�1 COLSUM LF � sum and order by length
LF�,(ALL_LEN�LF[;1])�LF[;2] � expand for missing lengths
:if lgrp�0.5 � if length group is not 0.5cm
   LF�+/(((�LF)�lgrp),lgrp)�LF � group by lgrp
:endif
LF�Z2MIN�LF � drop lgrps from 0 to min