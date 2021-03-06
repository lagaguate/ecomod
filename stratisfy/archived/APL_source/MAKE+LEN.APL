R�LGRP MAKE�LEN MINMAX;�elx
�elx�"ON_ERROR � �error ' '"
�
� Returns vector of lengths from min to max
�
� LGRP => desired length group
� MINMAX => vector of min max length
� R => vector of lengths from min to max
�
� Called by: EXEC

R�ALL_LEN�LGRP ALL�LEN MINMAX � get vector of all lengths from 0 to max
:if �/LGRP=2 3
    R�(((�R)�LGRP),LGRP)�R  � group into LGRP
    R�(+/R)�LGRP � midpoint of length group
:endif
R�(�(1�MINMAX)�LGRP)�R � drop lengths from 0 to min