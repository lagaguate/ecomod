R„Lgrp ALL‘LEN MinMax
©
© Returns vector of lengths from 0 to max
©
© Lgrp => desired length group
© MinMax => vector of min max length
© R => vector of lengths from 0 to max
©
© Called by MAKE‘LEN 

:select Lgrp
:case 0.5 © for 0.5 cm group
    R„0,0.5×¼—2×¯1†MinMax © vector of all lengths from 0
:else © for all other length groups
    R„¯1+¼Lgrp×—(1+¯1†MinMax)÷Lgrp © vector of all lengths from 0
:endselect