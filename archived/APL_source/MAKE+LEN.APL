R„LGRP MAKE‘LEN MINMAX;Œelx
Œelx„"ON_ERROR ª Œerror ' '"
©
© Returns vector of lengths from min to max
©
© LGRP => desired length group
© MINMAX => vector of min max length
© R => vector of lengths from min to max
©
© Called by: EXEC

R„ALL_LEN„LGRP ALL‘LEN MINMAX © get vector of all lengths from 0 to max
:if Ÿ/LGRP=2 3
    R„(((½R)÷LGRP),LGRP)½R  © group into LGRP
    R„(+/R)÷LGRP © midpoint of length group
:endif
R„(˜(1†MINMAX)÷LGRP)‡R © drop lengths from 0 to min