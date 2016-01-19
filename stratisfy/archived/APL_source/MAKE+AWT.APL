AWK„MAKE‘AWT AWK;ALLPAIRS;MAXAGE;ŒIO;COUNT;I;SHAPE‘AWT;SHAPE‘COUNT
© Creates age weight key
© AWK(right arguement) <=> matrix [record;LENGTH AGE COUNT FWT]
© AWK(result) <=> matrix [AGE;LENGTH]
© Called by MAKE_ALK
ŒIO„1
:if lgrp¬0.5 © if length group is not 0.5cm
    AWK[;1]„˜0.5+AWK[;1] © round to nearest cm
:endif
MAXAGE„—/,AWK[;2] © MAXIMUM AGE
ALLPAIRS„ALL_LEN PAIR 0,¼MAXAGE © ALL POSSIBLE LEN/AGE PAIRS, including age 0
ALLPAIRS„(ALLPAIRS,0),0
AWK„1 2 COLSUM ALLPAIRS®AWK © COUNTS FOR ALL POSSIBLE PAIRS
COUNT„((½ALL_LEN),1+MAXAGE)½AWK[;3] © Counts for average
AWK„((½ALL_LEN),1+MAXAGE)½AWK[;4]   © RESHAPE INTO MATRIX, allowing for age 0
:if lgrp¬0.5 © if length group is not 0.5cm
  AWK  „³+/(((²½AWK)÷1,lgrp),lgrp)½³AWK     © GROUP BY LGRP
  COUNT„³+/(((²½COUNT)÷1,lgrp),lgrp)½³COUNT © GROUP BY LGRP
:endif
SHAPE‘COUNT„½COUNT  ©Shape of the matrix
COUNT„,COUNT                ©
I„(COUNT=0)/¼½COUNT         ©Convert 0's to Ð's to avoid..
COUNT[I]„›Ð                 ©..0÷0=1 errors.
COUNT„SHAPE‘COUNT½COUNT     ©
AWK„AWK÷COUNT       ©Find average by taking sum of weights and dividing by count.
AWK„,AWK                    ©
AWK[I]„0                    ©Convert the Ð's back to 0's.
AWK„SHAPE‘COUNT½AWK         ©
AWK„(Z2MIN,0)‡AWK © drop lgrps from 0 to min
AWK„(((1†½AWK),16)†AWK),+/0 16‡AWK © MAKE AGE 16+ GROUP, first column is age 0