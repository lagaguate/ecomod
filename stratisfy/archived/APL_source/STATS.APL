Z�STATS data;avg
�DATA �� vector of numbers
�Z    �� (MEAN)(VARIANCE) 
data�,data � make sure variable is a vector
:if 1=�data
    Z�((+/data)��data) �    � Pass mean only if only one datum
:else
    Z  �(+/data)��data      � Pass mean..
    Z  �Z,((+/((data-Z)*2))�((�data)-1))��data �.. and mean variance
:endif