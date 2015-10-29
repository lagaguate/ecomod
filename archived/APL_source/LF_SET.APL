R„LF_SET SET;tmp;wgt_ratio
© Calculate length frequency by set adjusted for subsampling and distance
© SET <=> nested data for a set; set#, distance, slat, slong, unitarea, sampled weight, total weight, details
© R <=> nested result; set#, adjusted length frequency
© Called by LF_STRATUM

:if (6œSET)=0   © if no sample weight
  wgt_ratio „ 1 © no adjustment for weight
:else
  wgt_ratio „ ((7œSET)÷6œSET) © total weight divided by sample weight
:endif
 
tmp„(wgt_ratio×(distance÷2œSET)×MAKE_LF 8œSET)© make lf and adjust
R„(1œSET) (tmp,+/tmp)