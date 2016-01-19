R„WGT_SET SET;tmp
© Calculate weight by set adjusted for distance
© SET <=> weight data [SET# DISTANCE SLAT SLONG (SAMPLED WEIGHT) (TOTAL WEIGHT) DETAIL]
© R <=> result [SET# (ADJUSTED TOTAL WEIGHT)]
© Called by WGT_STRATUM 
tmp„(¹((distance÷2œSET)×7œSET))
R„ (1œSET) (tmp)