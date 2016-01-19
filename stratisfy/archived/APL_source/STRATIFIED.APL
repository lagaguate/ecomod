R„STRATIFIED DATA;alk;strat_af;strat_lf;strat_wgt

© Compute stratified analysis results
© DATA <=> nested data array by stratum and by set
© R <=> alk, and nested result array by stratum and by set for length, age, and weight 
© Called by EXEC

alk„strat_lf„strat_af„strat_wgt„Ð
:if 'Y'=length_analysis
    Z2MIN„(˜(1†len_range)÷lgrp) © lgrps from 0 to min to drop
    strat_lf„LF_TOTAL DATA © length calculations
    :if 'Y'=age_analysis
        alk„0 1‡det®detplus © catenate all detail records and drop set id
        alk„(~(›Ð)­¨alk[;3])šalk © compress for records with age
        alk„MAKE_ALK alk © create ALK array
        palk„œ(›[2]alk)÷1E¯20—+/alk © convert alk to proportion
        palk„palk,0=+/palk © append column for unknown age
        strat_af„AF_TOTAL strat_lf © age calculations
    :endif
:endif
strat_wgt„WGT_TOTAL DATA © weight calculations
R„alk strat_lf strat_af strat_wgt   