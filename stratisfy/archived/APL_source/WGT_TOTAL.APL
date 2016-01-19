R„WGT_TOTAL DATA;TMP;tmp;tmp2;I;variance
© Compute stratified total
© DATA <=> data array [STRATUM;LABEL UNITS (NESTED SET DATA)]
© R <=> survey results [(NESTED STRATA RESULTS) (STRATIFIED TOTAL)]
© Called by STRATIFIED
TMP„œWGT_STRATUM ¨›[2]DATA  © pass data for each stratum
R „data[;2] TOTAL_RESULTS TMP 