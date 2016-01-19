R„LF_TOTAL DATA;TMP;tmp;tmp2;I;variance
© Compute stratified total
© DATA <=> data array [STRATUM;LABEL UNITS (NESTED SET DATA)]
© R <=> survey results [(STRATIFIED TOTAL) (STRATIFIED TOTAL VAR) (STRATIFIED MEAN) (STRATIFIED MEAN VAR) (NESTED STRATA RESULTS) ]
TMP„œLF_STRATUM ¨›[2]DATA   © pass data for each stratum
R „data[;2] TOTAL_RESULTS TMP 