R�s_area AREA_STRATUM XX;tmp
� Author:Travis Shepherd
� Creation Date: Mar. 20, 2003
tmp�(+/~0=XX)��XX � proportion present in stratum
tmp�tmp, (((+/~0=XX)��XX)�((+/0=XX)��XX))�((�XX)-1) � variance of proportion present in stratum
R�, �,tmp[1]�s_area � total area
R�R, �,tmp[2]�(s_area*2) � total variance
R�R, (�,tmp[1]), (,tmp[2]) (�) � add proportion data to variable to return, � is for no catch info