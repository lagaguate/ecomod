R„SETUP data;TMP;TMP2;UNIK;BOOL;inf;cat;lf;detail;I;J;K;wgt_ratios;set_ids;tcat;ind_cat;ind_lf;set_size_classes

inf        „1œdata
cat        „2œdata
lf         „3œdata

lgrp„ combo_species['Form.lsSpecies' Œwi 'value';3] 

:IF   0='Form.ckSexed' Œwi 'enabled' 
:ORIF 0='Form.ckSexed' Œwi 'value'
    sexed„'N'
:ELSE
    sexed„'Y'
:ENDIF

©Check to see which analyses will be performed.
length_analysis„age_analysis„'N'
:if 0<1†½lf
    length_analysis „'Y' © compress for records with age
    detail„det®detplus
    detail„(~(›Ð)­¨detail[;4])šdetail  © compress for records with age
    :if 0¬1†½detail
       age_analysis„'Y'
    :endif   
:endif

©Combine multiple Size Classes
:if 1='Form.opDFO'  Œwi 'value'                     © for DFO data only
  :if 1<(—/cat[;4])                                 © check for multiple size classes in catch
    set_ids„(2=cat[;4])/cat[;1]                     © set ids with more than one size class, assumes 2 always occurs
    :for I :in ¼½set_ids                            © for each set
        tcat„(ind_cat„cat[;1]¹set_ids[I])/[1]cat    © rows of cat for set I
        ind_cat„(ind_cat/¼1†½cat)                   © indices of rows of cat for set I
        ind_cat„(1=cat[ind_cat;4])/ind_cat          © row index of cat for set I where size class is 1
        wgt_ratios„tcat[;3]÷tcat[;2]                © weight ratios for set I
        set_size_classes„tcat[;4]                   © size classes for set I
        :for K :in ¼½set_size_classes               © for eack size class
            ind_lf„lf[;1]¹set_ids[I]                © rows of lf for set I
            ind_lf„(ind_lf/¼1†½lf)                  © indices of rows of lf for set I
            ind_lf„(set_size_classes[K]=lf[ind_lf;5])/ind_lf          © row index of lf for set I where size class is 1
            lf[ind_lf;4]„lf[ind_lf;4]×wgt_ratios[K] © adjust lf in size class K by weight ratio
        :endfor
        cat[ind_cat;2 3]„+/[1]tcat[;3]              © sample and total weight for size class 1 replaced by set total weight
    :endfor
    cat„(1=cat[;4])/[1]cat                          © keep only size class 1 rows from cat 
  :endif
  cat„0 ¯1‡cat                                      © delete size class column from cat
  :if 'Y'= length_analysis                          © check for length frequency
    lf„0 ¯1‡lf                                      © delete size class column from lf
  :endif                                          
:endif

R„strata
BOOL„(strata[;1])°.­inf[;1] © rows of inf by strata
BOOL„›[2]BOOL
TMP„SETNO_DIST ¨BOOL © put setno and dist and wgts into TMP
:IF 'Y'= length_analysis
    TMP„TMP ,¨œ¨DETAIL_LF  ¨TMP © put LF data into TMP
:ENDIF
R„R,TMP