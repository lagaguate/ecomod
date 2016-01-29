output„lengths CREATE_OUTPUT results;age;len;alk;alkplus;t;tmp;colsum;lw;J;alt;awtage;awtlen;awt;alw;aln;detplus_with_ages;alws;nrows;aln0;awt0;I;alts;alt0

© Convert results into variables which will be used to send the data to Excel
© results <=> results produced by STRATIFIED
© lengths <=> lengths
© output  <=> formated results
© called by EXEC 

output„0½0 © initialize
age„(0,¼15),(›'16+'),(›'Unkown') © age labels
:IF 'Y'=length_analysis
  len„lengths
  :IF 1='Form.ckSexed' Œwi 'value'
      len„len,len,len
      lengths„•¨(›[2]'U',[1.5]lengths),(›[2]'M',[1.5]lengths),(›[2]'F',[1.5]lengths) © length labels for sexed data
  :ENDIF
:ENDIF

© Create the Query sheet information. 
t„0 2½0 © create a 2 column array to hold query information
t„t®  (›'STRANAL version'),›(version) 
t„t®  (›'Date/Time of Analysis'),›(7 DATESPELL ŒTS) © processing date
t„t®(›''),(›'') © add a blank row
t„t®  (›'Title'),(›'Form.edTitle' Œwi 'text')
:IF 1='Form.opDFO' Œwi 'value'
    t„t®(›'Data Source'),(›'DFO')
:ELSE
    t„t®(›'Data Source'),(›'NMFS')
:ENDIF
t„t®(›'Mission') ,(›"'",¯1 ‡,("'",(œcombo_mission[mission_field  Œwi 'value';1]),"'"),',')
t„t®(›'Area')    ,(›"'",(¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),','))
t„t®(›'Strata')  ,(›"'",(¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),','))
t„t®(›'Species') ,(combo_species['Form.lsSpecies' Œwi 'value';2])
t„t®(›'By Sex') ,(sexed)
t„t®(›'Distance'),(›'Form.edDistance' Œwi 'text')
t„t®(›'Spread')  ,(›'Form.cbSpread' Œwi 'text')
t„t®(›'Stratum Area Table'),(›'Form.cbStratumTable' Œwi 'text')
:IF 1='Form.opDFO' Œwi 'value'
  t„t®(›'Experiment Type') ,(›(¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),','))
:ELSE
  t„t®(›'Experiment Type <= '),(›'Form.edExpTypeNMFS' Œwi 'text')
:ENDIF
t„t®(›''),(›'') © add a blank row
:IF alk_mod_user='Y'
:ORIF alk_mod_plus = 'Y'
:ORIF alk_mod_sel = 'Y'
    t„t®(›'ALK Modifications'),(›'Yes')
    :IF ('Form.fAlk.ckXtype' Œwi 'value')=1
        t„t®(›'ALK Experiment Types'),(›'All')
    :ELSE 
        t„t®(›'ALK Experiment Types'),(›(¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),','))
    :ENDIF
    t„t®(›'ALK Strata'),(›"'",(¯1 ‡,("'",(œcombo_strata['Form.fAlk.lsAlkStrata' Œwi 'value';1]),"'"),','))
    t„t®(›'ALK Area')  ,(›"'",(¯1 ‡,("'",(œcombo_area['Form.fAlk.lsAlkUnit' Œwi 'value';1]),"'"),','))
    t„t®(›'ALK Table') ,(›('Form.fAlk.edAlkTable' Œwi 'text')) 
:ELSE
    t„t®(›'ALK Modifications'),(›'No')
:ENDIF

t„t®(›''),(›'') © add a blank row
:IF 1='Form.opNMFS' Œwi 'value' © only applies to US data
  :IF ('Form.ckVesselNetConversion' Œwi 'value') = 1 © is Vessel Net Conversion checked?
    :IF lfconv_applied = 'Y' 
      t„t®(›'Vessel Net Conversion'),(›'Yes')
    :ELSE
      t„t®(›'Vessel Net Conversion'),(›'Yes (but none applied to this data selection)')
    :ENDIF
  :ELSE
    t„t®(›'Vessel Net Conversion'),(›'No')
  :ENDIF
:ENDIF

© Query output    
output„›((›'QUERY')(t))

© Vessel Net Conversion
:IF ('Form.ckVesselNetConversion' Œwi 'value') = 1 © is Vessel Net Conversion checked?
  :IF lfconv_applied = 'Y' 
    output„(output),›((›'Vessel Net Conversion')  (((›'Vessel'),(›'Length'),(›'Vessel Conversion'))®((lfconv[;1 2 3]))))
  :ENDIF
:ENDIF

© Strata Area
output„(output),›((›'Strata Area')  (((›'STRATA'),(›'trawl units'),(›'sq nm'))®((data[;1 2]),nmsq)®((›'TOTAL'),(+/data[;2])(+/nmsq))))

© ALK, ALKPLUS, ALT output
:IF 'Y' = age_analysis

    :IF 0¬+/+/1œresults
        alk    „((›'LENGTHS'),(¯1‡age),(›'TOTAL'))®((lengths,(ZEROS_TO_NULLS 1œresults)),(+/1œresults))®((›'TOTAL'),(+š1œresults),(+/+/1œresults))
        output „(output),›((›'Age Length Key')(alk))
    :ENDIF

    :IF 0¬1†½detplus
        detplus_with_ages„(~(›Ð)­¨detplus[;4])šdetplus © compress for records with age
        :if 0¬1†½detplus_with_ages
          alkplus„MAKE_ALK detplus_with_ages[;2 3 4 5]
          alkplus „ ((›'LENGTHS'),(¯1‡age),(›'TOTAL'))®((lengths,(ZEROS_TO_NULLS alkplus)),(+/alkplus))®((›'TOTAL'),(+šalkplus),(+/+/alkplus))
          output„(output),›((›'Age Length Key Modifier')(alkplus))
        :endif
    :ENDIF
    
    alt„AGE_TABLE ¯1‡1œ2œresults © average lengths at age calculation 
    aln„œ(len×›[2]0 ¯1‡alt)
    :if sexed='Y'
        nrows„(1†½aln)÷3 © number of rows per sex
        alts„0 17½0
        :for I :in ¼3   © for each sex
            alt0„((I-1)×nrows,0)‡((nrows×I),17)†alt © select age length key for this sex
            aln0„((I-1)×nrows,0)‡((nrows×I),17)†aln © select age length key for the sex
            colsum„+šalt0   © column totals for this sex
            J„(colsum=0)/¼½colsum   © indexes of columns with total of zero
            colsum[J]„›Ð    © change zero totals  to null
            tmp„(+šaln0)÷colsum © average length of each column
            tmp[J]„0            © average length is zero for columns with total of zero
            alts„alts®tmp       © catenate results
        :endfor
        colsum„+š0 ¯1‡alt       © column totals for all sexes    
        J„(colsum=0)/¼½colsum   © indexes of columns with total of zero
        colsum[J]„›Ð            © change zero totals  to null     
        tmp„(+šaln)÷colsum      © average len of each column
        tmp[J]„0                © average length is zero for columns with total of zero
        alt„alt®(tmp,(›' '))             © catenate average length
        alt„alt®(alts,(›' '))            © catenate average length for each sex
        output „(output),›((›'Age Table')(((›' '),lengths,(›'Avg. Lgths'),(›'U Avg. Lgths'),(›'M Avg. Lgths'),(›'F Avg. Lgths')),(age®(ZEROS_TO_NULLS alt))))
    :else
        colsum„+š0 ¯1‡alt
        J„(colsum=0)/¼½colsum    ª colsum[J]„›Ð     
        tmp„(+šaln)÷colsum       ª tmp[J]„0 
        alt„alt®(tmp,(›' '))
        output „(output),›((›'Age Table')(((›' '),lengths,(›'Avg. Lgths')),(age®(ZEROS_TO_NULLS alt))))        
    :endif

©ALW output
    awtage„0 1‡(det®detplus)  © catenate and drop set id
    awtage„(~(›Ð)­¨awtage[;5])šawtage © compress for records with weight
    awtage„(~0=awtage[;5])šawtage © compress for records with weight>0
    :if 0¬1†½awtage      © If there is weight data...
        awtlen„awtage
        awtlen[;3]„0
        lw„MAKE_AWT awtlen © average weights at length
        lw„0.001×lw[;,1] © average weights at length
        :if 'Y' ¬ age_analysis
            output„(output),›((›'Length Weight')  (((›'LW'),(›'Avg. Wgt.'))®(lengths,lw)))
        :endif
        :if 'Y' = age_analysis
            awtage„(~(›Ð)­¨awtage[;3])šawtage © compress for records with age
            alw„MAKE_AWT awtage © average weights at length and age calculations
            :if sexed='Y'
                aln„(¯4 ¯1‡alt)×alw>0 © alt where value for average weight exists, first drop total rows and columns
                awt„alw×aln
                nrows„(1†½awt)÷3 © number of rows per sex
                alws„0 17½0
                :for I :in ¼3   © for each sex
                    awt0„((I-1)×nrows,0)‡((nrows×I),17)†awt © select age weight key for this sex
                    aln0„((I-1)×nrows,0)‡((nrows×I),17)†aln © select age length key for the sex
                    colsum„+šaln0   © column totals for this sex
                    J„(colsum=0)/¼½colsum   © indexes of columns with total of zero
                    colsum[J]„›Ð    © change zero totals  to null
                    tmp„(+šawt0)÷colsum © average weight of each column
                    tmp[J]„0    © average weight is zero for columns with total of zero
                    alws„alws®tmp © catenate results
                :endfor
                colsum„+šaln    © column totals for all sexes    
                J„(colsum=0)/¼½colsum   © indexes of columns with total of zero
                colsum[J]„›Ð    © change zero totals  to null
                tmp„(+šawt)÷colsum      © average weight of each column
                tmp[J]„0    © average weight is zero for columns with total of zero
                alw„0.001×alw®tmp   © age length weigh in thousands and catenate average weight
                alw„alw®(alws×.001)     © catenate average weight for each sex
                alw„alw,(lw®(4 1½›' '))     © append average weigth at length to age length weight
                output„(output),›((›'Age Length Weight')  (((›'ALW'),((¯1‡age),(›'Avg. Wgt.')))®((lengths,(›'Avg. Wgt.'),(›'U Avg. Wgt.'),(›'M Avg. Wgt.'),(›'F Avg. Wgt.')),(ZEROS_TO_NULLS alw))))
            :else
                aln„(¯1 ¯1‡alt)×alw>0 © alt where value for average weight exists, first drop total rows and columns
                awt„alw×aln
                colsum„+šaln
                J„(colsum=0)/¼½colsum    ª colsum[J]„›Ð     
                tmp„(+šawt)÷colsum       ªtmp[J]„0  ª  alw„0.001×alw®tmp
                alw„alw,(lw®(›' '))     © append average weigth at length to age length weight
                output„(output),›((›'Age Length Weight')  (((›'ALW'),((¯1‡age),(›'Avg. Wgt.')))®((lengths,(›'Avg. Wgt.')),(ZEROS_TO_NULLS alw))))
            :endif
        :endif        
    :endif
:ENDIF   

©LENGTH OUTPUT
:IF 'Y'=length_analysis
    output„(output),›((›'Length by Set')                (((›'STRATA'),(›'SLAT'),(›'SLONG'),(›'UNITAREA'),(›'SET'),lengths,(›'TOTAL'))®BY_SET 2œresults))
    output„(output),›((›'Length Mean')                  (((›'STRATA'),lengths,(›'TOTAL'))®(data[;1],•¨(œ(5œ2œresults)[;4]))®((›'TOTAL'),(3œ2œresults))))
    output„(output),›((›'Length Mean Standard Error')   (((›'STRATA'),lengths,(›'TOTAL'))®(data[;1],•¨(œ(5œ2œresults)[;5]*0.5))®((›'TOTAL'),(4œ2œresults)*0.5)))
    output„(output),›((›'Length Total')                 (((›'STRATA'),lengths,(›'TOTAL'))®(data[;1],•¨(œ(5œ2œresults)[;2]))®((›'TOTAL'),(1œ2œresults))))
    output„(output),›((›'Length Total Standard Error')  (((›'STRATA'),lengths,(›'TOTAL'))®(data[;1],•¨(œ(5œ2œresults)[;3]*0.5))®((›'TOTAL'),(2œ2œresults)*0.5)))   
:ENDIF

©AGE OUTPUT
:IF 'Y'=age_analysis
    output„(output),›((›'Age by Set')                   (((›'STRATA'),(›'SLAT'),(›'SLONG'),(›'UNITAREA'),(›'SET'),age,(›'TOTAL'))®BY_SET 3œresults))
    output„(output),›((›'Age Mean')                     (((›'STRATA'),age,(›'TOTAL'))®(data[;1],•¨(œ(5œ3œresults)[;4]))®((›'TOTAL'),(3œ3œresults))))
    output„(output),›((›'Age Mean Standard Error')      (((›'STRATA'),age,(›'TOTAL'))®(data[;1],•¨(œ(5œ3œresults)[;5]*0.5))®((›'TOTAL'),(4œ3œresults)*0.5)))
    output„(output),›((›'Age Total')                    (((›'STRATA'),age,(›'TOTAL'))®(data[;1],•¨(œ(5œ3œresults)[;2]))®((›'TOTAL'),(1œ3œresults))))
    output„(output),›((›'Age Total Standard Error')     (((›'STRATA'),age,(›'TOTAL'))®(data[;1],•¨(œ(5œ3œresults)[;3]*0.5))®((›'TOTAL'),(2œ3œresults)*0.5)))
:ENDIF

©WEIGHT OUTPUT
:IF ~(4œresults)­0½0
    output„(output),›((›'Weight by Set')                (((›'STRATA'),(›'SLAT'),(›'SLONG'),(›'UNITAREA'),(›'SET'),(›'TOTAL'))®BY_SET 4œresults))
    output„(output),›((›'Weight Mean')                  (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ4œresults)[;4]))®((›'TOTAL'),(3œ4œresults))))
    output„(output),›((›'Weight Mean Standard Error')   (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ4œresults)[;5]*0.5))®((›'TOTAL'),(4œ4œresults)*0.5)))
    output„(output),›((›'Weight Total')                 (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ4œresults)[;2]))®((›'TOTAL'),(1œ4œresults))))
    output„(output),›((›'Weight Total Standard Error')  (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ4œresults)[;3]*0.5))®((›'TOTAL'),(2œ4œresults)*0.5)))  
:ENDIF

©AREA OUTPUT
:IF ~(5œresults)­0½0

    output„(output),›((›'Prop Area')                  (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ5œresults)[;4]))®((›'TOTAL'),(3œ5œresults))))
    output„(output),›((›'Prop Area Standard Error')   (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ5œresults)[;5]*0.5))®((›'TOTAL'),(4œ5œresults)*0.5)))
    output„(output),›((›'Total Area')                 (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ5œresults)[;2]))®((›'TOTAL'),(1œ5œresults))))
    output„(output),›((›'Total Area Standard Error')  (((›'STRATA'),(›'TOTAL'))®(data[;1],[1.5]›[2]œ(•¨(5œ5œresults)[;3]*0.5))®((›'TOTAL'),(2œ5œresults)*0.5)))       
:ENDIF