EXEC;check;lengths;infquery;catquery;detquery;lfquery;alkquery;lgrp;length_analysis;age_analysis;sexed;wing;distance;indx;len_range;output;cat;alk;lf;ALL_LEN;data;results;mission_field;num_of_missions;I;inf;nmsq;alkmodtable;alkmoddet;alkdetplus;palk;Z2MIN;alkmodquery;strata;alk_mod_plus;alk_mod_sel;alk_mod_user;alkplusquery;det;detplus
�� Execute functions to process data based on forms selection criteria.
�
� Called by the Execute button on the main form.

num_of_missions��1+���'Form.s1' �wi 'children' � Determine the number of missions selected.

:FOR I :IN �num_of_missions     � For each mission 
    mission_field�('Form.s1.pg',(�I),'.lsMission')     � Detemine location of the mission field on the form.
    data�catquery�infquery�alkquery�lfquery��    � Initialize arrays

    'Form' �wi 'pointer' 11 � Hourglass pointer. 

    check�ENTRY_CHECK mission_field
    :if check � � ERR � :endif

    :if 1='Form.opDFO'  �wi 'value'� check�BUILD_DFO_QUERY  mission_field � :endif
    :if check � � ERR � :endif
    
    :if 1='Form.opNMFS' �wi 'value'� check�BUILD_NMFS_QUERY mission_field � :endif
    :if check � � ERR � :endif

    check�GET_DATA (infquery) (catquery) (lfquery) (alkquery)
    :if check � � ERR � :endif

    :IF ~cat��      � If there is catch data...

        data�SETUP (inf) (cat) (lf) � Convert the raw data into a usable format.
        nmsq�data[;2] � nautical miles squared (added for area occupied)
        data[;2]�((data[;2])�(wing�6080.2)�distance) � Convert area to Number of Units in each stratum
    
        �Find the len_range from all available sources. 
        :IF 'Y' = length_analysis
          len_range�(�/lf[;3]),�/lf[;3]
        
          :IF ~Эdetplus
              len_range�(�/len_range,detplus[;3]),�/len_range,detplus[;3]
          :ENDIF

          �Get the lengths
          lengths� lgrp MAKE�LEN len_range
        :ELSE
          lengths��
        :ENDIF
        results� STRATIFIED data    �Return results from prepared data
    
        � call AREA function and append return info to results
        results�results, �AREA � added for area occupied
        
        output�lengths CREATE_OUTPUT results    � Prepare results for output to Excel
        XL_INIT output  � Send prepared output to Excel
    
        � Show Excel.
        :IF 1=(0��'xl' �wi 'self')
            'xl' �wi 'xvisible' 1
        :ENDIF
    :ELSE
        POPUP 'No Data from Query.'
        �ERR
    :ENDIF

:ENDFOR

ERR:
�Finish the Progress bar and remove it from sight.
'Form.pbProgress' �wi 'Stepit'
'Form.pbProgress' �wi 'visible' 0
'Form.lProgress' �wi 'visible' 0
'Form' �wi 'pointer' 0