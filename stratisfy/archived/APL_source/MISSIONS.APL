MISSIONS;page;list;tmp;btn
��Mission page management.
� Called by Selection Form

:if    (���'Form.s1' �wi 'children')=(���10�10��wi 'self') �If this is the last page..
:andif ~Э �wi 'value'  �.. and selections were made ..
    tmp�(page�'Form.s1.pg',�1+�'Form.s1' �wi 'children') �wi 'Create' 'Page' �Create a new page
    page �wi 'caption' ('Survey',2�page �wi 'name') �Name the page Survey#
    tmp�(list�page,'.lsMission') �wi 'Create' 'List'    � Create a new list for the page
    list �wi 'where' 0.2 0 10.3 20  � Place the list on the page.
    :if 1= 'Form.opDFO' �wi 'value'
        list �wi 'style' 145    �Give the list the appropriate style properties for DFO
    :elseif 1= 'Form.opNMFS' �wi 'value'
        list �wi 'style' 144    �Give the list the appropriate style properties for NMFS
    :endif
    list �wi 'list' (combo_mission[;1]) �Populate list
    list �wi 'onClick' 'MISSIONS'   �Give the page the onClick property to call this function.
:elseif (���1+�'Form.s1' �wi 'children')=(���10�10��wi 'self') � or if the page is the second to last..
:andif Э �wi 'value'   � and the selection has been cleared
    (page�'Form.s1.pg',��'Form.s1' �wi 'children') �wi 'Delete'     �Delete the last page.
:endif