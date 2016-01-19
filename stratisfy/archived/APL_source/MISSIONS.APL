MISSIONS;page;list;tmp;btn
©’Mission page management.
© Called by Selection Form

:if    (–•½'Form.s1' Œwi 'children')=(–•¯10‡10‡Œwi 'self') ©If this is the last page..
:andif ~Ð­ Œwi 'value'  ©.. and selections were made ..
    tmp„(page„'Form.s1.pg',•1+½'Form.s1' Œwi 'children') Œwi 'Create' 'Page' ©Create a new page
    page Œwi 'caption' ('Survey',2‡page Œwi 'name') ©Name the page Survey#
    tmp„(list„page,'.lsMission') Œwi 'Create' 'List'    © Create a new list for the page
    list Œwi 'where' 0.2 0 10.3 20  © Place the list on the page.
    :if 1= 'Form.opDFO' Œwi 'value'
        list Œwi 'style' 145    ©Give the list the appropriate style properties for DFO
    :elseif 1= 'Form.opNMFS' Œwi 'value'
        list Œwi 'style' 144    ©Give the list the appropriate style properties for NMFS
    :endif
    list Œwi 'list' (combo_mission[;1]) ©Populate list
    list Œwi 'onClick' 'MISSIONS'   ©Give the page the onClick property to call this function.
:elseif (–•¯1+½'Form.s1' Œwi 'children')=(–•¯10‡10‡Œwi 'self') © or if the page is the second to last..
:andif Ð­ Œwi 'value'   © and the selection has been cleared
    (page„'Form.s1.pg',•½'Form.s1' Œwi 'children') Œwi 'Delete'     ©Delete the last page.
:endif