R�BY_SET DATA;I;TEMP
� DATA <=> element 2, 3 or 4 of results after analysis
� e.g. DATA is 2�results1999

DATA�5�DATA
TEMP�BY_ROW �DATA[;6]
R� (0,4+��0 1����TEMP) �0
:for I :in ��TEMP
    :if ~Э�I�TEMP
        R�R�((�I�((DATA)[;1])),(�data[I;3])[;3 4 5],I�TEMP)
    :endif
:endfor