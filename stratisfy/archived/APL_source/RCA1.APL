r„RCA1 arg;a;i;row;col;alpha

© Convert from RC notation to A1 notaton

  r„Ğ
  i„1 2
  alpha„' ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  
© Offset the range by the first two elements
© Instead of cutting the range by the first two elements.  
 
 :IF 4=½arg ª arg[3 4]„ (arg[3 4]+arg[1 2])-1 ª :ENDIF
 

 start:
   (row col)„arg[i]
    r„r,alpha[1+27 27‚col+˜col÷26+100×Œct]
    r„r,•row
 :IF 4=½arg ª r„r,':' ª arg„2‡arg ª …start ª :ENDIF
 
 r„r~' '