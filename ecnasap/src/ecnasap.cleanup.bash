
# inf
gunzip inf.dat.gz 
gawk 'BEGIN {RS="\n"; FS=";"; OFS=";"}  \
      NR>2 && !/SETID/ && !/REMARK/ && !/----/ && !/SQL/ && !/select/ \
      {gsub(/[ ]/,""); print} ' inf.dat > inf.out
gzip inf.dat


#  cat.dat 
gunzip cat.dat.gz
gawk 'BEGIN {RS="\n"; FS=";"; OFS=";"} \
      NF>2 && NR>2 && !/SETID/ && !/----/ && !/SQL/ && !/select/ \
      {gsub(/[ ]/,""); print $1, $2, $3}'  cat.dat > cat.out
gzip cat.dat


#  spec.dat 
gunzip spec.dat.gz
gawk 'BEGIN {RS="\n"; FS=";"; OFS=";"} \
      NF>3 && NR>2 && !/SPID/ && !/----/ && !/SQL/ && !/select/ \
      {print $1, $2, $3, $4}'  spec.dat > spec.out
gzip spec.dat



