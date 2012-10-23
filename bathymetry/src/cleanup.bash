gawk 'BEGIN {RS="\n"; FS=" "; OFS=" "} \
      NF==3 \
      {print $2, $3, $5}' tmp.nei > nwa.out 


# remove the first line (the header) and quotations and dos carriage returns
sed -e '1d' -e 's/\"//g' -e 's/\r//g' main.tmp > main.csv


