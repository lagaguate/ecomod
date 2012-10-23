gawk 'BEGIN {RS="\n"; FS=" "; OFS=" "} \
      $1 <= -68 && $1>=-56 && $2<=48 && $2>= 41 {print}' AtlanticBathy15sec_v1.dat > scotianshelf.15sec
