gawk 'BEGIN {RS="\n"; FS=" "; OFS=" "} \
      NF=3 {print }' nwa.chs15sec.xyz > tmp.xyz


