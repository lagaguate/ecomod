  # general setup of GMT output config

  gmt.prep = function(fnt="18p", psres=300) {
    cmd( "gmtset ANOT_FONT Helvetica")
      # other good: Times-Roman Palatino-Roman Helvetica-Narrow
      #             NewCenturySchlbk-Roman Courier
    cmd( "gmtset ANOT_FONT_SIZE", fnt)
    cmd( "gmtset LABEL_FONT_SIZE ", fnt)
    cmd( "gmtset DOTS_PR_INCH", psres)
    cmd( "gmtset PAPER_MEDIA letter")
    cmd( "gmtset PAGE_ORIENTATION portrait")
    cmd( "gmtset PSIMAGE_FORMAT bin") # binaries are smaller
    cmd( "gmtset PLOT_DEGREE_FORMAT dddF") # add degree notation in range -180 to +180,
    cmd( "gmtset BASEMAP_TYPE fancy") # the + adds rounded corners
    cmd( "gmtset ELLIPSOID WGS-84")
    cmd( "gmtset FRAME_WIDTH 1p") #map frame
    cmd( "gmtset FRAME_PEN 1p")
    cmd( "gmtset CHAR_ENCODING = ISOLatin1+")
    return (NULL)
  }


