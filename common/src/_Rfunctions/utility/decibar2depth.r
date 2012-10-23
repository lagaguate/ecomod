

      decibar2depth = function( P, lat, Del=0 ) {
        # http://www.code10.info/index.php?option=com_content&view=article&id=67:calculating-the-depth-from-pressure&catid=54:cat_coding_algorithms_seawater&Itemid=79
        # DEPTH IN METERS FROM PRESSURE IN DECIBARS USING SAUNDERS AND FOFONOFF'S METHOD.
        # DEEP-SEA RES., 1976, 23, 109-111.
        # / UNITS:
        # //   PRESSURE     P      DECIBARS
        # //   LATITUDE     LAT    DEGREES
        # //   DEPTH        DEPTH  METERS
        # //   DTN. HEIGHT  DEL    DYN. METERS (geopotenial anomaly in J/kg:: assume 0) 
        # // CHECKVALUE:
        # //   1.) DEPTH = 9712.653 M for P=10000 DECIBARS, LAT=30 DEG, DEL=0
        # //   ABOVE FOR STANDARD OCEAN: T=0 DEG CELSIUS; S=35 (PSS-78)
        # // ----------------------------------------------------------
        # // Original fortran code is found in:
        # //   UNESCO technical papers in marine science 44 (1983) -
        # //   'Algorithms for computation of fundamental properties of seawater'
        
        X = sin( lat * pi / 180 ) # convert degree decimal to RADs
        X = X^2

        # GR=GRAVITY VARIATION WITH LATITUDE: ANON (1970) BULLETIN GEODESIQUE
        GR = 9.780318 * (1.0 + (5.2788E-3 + 2.36E-5 * X) * X) + 1.092E-6 * P
        DepthTerm = (((-1.82E-15 * P + 2.279E-10) * P - 2.2512E-5) * P + 9.72659) * P ## assuming (35 psu, 0 C, and P=pressure in decibars )
        DEPTH = DepthTerm / GR + Del / 9.8
        return (DEPTH)
      }


