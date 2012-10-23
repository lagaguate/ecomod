
# ----------------------------------------------------------------------------
# do a length-weight regression analysis for each species

  # the commented-out section does the analysis manually
  # .. in case more control is needed
  # .. currently, it is not used:
  #
  # obtain standardised residuals from the "hat" matrix
  # ... see a book on regression analysis for more details
  # ... no longer used ... there is a default function "rstandard" that does this
  #
  #resid.std <- function(lm.object)
  #				{ w = weights(lm.object)
  #		  		r = residuals(lm.object)
  #		  		r.w = (sqrt(w) * r)
  #		  		s = sqrt(deviance(lm.object)/df.residual(lm.object))
  #          hii = lm.influence(lm.object,do.coef=F)$hat
  #		  		rs = r.w / (s*sqrt(1-hii))
  #				}
  #
  # obtain standardised residuals from the "hat" matrix
  # ... there is a default function "rstandard" that does this but some cases seem to be dropped by it
  
#    resid.std <- function(lm) { 
#    	rstd = (sqrt(weights(lm)) * residuals(lm)) / (sqrt(deviance(lm)/df.residual(lm)) 
#    				* sqrt(1-lm.influence(lm, do.coef=F, drop0=F )$hat) )
#			return(rstd)
#		}

  lm.resid = function ( x, threshold=0.75 ) {
    
    x$residual = NA
    x$predicted.mass = NA

    male=1
    female=2
    nosex=3
    x$sex[ ! which( x$sex %in% c(male, female) ) ] = nosex
    
    species = sort( unique( as.numeric(as.character(x$spec ) )))
    lm.summ=NULL

    for (i in species) { 
      print( i )
      for (sx in c(male,female, nosex)) {
        w = which( x$spec==i & x$sex==sx)
        
        # remove extremes from the data to generate regressions
          ql = quantile( x$len[w], probs=c(0.01, 0.99), na.rm=T )
          qm = quantile( x$mass[w], probs=c(0.01, 0.99), na.rm=T )
          is =  which( x$len> ql[1] & x$len< ql[2] & x$mass> qm[1] & x$mass< qm[2] ) 
        w = unique( intersect( w, is ) )
        nw = length(w)

        if ( nw > 10 ) { 
          q = x[w ,]
          q.lm = lm( mass~len, data=q )
          s = summary( q.lm )
          lm.summ = rbind( 
            lm.summ, data.frame(
              cbind ( 
                spec=i, sex=sx, rsquared=s$r.squared, sigma=s$sigma, df=s$df[2],
                b0=s$coefficients[1], b1=s$coefficients[2] 
          ) ) )
          if ( is.finite(s$r.squared ) && ( s$r.squared > threshold )) {
            x$residual[w] = rstandard(q.lm) 
            x$predicted.mass[w] = predict( q.lm, newdata=q ) 
          }
        }
      }
    }

 
    # remove poor data
    lm.summ = as.data.frame(lm.summ)
    specieslist = lm.summ$spec[ !is.finite(lm.summ$rsquared) ]
    x$residual [ x$spec %in% specieslist ] = NA
    x$predicted.mass [ x$spec %in% specieslist ] = NA

    x$residual [ abs( x$residual ) > 4 ] = NA
    x$predicted.mass [ abs( x$residual ) > 4 ] = NA

    return (list(residual=x$residual, predicted.mass=x$predicted.mass, lm.summ=lm.summ))
  }


