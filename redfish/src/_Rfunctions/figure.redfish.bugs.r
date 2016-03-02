figure.redfish.bugs <- function(y,sb =sb, DS) {

if(DS=='biomass.ts') {

		        SI1 =  apply( y$q1, 1, mean, na.rm=T  )
            SI2 =  apply( y$q2, 1, mean, na.rm=T  )
         yrs = sb$yr
          qIOA = sb$I1 / SI1
			qIOA1 = sb$I2 / SI2
          
          meanval = apply( y$B, 1, mean, na.rm=T  )

          prs = seq( from=0.025, to=0.975, length.out=600)
          Bq =  apply( y$B, 1, quantile, probs=prs, na.rm=T  )

          yran = range(c(0, Bq, sb$I1,sb$I2 ), na.rm=T )*1.01
          plot( yrs, Bq[1,], type="n", ylim=yran, xlab="", ylab=""  )
          cols = gray.colors( floor(length( prs)/2) )
          cols2 = c(cols[length(cols):1], cols )
          for ( j in 1:length(prs) ) {
            lines ( yrs, Bq[j,], lwd=4, col=cols2[j] )
          }
          # lines( yrs, B, lwd=3, col="darkgreen" )
          title( ylab="Fishable biomass (kt)" ) 
          title( xlab="Year" ) 
          points( yrs, qIOA, pch=20, col="darkgreen" )
          lines ( yrs, qIOA, lwd=1, col="darkgreen", lty="dashed" )
          points( yrs[21:length(yrs)], qIOA1, pch=20, col="darkred" )
          lines ( yrs[21:length(yrs)], qIOA1, lwd=1, col="darkred", lty="dashed" )
          
          lines ( yrs, meanval, lwd=2, col="blue", lty="dotted" )
          qs = apply( y$K/2, 1, mean )
          abline(h = qs*0.4,col='purple')
		  abline(h = qs*0.8,col='purple')

          
          	}
       if ( DS=="FMSY" ) {
        qs = apply( y$r/2, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$r)
          prr=NULL
          prr$class="none"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }

if ( DS=="r" ) {
        qs = apply( y$r/2, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$r)
          prr=NULL
          prr$class="none"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }
    	

if ( DS=="q" ) {
        qs = apply( y$q1, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$q1)
          prr=c()
          prr$max=sb$q.b
          prr$min=sb$q.a
          
          prr$class="uniform"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }

if ( DS=="BMSY" ) {
        qs = apply( y$K/2, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$K)
          prr=NULL
           plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }

if ( DS=="K" ) {
        qs = apply( y$K, 1, quantile, probs=c(0.025, 0.5, 0.975) )
        qs = signif( qs, 3 )
          pdat = as.vector(y$K)
          prr=NULL
          prr$meanlog = sb$K$xbar
          prr$sdlog = sqrt(sb$K$sig2)
          
          prr$class="lognormal"
          plot.freq.distribution.prior.posterior( prior=prr, posterior=pdat )
      }

}