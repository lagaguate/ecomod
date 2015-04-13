
bottom.contact.plot = function ( O ) {
  
  if ( exists("error.flag", O) && !is.na( O$error.flag ) )  {
    x = O$plotdata
    trange = range( x$ts, na.rm=TRUE )
    drange = range( x$depth, na.rm=TRUE) 
    plot(depth~ts, x, ylim=c(drange[2] +2.5, drange[1] -5), xlim=c(trange[1]-20,trange[2]+20), type="p", pch=".", col="black", cex=2 )
    title( sub=paste(O$id, O$error.flag) ) 
    return() 
  }

  x = O$plotdata
   # browser()

  # all data within range of gated limits
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = range( x$depth[O$good], na.rm=TRUE) 
    plot(depth~ts, x, ylim=c(drange[2] +2.5, drange[1] -5), xlim=c(trange[1],trange[2]), type="n", xlab="" )
    legendtext = NULL
    legendcol = NULL
    legendpch = NULL
    points( depth~ts, x[O$good,], pch=21, col="slategray", cex=0.5)
    points( depth~ts, x[!O$good,], pch=21, col="red", cex=0.4)


    if (all(is.finite( O$variance.method) ) ) {
        mcol = "gray"
        # points( depth~ts, x[ O$variance.method.indices, ], pch=20, col=mcol, cex=0.2)
        abline (v=x$ts[min(O$variance.method.indices)], col=mcol, lty="solid", lwd=1.2)
        abline (v=x$ts[max(O$variance.method.indices)], col=mcol, lty="solid", lwd=1.2)
        duration = as.numeric( difftime( O$variance.method[2], O$variance.method[1], units="mins" ) )
        legendtext = c( legendtext, paste( "variance gate:   ", round( duration, 2), "" ) )
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20 ) 
    }
    

    if (all(is.finite( O$modal.method) ) ) {
        mcol = "red" # colour for plotting
        # points( depth~ts, x[O$modal.method.indices,], col=mcol, pch=20, cex=0.2)       
        abline (v=x$ts[min(O$modal.method.indices)], col=mcol, lty="dashed")
        abline (v=x$ts[max(O$modal.method.indices)], col=mcol, lty="dashed")
        duration = as.numeric( difftime( O$modal.method[2], O$modal.method[1], units="mins" ) )
        legendtext = c( legendtext, paste( "modal:   ", round( duration, 2) ) )
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20) 
    }

    
    if ( all(is.finite(O$smooth.method) ) ) {
          mcol = "blue"
          # points( depth~ts, x[O$smooth.method.indices,], col=mcol, pch=20, cex=0.2)   
          abline (v=x$ts[min(O$smooth.method.indices)], col=mcol, lty="dotdash", lwd=1.5)
          abline (v=x$ts[max(O$smooth.method.indices)], col=mcol, lty="dotdash", lwd=1.5)
          duration = as.numeric( difftime( O$smooth.method[2], O$smooth.method[1], units="mins" ) )
          legendtext = c(legendtext, paste( "smooth:   ", round(duration, 2)) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
    }
  
    if (all(is.finite(O$linear.method)) ) {
          mcol ="green"
          # points( depth~ts, x[O$linear.method.indices,], col=mcol, pch=20, cex=0.2)      
          abline (v=x$ts[min(O$linear.method.indices)], col=mcol, lty="twodash")
          abline (v=x$ts[max(O$linear.method.indices)], col=mcol, lty="twodash")
          duration = as.numeric( difftime( O$linear.method[2], O$linear.method[1], units="mins" ) )
          legendtext = c( legendtext, paste( "linear: ", round( duration, 2) ) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
     }


    if (all(is.finite(O$maxdepth.method)) ) {
      mcol ="orange"
          # points( depth~ts, x[O$linear.method.indices,], col=mcol, pch=20, cex=0.2)      
          abline (v=x$ts[min(O$maxdepth.method.indices)], col=mcol, lty="solid")
          abline (v=x$ts[max(O$maxdepth.method.indices)], col=mcol, lty="solid")
          duration = as.numeric( difftime( O$maxdepth.method[2], O$maxdepth.method[1], units="mins" ) )
          legendtext = c( legendtext, paste( "maxdepth: ", round( duration, 2) ) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
     }

    if (all(is.finite( c(O$bottom0, O$bottom1) )) ) {
      mcol ="purple"
          abline (v=x$ts[min(O$bottom.contact)], col=mcol, lty="dotted")
          abline (v=x$ts[max(O$bottom.contact)], col=mcol, lty="dotted")
          duration = as.numeric( difftime( O$bottom1, O$bottom0, units="mins" ) )
          legendtext = c( legendtext, paste( "Trimmed mean: ", round( duration, 2) ) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
     }


     if (all(is.finite(O$manual.method)) ) {
        mcol ="cyan"
        points( depth~ts, x[O$manual.method.indices,], col=mcol, pch=20, cex=0.2)      
        abline (v=x$ts[min(O$manual.method.indices)], col=mcol, lty="dotdash")
        abline (v=x$ts[max(O$manual.method.indices)], col=mcol, lty="dotdash")
        tdif = abs( as.numeric(difftime(O$manual.method, units="mins")) )
        tdiffman = round( tdif, 2)
        legendtext = c( legendtext, paste( "manual: ", tdiffman  ) ) 
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20) 
     }
   
    if ( !( is.null( legendtext)))  legend( "top", legend=legendtext, col=legendcol, pch=legendpch )
     
    #x11(); plot( slopes ~ ts, x2 )
    lines( O$depth.smoothed ~ x$ts, col="brown" )
    # points( depth0~ts, x[!O$good,], col="red", cex=1 )   ## points dropped from filters
    
    title( sub=paste( O$id,  
      " :: time mean=", signif( as.numeric(O$bottom.diff)/60, 3), 
      "; time sd=", signif( as.numeric(O$bottom.diff.sd)/60, 3) , 
      "; n=", O$depth.n,  
      ";", "\n", " depth mean=", signif( O$depth.mean, 3), 
      "; depth sd=", signif( O$depth.sd, 3), 
      "; signal=", signif(O$signal2noise, 3) , sep="")  )

}


