
bottom.contact.plot = function ( O ) {

  x = O$plotdata
   # browser()

  # all data within range of gated limits
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    legendtext = NULL
    legendcol = NULL
    legendpch = NULL
    mcol = "steelblue"
    points( depth~ts, x[O$good,], pch=20, col=mcol, cex=0.2)


    if (all(is.finite( O$variance.method) ) ) {
        mcol = "gray"
        points( depth~ts, x[ O$variance.method.indices, ], pch=20, col=mcol, cex=0.2)
        abline (v=x$ts[min(O$variance.method.indices)], col=mcol, lty="dotted")
        abline (v=x$ts[max(O$variance.method.indices)], col=mcol, lty="dotted")
        duration = as.numeric( difftime( O$variance.method[2], O$variance.method[1], units="mins" ) )
        legendtext = c( legendtext, paste( "variance:   ", round( duration, 2) ) )
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20 ) 
    }
    

    if (all(is.finite( O$modal.method) ) ) {
        mcol = "red" # colour for plotting
        points( depth~ts, x[O$modal.method.indices,], col=mcol, pch=20, cex=0.2)       
        abline (v=x$ts[min(O$modal.method.indices)], col=mcol, lty="dashed")
        abline (v=x$ts[max(O$modal.method.indices)], col=mcol, lty="dashed")
        duration = as.numeric( difftime( O$modal.method[2], O$modal.method[1], units="mins" ) )
        legendtext = c( legendtext, paste( "modal:   ", round( duration, 2) ) )
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20) 
    }

    
    if ( all(is.finite(O$smooth.method) ) ) {
          mcol = "blue"
          points( depth~ts, x[O$smooth.method.indices,], col=mcol, pch=20, cex=0.2)   
          abline (v=x$ts[min(O$smooth.method.indices)], col=mcol, lty="dashed")
          abline (v=x$ts[max(O$smooth.method.indices)], col=mcol, lty="dashed")
          duration = as.numeric( difftime( O$smooth.method[2], O$smooth.method[1], units="mins" ) )
          legendtext = c(legendtext, paste( "smooth:   ", round(duration, 2)) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
    }
   
    if ( all(is.finite(O$incremental.method) ) ) {
          mcol = "yellow"
          points( depth~ts, x[O$incremental.method.indices,], col=mcol, pch=20, cex=0.2)   
          abline (v=x$ts[min(O$incremental.method.indices)], col=mcol, lty="dotted")
          abline (v=x$ts[max(O$incremental.method.indices)], col=mcol, lty="dotted")
          duration = as.numeric( difftime( O$incremental.method[2], O$incremental.method[1], units="mins" ) )
          legendtext = c(legendtext, paste( "incremental:   ", round(duration, 2)) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
    }
  

    if ( all(is.finite(O$intersect.method) ) ) {
          mcol = "magenta"
          points( depth~ts, x[O$intersect.method.indices,], col=mcol, pch=20, cex=0.2)   
          abline (v=x$ts[min(O$intersect.method.indices)], col=mcol, lty="dashed")
          abline (v=x$ts[max(O$intersect.method.indices)], col=mcol, lty="dashed")
          duration = as.numeric( difftime( O$intersect.method[2], O$intersect.method[1], units="mins" ) )
          legendtext = c(legendtext, paste( "intersect:   ", round(duration, 2)) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
    }

      if (all(is.finite(O$linear.method)) ) {
          mcol ="green"
          points( depth~ts, x[O$linear.method.indices,], col=mcol, pch=20, cex=0.2)      
          abline (v=x$ts[min(O$linear.method.indices)], col=mcol)
          abline (v=x$ts[max(O$linear.method.indices)], col=mcol)
          duration = as.numeric( difftime( O$linear.method[2], O$linear.method[1], units="mins" ) )
          legendtext = c( legendtext, paste( "linear: ", round( duration, 2) ) )
          legendcol = c( legendcol, mcol)
          legendpch =c( legendpch, 20) 
      }

     if (all(is.finite(O$manual.method)) ) {
        mcol ="cyan"
        points( depth~ts, x[O$manual.method.indices,], col=mcol, pch=20, cex=0.2)      
        abline (v=x$ts[min(O$manual.method.indices)], col=mcol)
        abline (v=x$ts[max(O$manual.method.indices)], col=mcol)
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
    
 
}


