

  itis.taxa.to.tsn = function( tx, itaxa=itis.db( "itaxa" ) ) {
    # merge taxa to itis using all possible avenues, including partial matches
    out = NA

    txs = unlist( strsplit( tolower( tx), "[[:space:]]+" ) )
    txs = txs[ which( nchar(txs)>1 ) ]
    if ( length(txs)==0 ) return( NA )

    e0 = 1:nrow( itaxa )

    if ( length(txs)==1 ) { # single noun ..
      # exact matches
      e1 = intersect( e0, which( tolower(itaxa$unit_name1) == txs ) ) # case insensitive
      if ( length(e1)==1 ) {
        out = e1
      } else {
        # there must be no subcategories as there is only a single noun
        e2 = which( itaxa$unit_name2 =="" ) 
        ex = intersect( e1, e2 )
        if ( length(ex)==1 ) {
          out = ex
        } else {
          tx.grep = paste("^", txs, sep="")
          e02 = intersect(e0, e2)
          # try partial matches .. ^ = make sure it begins with the same sequence
          o1 = grep( tx.grep, itaxa$unit_name1, ignore.case=T, perl=T ) 
          ox = intersect( e02, o1 )
          if (length(ox)==1 ) {
            out = ox
          } else { 
            # last try .. partial matches using agrep .. slow/expensive .. no regex
            p1 = agrep( txs, itaxa$unit_name1, ignore.case=T, max.distance =0.1 ) # perl=T to override multibyte characters  
            px = intersect( e02, p1 )
            if ( length(px)==1 ) {
              out = px
            } else { # return all possible matches and give up
              qx = grep( paste("\\<",txs[1], sep=""), itaxa$completename, ignore.case=T ) 
              if( length(qx)==1 ) {
                out=qx
              } else {
                out = c(ex, px, ox) 
              }
            }
          }
        }
      }
    } # end if single

    
    if (length(txs)==2) {
      # most likely a genus and species combination: 
      # first scan for exact matches
      e1 = which( tolower(itaxa$unit_name1) == txs[1] )  # case insensitive
      e2 = which( tolower(itaxa$unit_name2) == txs[2] )  # case insensitive
      ec = intersect( e1, e2 ) 
      if ( length(ec)==1 ) {
        
        out = ec

      } else {

        # there must be no subcategories as there is only a single noun
        ec2 = which( itaxa$unit_name3 =="" ) 
        ex = intersect( ec, ec2 )
        if ( length(ex)==1 ) {
          out = ex
        } else {
          tx.grep1 = paste("^", txs[1], sep="")
          tx.grep2 = paste("^", txs[2], sep="")
          e02 = intersect(e0, ec2)
          # try partial matches .. ^ = make sure it begins with the same sequence
          o1 = grep( tx.grep1, itaxa$unit_name1, ignore.case=T, perl=T ) 
          o2 = grep( tx.grep2, itaxa$unit_name2, ignore.case=T, perl=T ) 
          ox = intersect( o1, o2 )
          if (length(ox)==1 ) {
            out=ox
          } else {
            ox = intersect( e02, ox )
            if (length(ox)==1 ) {
              out = ox
            } else { 
              # last try .. partial matches using agrep .. slow/expensive no regex
              p1 = agrep( txs[1], itaxa$unit_name1, ignore.case=T, max.distance =0.1 ) 
              p2 = agrep( txs[2], itaxa$unit_name2, ignore.case=T, max.distance =0.1 )
              px = intersect( p1, p2 )
              if ( length(px)==1 ) {
                out = px
              } else { 
                px = intersect( e02, px )
                if( length(px)==1 ) {
                  out=px
                } else {
                  q1 = grep( paste("\\<",txs[1], sep=""), itaxa$completename, ignore.case=T ) 
                  q2 = grep( paste("\\<",txs[2], sep=""), itaxa$completename, ignore.case=T ) 
                  qx = intersect ( q1, q2 )
                  if( length(qx)==1 ) {
                    out=qx
                  } else {
                    # return all possible matches and give up
                    # include exact matches of species names as there may
                    # have been a genus name change
                    out = c(e1,e2, px, ox) 
                  }
                }
              }
            }
          }
        }
      }
    } # end if # txs=2
        
         
    if (length(txs)==3) {
      # most likely a genus, species and subspecies combination: 
      # first scan for exact matches
      e1 = which( tolower(itaxa$unit_name1) == txs[1] )  # case insensitive
      e2 = which( tolower(itaxa$unit_name2) == txs[2] )  # case insensitive
      ec = intersect( e1, e2 )
      if ( length(ec)==1 ) {
        # see if a genus-species combination is sufficient
        out = ec
      } else {
        e3 = which( tolower(itaxa$unit_name3) == txs[3] )  # case insensitive
        ec = intersect( ec, e3 )
        if ( length( ec )==1 ) {
          out = ec 
        } else {
          tx.grep1 = paste("^", txs[1], sep="")
          tx.grep2 = paste("^", txs[2], sep="")
          tx.grep3 = paste("^", txs[3], sep="")
          # try partial matches .. ^ = make sure it begins with the same sequence
          o1 = grep( tx.grep1, itaxa$unit_name1, ignore.case=T, perl=T ) 
          o2 = grep( tx.grep2, itaxa$unit_name2, ignore.case=T, perl=T ) 
          ox = intersect( o1, o2 )
          if (length(ox)==1 ) {
            out = ox
          } else {
            o3 = grep( tx.grep3, itaxa$unit_name3, ignore.case=T, perl=T ) 
            ox = intersect( ox, o3 )
            if (length(ox)==1 ) {
              out = ox
            } else { 
              # try .. partial matches using agrep .. slow/expensive .. no regex
              p1 = agrep( txs[1], itaxa$unit_name1, ignore.case=T, max.distance =0.1 ) 
              p2 = agrep( txs[2], itaxa$unit_name2, ignore.case=T, max.distance =0.1 )
              px = intersect( p1, p2 )
              if ( length(px)==1 ) {
                out = px
              } else { 
                p3 = agrep( txs[3], itaxa$unit_name3, ignore.case=T, max.distance =0.1 )
                px = intersect( px, p3 )
                if( length(px)==1 ) {
                  out=px
                } else {
                  #match beginning of each word in case there has been a name change .. the root usually stays the same
                  q1 = grep( paste("\\<",txs[1], sep=""), itaxa$completename, ignore.case=T ) 
                  q2 = grep( paste("\\<",txs[2], sep=""), itaxa$completename, ignore.case=T ) 
                  q3 = grep( paste("\\<",txs[3], sep=""), itaxa$completename, ignore.case=T ) 
                  qx = intersect ( q1, q2 )
                  qx = intersect ( qx, q3 )
                  if( length(qx)==1 ) {
                    out=qx
                  } else {
                    # return all possible matches and give up
                    # include exact matches of species names as there may
                    # have been a genus name change
                    out = c(e1, e2, e3, px, ox) 
                  }
                }
              }
            }
          }
        }
      }
    } # end if # txs=3

    # out contains candidate row indices of itaxa
    # now refine search using 
    res = itis.refine.search( out, itaxa )
    
    if (is.data.frame( res) ) {
      res = res$tsn
    } else {
      res = NA
    }

    return (res)

  }


