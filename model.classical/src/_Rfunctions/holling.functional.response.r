
  holling.functional.response = function( type=2 ) {
    
    if (type==2) out = function(X,Y,K) { X * Y / (1+K*X) }  
  
    return (out)
  }  

