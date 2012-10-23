
  NEA_structure = function(F) {
    # This subfunction calculates several statistics that describe the network
    # structure of the system.
    n = nrow(F)
    I = diag(n)
    A = sign(F)           # nxn adjacency matrix
    A1 = A + I			          # nxn adjacency walk matrix
    L = length(which(A !=0 ) )      # number of links or arcs in the network
    connectance = L/(n^2)             # network connectance
    Ln = L/n                # link density
    max_eig = max( abs( Mod( eigen(A)$values) ) )
# dominant eigenvalue of A = rate of pathway proliferation.  
# This can serve as a complexity index
    structure_ep = list(link.number=L, connectance=connectance, link.density=Ln, max.eigenvalue=max_eig) 
    return (structure_ep)
  }


