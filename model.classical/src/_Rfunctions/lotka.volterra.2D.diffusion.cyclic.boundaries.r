

 ## =======================================================================
 ## An example with a cyclic boundary condition.
 ## Diffusion in 2-D; extra flux on 2 boundaries,
 ## cyclic boundary in y
 ## =======================================================================
lotka.volterra.2D.diffusion.cyclic.boundaries = function(ti, S, P) {
    y    <- matrix(nr=nx, nc=ny, data=S)  # vector to 2-D matrix
    dS   <- -r*y        # consumption
    BNDx   <- rep(1,nx)   # boundary concentration
    BNDy   <- rep(1,ny)   # boundary concentration

    #diffusion in X-direction; boundaries=imposed concentration
    Flux <- -Dx * rbind(y[1,]-BNDy,(y[2:nx,]-y[1:(nx-1),]),BNDy-y[nx,])/dx
    dS   <- dS - (Flux[2:(nx+1),]-Flux[1:nx,])/dx

    #diffusion in Y-direction
    Flux <- -Dy * cbind(y[,1]-BNDx,(y[,2:ny]-y[,1:(ny-1)]),BNDx-y[,ny])/dy
    dS    <- dS - (Flux[,2:(ny+1)]-Flux[,1:ny])/dy

    # extra flux on two sides
    dS[,1] <- dS[,1]+  10
    dS[1,] <- dS[1,]+  10

    # and exchange between sides on y-direction
    dS[,ny] <- dS[,ny]+ (y[,1]-y[,ny])*10
    list(as.vector(dS))
 }

