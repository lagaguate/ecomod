
# To examine the effect of relative changes in abundance and spatial clusters upon variogram params
#

ncol = 400
nrow = 300

nlocs = 5000

abund = runif( nlocs)
pos = data.frame( cbind( x = floor( runif( nlocs*2, min=1, max=ncol+1 ) ),
												 y = floor( runif( nlocs*2, min=1, max=nrow+1 ) ) ))

o = which (duplicated( pos) )
if (length(o) > 0 ) pos = pos[ -o,] 
pos = pos[sample.int( nlocs, replace=F ),]

plot (x,y)

dat = pos
dat$ab = abund *100

require( geoR )

g = as.geodata( dat )
v = variog(g, estimator.type= "classical")
plot( v )
Vvar = var(dat$ab)
D0 = floor( (ncol + nrow ) / 2 / 10 )



nclusters = 5
drange = 10  # drange for clusters


x = variofit( v, cov.model="exponential", ini.cov.pars = c(Vvar/2, D0), nugget=Vvar/2, weights="npairs")
x



