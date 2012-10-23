
params.time = function(P, runtype="stochastic", simulation.stepsize=1, nYears=10) {
    P$runtype =runtype
    P$lowerLimit = 1e-6  # the lower limit  of N before a "species" is considered extinct/exterpated
    P$simulation.stepsize = simulation.stepsize # fraction of a year
    P$nYears = nYears
    P$timestep = floor(P$simulation.stepsize * 365)
    return(P)
}


