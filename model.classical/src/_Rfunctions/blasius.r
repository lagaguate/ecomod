  
  blasius = function(ti, S, P) {
    # Blasius, B., Huppert, A., and Stone, L. (1999). Complex dynamics and phase 
    # synchronization in spatially extended ecological systems. Nature, 399:354-359. 
    # Blasius, B. and Stone, L. (2000). Chaos and phase synchronization in 
    # ecological systems. International Journal of Bifurcation and Chaos, 10:2361-2380.

    # u = resource
    # h = herbivore
    # w = carnivore

    # du/dt =  a*u - alpha1 * f1(u,v)
    # dv/dt = -b*v + alpha1 * f1(u,v) - alpha2 * f2(v,w)
    # dw/dt = -c(w-w*) - alpha2 * f2(v,w)

    # where f1(x,y) and f2(x,y) 
    #   = x*y in the Lotka-Voltera case
    #   = x*y / (1+k_i*x) in Holling type II functional relationship
    # and where w* = min predator level when the prey population is low 
    # (this stabilises the system)

    u = S[1]; v = S[2]; w = S[3]
    
    f = holling.functional.response( type=2 ) 

    out = with( P, {
      du = a * u - alpha1 * f(u, v, k1)
      dv = -b * v + alpha1 * f(u, v, k1) - alpha2 * f(v, w, k2)
      dw = -c * (w - wstar) + alpha2 * f(v, w, k2)
      list( c(du, dv, dw) )
    })
  } 

