
# R version of matlab routines for netowrk analysis 
# MATLAB written by: Brian D. Fath & Stuart R. Borrett 2004, VERSION REV 1.0.0 of NEA.m (2004)
# based upon:
# Fath, B.D. & Borrett, S.R. 2004. A MATLAB function for Network Environ Analysis. Environmental Modelling and Software 21:375-405.
# Ported to R by Jae S. Choi 2006

# Network Environ Analysis

  loadfunctions( "linear.network.analysis" )


# parameters
#
# The input variable 'data' is an (n+1 x n+2) matrix composed of:
#
# F = flow matrix [nxn] 
# z = input vector [nx1] 
# x = storage vector [nx1] 
# y = output vector [1xn] 
# .. a 1x2 vector of zeros.  
#
# Sample data from the intertidal oyster reef ecosystem model created by Dame and Patten (1981)
# From Patten (1985).  Model flows are in kcal m^-2 day^-1;
# storage data is kcal m^-2. 
# Dame, R. F., and B. C. Patten. 1981. Analysis of energy flows in an 
# intertidal oyster reef. Marine Ecology Progress Series 5:115-124.
# Patten, B. C. 1985. Energy cycling, length of food chains, and direct 
# versus indirect effects in ecosystems. Can. Bull. Fish. Aqu. Sci. 213:119-138.

# labels
Compartments = c( 
	"Filter Feeders", "Deposited Detritus", "Microbiota", "Meiofauna", "Deposit Feeders", "Predators")

# Steady-State Flow Matrix
F = matrix( c(
	0, 0, 0, 0, 0, 0, 
	15.7915, 0, 0, 4.2403, 1.9076, 0.3262,
	0, 8.1721, 0, 0, 0, 0, 
	0, 7.2745, 1.2060, 0, 0, 0,
	0, 0.6431, 1.2060, 0.6609, 0, 0,
	0.5135, 0, 0, 0, 0.1721, 0
), nrow=length(Compartments), byrow=T, dimnames=list(Compartments,Compartments)  )

# Steady-State Storage Vector
x = c(2000, 1000, 2.4121, 24.121, 16.274, 69.237)    

# Steady-state Input Vector
z = c(41.4697, 0, 0, 0, 0, 0) 

# Steady-State Outputs
y = c(25.1646, 6.1759, 5.7600, 3.5794, 0.4303, 0.3594)


# Ensure steady-state 
  check.data( F,z )

# Structural Analysis
  SAnal = NEA_structure(F)
  SAnal

# Throughflow Analysis 
  TFAnal = NEA_throughflow(F,y,z)
  TFAnal

# Storage Analysis 
  StAnal = NEA_storage(F,x)
  StAnal
  
# Utility Analysis 
  UtAnal = NEA_utility(F,x)
  UtAnal

# Unit Environ Analysis
  Unit.Environs = NEA_u_environs(TFAnal, StAnal)
  Unit.Environs

# Control Analysis 
  CtrlAnal = NEA_control( TFAnal, StAnal )
  CtrlAnal




