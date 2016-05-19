netmensuration.parameters.local.overrides = function( id , bcp) {
  #\\ some trawl profiles are just really challenging to summarize with a default/fixed set
  #\\ of parameters. This permits a local fix, specific to a sampling program.
  #\\ Ideally, these parameters can be chosen more cleverly inside the system. This is a future TODO.
  #\\ Most issues can be resolved by modfying the depth range that ultimately gets gated into the analysis.

  # low-level hacks:: over-ride of bottom contact parameters for strange/extreme data
  if (id %in% c("TEL2004529.18" )) bcp$depth.range = c(-120, 120) # a large range!

  if (id %in% c("NED2013028.172", "TEL2004530.84" )) {
    bcp$depth.range = c(-70, 120)
  }

  if (id=="TEL2004529.16") {
    bcp$depth.range = c(-200, 200)
    bcp$noisefilter.trim = 0.025
    bcp$noisefilter.quants = c(0.025, 0.975)
  }

  if (id=="NED2013028.10") {
    bcp$depth.range = c(-10, 10)
    bcp$noisefilter.var.window = 5
    bcp$noisefilter.quants = c(0.025, 0.975)
  }

  if (id=="TEL2004530.85") {
    bcp$noisefilter.trim = 0.05
    bcp$noisefilter.var.window = 5
    bcp$depth.range = c(-80, 80)
  }


  if (id=="TEL2004529.20") {
    bcp$noisefilter.quants = c(0.1, 0.9)
    bcp$noisefilter.target.r2 = 0.95
    bcp$depth.range = c(-80, 80)
  }

  if (id=="TEL2004529.16") {
    bcp$noisefilter.var.window = 5
    bcp$noisefilter.target.r2 = 0.9
    bcp$depth.range = c(-80, 80)
  }

  if (id=="NED2010001.36") {
    bcp$noisefilter.trim = 0.01
    bcp$noisefilter.var.window = 5
  }

  if (id=="NED2010002.3") {
    bcp$depth.range = c(-20, 20)
    bcp$noisefilter.target.r2 = 0.95
  }

  if (id=="NED2014101.19") {
    bcp$depth.range = c(-60, 50)
    bcp$noisefilter.target.r2 = 0.9
    bcp$noisefilter.trim = 0.1
  }

  if (id=="TEL2004530.50") {
    bcp$noisefilter.trim = 0.1
    bcp$noisefilter.quants = c(0.1, 0.9)
    bcp$noisefilter.target.r2 = 0.9
    bcp$depth.range = c(-45, 55)
    bcp$noisefilter.var.window = 5
  }

  if (id=="NED2009027.68") {
    bcp$noisefilter.target.r2 = 0.95
    bcp$depth.range = c(-35, 25)
  }

  if (id=="NED2015002.39") {
    bcp$depth.range = c(-15, 15)
  }

  return (bcp)
}
