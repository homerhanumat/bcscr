findLimits <- function(qdist, qparams) {
  xlim_opts <- do.call(qdist, c(list(p = c(0, 0.001, 0.999,
                                           1)), qparams))
  dxlim_opts <- diff(xlim_opts)
  xlim <- xlim_opts[2:3]
  if (dxlim_opts[1] < dxlim_opts[2]) {
    xlim[1] <- xlim_opts[1]
  }
  if (dxlim_opts[3] < dxlim_opts[2]) {
    xlim[2] <- xlim_opts[4]
  }
  xlim
}

findLimits("qgamma", list(shape = 3, rate = 0.2))
