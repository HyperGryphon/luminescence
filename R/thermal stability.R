library(Luminescence)
#usar los datos del artículo de thays
E <- c(1.6)
s <- c(1e+13)
T <- 20
temp <- calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  output_unit = "Ma", profiling = F)
#contour(x = E, y = T, z = temp$lifetimes[1,,],
#        ylab = "Temperature [\u00B0C]",
#        xlab = "Trap depth [eV]",
#        main = "Thermal Lifetime Contour Plot")
#mtext(side = 3, "(values quoted in Ma)")