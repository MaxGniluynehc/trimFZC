#
# # library(trimFZC)
#
# # Suppose we want to analyze on the effect of sample size on the efficiency of the
# # trimmed mean estimates.
#
# mu = 0
# sigma = 5
# n = seq(50, 1000, by=100) # sample size (the parameter that varies)
# N = 5000 # number of simulations
# tr = 0.2 # trim ratio
#
# # Plot boxplot
# plotter("boxplot", n, mu, sigma, tr, N)
#
# # Plot Monte Carlo Simulation Error (MCSE) against the parameter that varies
# plotter("MCSE", n, mu, sigma, tr, N)
#
# # Plot Empirical Variance (EmpVar) against the parameter that varies
# plotter("EmpVar", n, mu, sigma, tr, N)
#
#
# # For pairwise comparisons, suppose we want to compare the tr = 0.2 vs tr = 0,
# # holding all other parameters fixed.
#
# # Plot QQplot
# plotter("QQplot", n=500, mu, sigma, tr=c(0.2, 0), N)
#
# # Plot Empirical Cumulative Distribution Function (ECDF)
# plotter("QQplot", n=500, mu, sigma, tr=c(0.2, 0), N)
#
#
#
#
#
#
#
#
#
#
#
