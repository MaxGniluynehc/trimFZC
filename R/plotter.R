#' Plotter Function
#'
#' `plotter()` can plot the boxplot, Monte Carlo Simulation Error (MCSE), Empirical Variance
#' (EmpVar), QQplot, and Empirical Cummulative Distribution Function (ECDF).
#'
#' @param plot_type takes values from ["boxplot", "MCSE", "EmpVar", "QQplot", "ECDF"]
#' @param n sample size of each simulation
#' @param mu The grounding true mean of the Normal distribution. A float.
#' @param sigma The grounding true standard deviation of the Normal distribution.
#' A float.
#' @param tr The trim ratio, the proportion of the sample is chopped off as extreme values.
#' A float.
#' @param N The nunber of repeated simulations. An integer.
#'
#' @examples
#' # Suppose we want to analyze on the effect of sample size on the efficiency of the
#' # trimmed mean estimates.
#' mu = 0
#' sigma = 5
#' n = seq(50, 1000, by=100) # sample size (the parameter that varies)
#' N = 5000 # number of simulations
#' tr = 0.2 # trim ratio
#'
#' # Plot boxplot
#' plotter("boxplot", n, mu, sigma, tr, N)
#'
#' # Plot Monte Carlo Simulation Error (MCSE) against the parameter that varies
#' plotter("MCSE", n, mu, sigma, tr, N)
#'
#' # Plot Empirical Variance (EmpVar) against the parameter that varies
#' plotter("EmpVar", n, mu, sigma, tr, N)
#'
#'
#' # For pairwise comparisons, suppose we want to compare the tr = 0.2 vs tr = 0,
#' # holding all other parameters fixed.
#'
#' # Plot QQplot
#' plotter("QQplot", n=500, mu, sigma, tr=c(0.2, 0), N)
#'
#' # Plot Empirical Cumulative Distribution Function (ECDF)
#' plotter("QQplot", n=500, mu, sigma, tr=c(0.2, 0), N)
#'
#' @export

plotter <- function(plot_type, n, mu, sigma, tr, N) {
  # All the tests:
  testthat::expect_true(plot_type %in% c("boxplot", "MCSE", "EmpVar", "QQplot", "ECDF"),
              info = "Wrong plot_type! plot_type has to be one of ['boxplot', 'MCSE', 'EmpVar', 'QQplot', 'ECDF']. ")
  testthat::expect_true(N == round(N), info = "The number of simulations (N) has to be an integer!")

  testthat::expect_true(all(is.numeric(n), is.numeric(mu), is.numeric(sigma), is.numeric(tr)),
              info = "The sample size (n), grounding true mean (mu), groudning true standard deviation (sigma),
              and trim ratio (tr) all have to take numerical values!")
  if (! plot_type %in% c("QQplot", "ECDF")){
    testthat::expect_equal(sum(c(length(n), length(mu), length(sigma), length(tr)) != 1), 1,
                info = "If not plotting boxplot, MCSE or EmpVar, then exactly one of the sample size (n), grounding true mean (mu),
                groudning true standard deviation (sigma), and trim ratio (tr) has to take a vector of length larger than 1.")
  }
  else{
    testthat::expect_equal(sum(c(length(n), length(mu), length(sigma), length(tr)) == 2), 1,
                 info = "If plotting QQplot or ECDF, then exactly one of the sample size (n), grounding true mean (mu),
                 groudning true standard deviation (sigma), and trim ratio (tr) has to take a vector of length 2.")
  }
  # else{
  #   expect_equal(sum(c(length(n), length(mu), length(sigma), length(tr)) != 1), 0,
  #                info = "If plotting ECDF, then all of the sample size (n), grounding true mean (mu),
  #                groudning true standard deviation (sigma), and trim ratio (tr) have to take a single value.")
  # }

  # Now the function:
  if (plot_type %in% c("boxplot", "MCSE", "EmpVar")){
    params = cbind(n, mu, sigma, tr)
    pml = apply(params, 2, function(x){return(length(unique(x)))})
    pname_var = names(pml)[which.max(pml)]

    mu_hats_list = c()
    MCSEs_list = c()
    EmpVar_list = c()
    iters = nrow(params)
    for (i in 1:iters){
      mu_hats = getTrimMean(n=params[i,1], N=N, mu=params[i,2], sigma=params[i,3], tr=params[i,4])
      mu_hats_list = cbind(mu_hats_list, mu_hats)

      EmpVar = var(mu_hats)
      MCSEs_list = append(MCSEs_list, sqrt(EmpVar/(2*(N-1))))
      EmpVar_list = append(EmpVar_list, EmpVar)
    }
    colnames(mu_hats_list) = paste(pname_var, "=", params[, pname_var], sep = "")

    lgd = ""
    pnames = c("n", "mu", "sigma", "tr")
    for (i in 1:length(pnames)){
      if (pnames[i] != pname_var){
        lgd = paste(lgd, " ", pnames[i], "=", unique(params[,i]), sep="")
      }
    }

    if (plot_type == "boxplot"){
      par(mfrow=c(1,1))
      boxplot(mu_hats_list,
              xlab=pname_var, ylab="Trimmed mean", cex.lab=1.5, cex.main=1.5, main=lgd)
      # legend("bottomright", legend = lgd, cex=1.5)
    }

    else if (plot_type =="MCSE"){
      par(mfrow=c(1,1))
      plot(params[, pname_var], MCSEs_list,
           xlab=pname_var, ylab="MCSE", cex.lab=1.5, cex.main=1.5,
           main=lgd)
      lines(params[, pname_var], MCSEs_list, lty=1, col="black")
      abline(h=2e-04, lty="dashed", col="blue")
      # legend("topright", legend = paste("mu=",0, ", sigma=", 1, ", tr=", 0.1, sep=""),
             # cex=1.5)
    }

    else if (plot_type =="EmpVar"){
      EmpVar_list = apply(mu_hats_list, 2, var)
      plot(params[, pname_var], EmpVar_list,
           xlab=pname_var, ylab="Empirical Variance", cex.lab=1.5, cex.main=1.5,
           main=lgd)
      lines(params[, pname_var], EmpVar_list, lty=1, col="black")
      abline(h=(2e-04*sqrt(2*(15000-1)))**2, lty="dashed", col="blue")
      # points(850, EmpVar_list[9], col="red", pch=17, cex=2)
      # legend("topright", legend = paste("mu=",0, ", sigma=", 1, ", tr=", 0.1, sep=""),
      #        cex=1.5)
    }

  }

  else{
    params = cbind(n, mu, sigma, tr)
    pml = apply(params, 2, function(x){return(length(unique(x)))})
    pname_var = names(pml)[which.max(pml)]

    lgd = ""
    pnames = c("n", "mu", "sigma", "tr")
    for (i in 1:length(pnames)){
      if (pnames[i] != pname_var){
        lgd = paste(lgd, " ", pnames[i], "=", unique(params[,i]), sep="")
      }
    }

    mu_hats_list = c()
    iters = nrow(params)
    for (i in 1:iters){
      mu_hats = getTrimMean(n=params[i,1], N=N, mu=params[i,2], sigma=params[i,3], tr=params[i,4])
      mu_hats_list = cbind(mu_hats_list, mu_hats)
    }
    colnames(mu_hats_list) = paste(pname_var, "=", params[, pname_var], sep = "")


    if (plot_type =="QQplot"){
      qqplot(mu_hats_list[,1], mu_hats_list[,2], main=lgd,
             xlab=colnames(mu_hats_list)[1], ylab=colnames(mu_hats_list)[2], cex.lab=1.5)
      abline(a=0, b=1, col=2)
    }

    else if (plot_type =="ECDF"){
      plot(ecdf(mu_hats_list[,1]), main=lgd,
           xlab="Mean", ylab = "ECDF(mean)", cex.lab=1.5, col="black") # cex.main=3)
      lines(ecdf(mu_hats_list[,2]), col="red")
      legend("bottomright", legend = colnames(mu_hats_list),
             lty = 1, col=c("black", "red"), cex=1.2)
    }

  }

}



