#' Tabler Function
#'
#' `tabler()` can output a table with the bias, Empirical Variance (EmpVar) and Monte Carlo Simulation Error (MCSE)
#' as the columns. The rows corresponds to the input argument (one of ['n', 'mu', 'sigma', 'tr', 'N']) that takes a
#' vector with length > 1.
#'
#' @details
#' Note that only one of the input argument can take a vector with length > 1, the other input arguments have to be
#' singular numbers. Otherwise, the function will explode.
#'
#' @param n sample size of each simulation
#' @param mu The grounding true mean of the Normal distribution. A float.
#' @param sigma The grounding true standard deviation of the Normal distribution.
#' A float.
#' @param tr The trim ratio, the proportion of the sample is chopped off as extreme values.
#' A float.
#' @param N The number of repeated simulations. An integer. Default as 15000.
#'
#' @examples
#' To obtain a table of bias, EmpVar and MCSE corresponding to various trim ratios:
#' tabler(50, 0, 1, seq(0,0.5, by=0.025))
#'
#'
#' @export


tabler <- function(n ,mu, sigma, tr, N=15000) {
  testthat::expect_true(all(length(n) >=1, length(mu)>=1, length(sigma)>=1, length(tr)>=1),
                        info = "Exactly one of the sample size (n), grounding true mean (mu),
                        groudning true standard deviation (sigma), and trim ratio (tr) has to take a vector of length larger than 1.")

  testthat::expect_equal(sum(c(length(n), length(mu), length(sigma), length(tr)) > 1), 1,
                         info = "Exactly one of the sample size (n), grounding true mean (mu),
                         groudning true standard deviation (sigma), and trim ratio (tr) has to take a vector of length larger than 1.")

  params = cbind(n, mu, sigma, tr)
  pml = apply(params, 2, function(x){return(length(unique(x)))})
  pname_var = names(pml)[which.max(pml)] # name of the parameter that varies
  if (max(pml)>20){
    warning(paste("Input parameter ", pname_var, " contains too many entries, causing too many rows in the tabularized output!"))
  }

  bias_list = c()
  MCSEs_list = c()
  EmpVar_list = c()
  iters = nrow(params)
  for (i in 1:iters){
    mu_hats = getTrimMean(n=params[i,1], N=N, mu=params[i,2], sigma=params[i,3], tr=params[i,4])
    # mu_hats_list = cbind(mu_hats_list, mu_hats)
    bias_list = append(bias_list, mean(mu_hats - mu))


    EmpVar = var(mu_hats)
    MCSEs_list = append(MCSEs_list, sqrt(EmpVar/(2*(N-1))))
    EmpVar_list = append(EmpVar_list, EmpVar)
  }

  tab = cbind(bias_list, EmpVar_list, MCSEs_list)
  colnames(tab) = c("Bias", "EmpVar", "MCSE")
  rownames(tab) = paste(pname_var, "=", params[, pname_var], sep = "")

  return(data.frame(tab))
}








