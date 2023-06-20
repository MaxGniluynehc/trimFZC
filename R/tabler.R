#' Tabler Function
#'
#' `tabler()` can output a table with the bias, Empirical Variance (EmpVar), Empirical Standard Error (EmpSE)
#'  and Monte Carlo Simulation Error (MCSE) as the columns. The rows corresponds to the input argument
#'  (one of ['n', 'mu', 'sigma', 'tr', 'N']) that takes a vector with length > 1.
#'
#' @details
#' Note that only one of the input argument can take a vector with length > 1, the other input arguments
#' have to be singular numbers. Otherwise, the function will explode.
#'
#' @param n (Required, numeric) sample size of each simulation
#' @param mu (Required, numeric) The grounding true mean of the Normal distribution.
#' @param sigma (Required, numeric) The grounding true standard deviation of the Normal distribution.
#' @param tr (Required, numeric) The trim ratio, the proportion of the sample is chopped off as extreme values.
#' @param N (Required, numeric) The number of repeated simulations. An integer. Default as 15000.
#' @param to_kable (Optional, Boolean) Whether to print the tabularized statistics using the knitr::kable().
#' Default as False.
#' @param degits (Optional, numeric) Number of digits to keep in the printed kable output. Only applied when
#' to_kable is TRUE. Default as 4.
#' @param random.seed (Optional) Set a random seed.
#'
#' @examples
#' To obtain a table of bias, EmpVar, EmpSE and MCSE corresponding to various trim ratios:
#' tabler(50, 0, 1, seq(0,0.5, by=0.025))
#'
#' Print the result as kable output:
#' tab = tabler(50, 0, 1, seq(0,0.5, by=0.025), to_kable=T, digits=3, random.seed=1234)
#'
#' @returns A datafrme containing the bias, EmpVar, EmpSE and MCSE.
#'
#' @export


tabler <- function(n ,mu, sigma, tr, N=15000, random.seed = 20230615) {
  testthat::expect_true(sum(c(length(n), length(mu), length(sigma), length(tr)) > 1) <= 1,
                         info = "At most one of (n, mu, sigma, tr) can take a vector of length
                         larger than 1.")

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
    mu_hats = getTrimMean(n=params[i,1], N=N, mu=params[i,2], sigma=params[i,3], tr=params[i,4], random.seed = random.seed)
    bias_list = append(bias_list, mean(mu_hats - mu))


    EmpVar = var(mu_hats)
    MCSEs_list = append(MCSEs_list, sqrt(EmpVar/(2*(N-1))))
    EmpVar_list = append(EmpVar_list, EmpVar)
  }
  EmpSE_list = sqrt(EmpVar_list)
  tab = cbind(bias_list, EmpVar_list, EmpSE_list, MCSEs_list)
  colnames(tab) = c("Bias", "EmpVar", "EmpSE", "MCSE")
  rownames(tab) = paste(pname_var, "=", params[, pname_var], sep = "")

  return(data.frame(tab))
}








