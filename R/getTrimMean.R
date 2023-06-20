#' Compute the trimmed mean of a sample from Normal distribution
#'
#' `getTrimMean()` computes the trimmed sample means of `N` simulations (each of size `n`)
#' from a Normal distribution with mean `mu` and standard deviation `sigma`. The trimmed
#' sample is obtained by chopping off the `tr` portion of the extreme values in a sample.
#'
#' @param n The size of a single simulation sampled from the Normal distribution.
#' An integer. Defaults to 1000.
#' @param N The nunber of repeated simulations. An integer. Default to 5000.
#' @param mu The grounding true mean of the Normal distribution. A float. Default to 0.
#' @param sigma The grounding true standard deviation of the Normal distribution.
#' A float. Default to 1.
#' @param tr The trim ratio, the proportion of the sample is chopped off as extreme values.
#' A float. Default to 0.
#' @param random.seed Set a random seed.
#'
#' @return A vector of trimmed sample means of size `N`. If `tr` is set as 0, it effectively
#' returns the untrimmed mean.
#'
#' @examples
#' # Directly get a vector of trimmed sample means of size `N`.
#' getTrimMean()
#'
#' @export

getTrimMean <- function(n=1000, N=5000, mu=0, sigma=1, tr=0, random.seed=20230615) {
  # Tests for invalid inputs:
  testthat::expect_true(all(is.numeric(n), is.numeric(mu), is.numeric(sigma), is.numeric(tr), is.numeric(N)),
                        info = "The parameters (n, mu, sigma, tr, N) all have to be numerical!")
  testthat::expect_equal(sum(c(length(n), length(mu), length(sigma), length(tr), length(N)) != 1), 0,
                         info = "The parameters (n, mu, sigma, tr, N) all have to take singlular numerical value!")
  testthat::expect_true(all(n > 0, N > 0, sigma > 0),
                        info= "The (n, N, sigma) cannot take 0 or negative numbers!")
  testthat::expect_true(all(tr >= 0, tr < 1),
                        info= "'tr' cannot take negative numbers or any numbers >= 1!")
  testthat::expect_true(N == round(N), info = "'N' has to be an integer!")


  set.seed(random.seed)
  mu_hats = c()
  for (i in 1:N) {
    y = rnorm(n, mu, sigma)
    mu_hat = mean(y, trim=tr)
    mu_hats = append(mu_hats, mu_hat)
  }
  return(mu_hats)
}






