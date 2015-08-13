# our problem is that, after bootstrapping splines, we get 1000 condition means.
# while it's easy to get a Mean and CI (1.96*SD) for the conditions, we actually want
# to compare the two distributions and see whether they are different.
# this method allows us to resampled the bootstrapped distribution and recover
# a Mean and CI for the *difference* between conditions that approximates
# the results of a t-test on the original data

# original data
test1 <- rnorm(30,.45,.4)
test2 <- rnorm(30,.85,.4)

# now sample a mean from test1 and test2 1000 times,
# creating a dataset that is way bigger than original data
# (this simulates our bootstrapped splines -- we get 1000 samples at each timepoint)
test1_sampled <- replicate(1000, mean(sample(test1, 30, replace=T)))
test2_sampled <- replicate(1000, mean(sample(test2, 30, replace=T)))

bootstrapper <- function(test1_sampled,test2_sampled) {
  # take any 2 random means and subject them from one another
  random1 <- sample(test1_sampled, 1)
  random2 <- sample(test2_sampled, 1)
  
  return (random1 - random2)
}

straps <- replicate(1000, bootstrapper(test1_sampled,test2_sampled))

ci_low <- mean(straps) - 1.96*sd(straps)
ci_high <- mean(straps) + 1.96*sd(straps)

# cool: we can retrieve the bootstrapped CI of the *original* data despite
# actually resampling from a bootstrapped distribution of sample means
ci_low
ci_high

t.test(test1,test2,var.equal=T)
