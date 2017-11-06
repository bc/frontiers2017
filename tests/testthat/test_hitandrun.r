context("hit and run functions")

test_that("hit and run works with the canonical 3 muscle, 1N output request", {

  constr <- canonical_linear_system()

  state <- har.init(constr)
  result <- har.run(state, n.samples = 10000)
  samples <- result$samples
  hist(samples[,1], breaks=200)

  plot3d(samples)  #show 3d plane
})

canonical_linear_system <- function() {
  list(constr = rbind(c(10/3, -53/15, 2), c(-10/3, 53/15, -2), c(1, 0, 0), c(-1,
    0, 0), c(0, 1, 0), c(0, -1, 0), c(0, 0, 1), c(0, 0, -1)), dir = rep("<=",
    8), rhs = c(1, -1, 1, 0, 1, 0, 1, 0))
}
