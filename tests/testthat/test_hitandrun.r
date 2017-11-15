context("test_hitandrun.r")

canonical_linear_system <- function(task) {
  list(constr = rbind(c(10/3, -53/15, 2),
                      c(-10/3, 53/15, -2),
                      c(task, 0, 0),
                      c(-task,0, 0),
                      c(0, 1, 0),
                      c(0, -1, 0),
                      c(0, 0, 1),
                      c(0, 0, -1)), dir = rep("<=",
    8), rhs = c(0.5, -0.5, 1, 0, 1, 0, 1, 0))
  }
test_that("hit and run works with the canonical 3 muscle, 1N output request", {
  constr <- canonical_linear_system(task)
  constraints_are_feasible(constr)
  state <- har.init(constr)
  result <- har.run(state, n.samples = 10000)
  samples <- result$samples
  hist(samples[,1], breaks=200)
  plot3d(samples)  #show 3d plane
})
