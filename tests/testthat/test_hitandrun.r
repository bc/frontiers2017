context("test_hitandrun.r")



test_that("hit and run works with the canonical 3 muscle, 1N output request", {
  task <- c(5.01)
  constr <- canonical_linear_system(task)
  state <- har.init(constr)
  result <- har.run(state, n.samples = 10000)
  samples <- result$samples
  hist(samples[,1], breaks=200)
  plot3d(samples)  #show 3d plane
  predicted_forces <- predict_output_force(t(constr$constr), samples)[,1]

  maximum_absolute_residual(vector = predicted_forces,desired_val = task)
})


test_that("2D system gives correct output", {
  task <- 1
  big_A <- rbind(c(2, -2),
                  c(1, 0),
                  c(0,1),
                  c(-1,0),
                  c(0,-1))
  big_b <- c(task, task, 1, 0, 1, 0)
  dir <- c("=","=",rep("<=", 4))
  constr <- list(constr = big_A, dir = dir, rhs = big_b)
  state <- har.init(constr, thin=100)
  result <- har.run(state, n.samples = 20)
  samples <- result$samples
  hist(samples[,1], breaks=200)
  plot3d(samples)  #show 3d plane
  big_A %*% t(samples)
})
