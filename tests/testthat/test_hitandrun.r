context("test_hitandrun.r")



test_that("hit and run works with the canonical 3 muscle, 1N output request", {
  task <- c(1)
  motor_task_constraint <- list(constr = c(10/3, -53/15, 2), dir = rep("=", 1),
    rhs = c(task))
    constraints_width <- 3
    tension_range <- c(0,1)
    bound_constraints <- mergeConstraints(create_bound_constraints(constraints_width=3, tension_range))
  constr <- mergeConstraints(motor_task_constraint, bound_constraints)
  num_samples_desired <- 10000
  state <- har.init(constr, thin=100)
  result <- har.run(state, n.samples = num_samples_desired)
  samples <- result$samples
  expect_equal(nrow(samples), num_samples_desired)
  hist(samples[, 1], breaks = 200)
  plot3d(samples)  #show 3d plane
  predicted_forces <- predict_output_force(t(constr$constr), samples)[, 1]
  expect_equal(maximum_absolute_residual(vector = predicted_forces, desired_val = task),
    0, tol = 1e-05)

})

test_that("samples_from_2d_square in Q1 successfully", {
  task <- 1
  lbx <- lowerBoundConstraint(n = 2, i = c(1), 0)
  lby <- lowerBoundConstraint(n = 2, i = c(2), 0)
  ubx <- upperBoundConstraint(n = 2, i = c(1), 1)
  uby <- upperBoundConstraint(n = 2, i = c(2), 1)
  constr <- mergeConstraints(lbx, lby, ubx, uby)
  state <- har.init(constr, thin = 100)
  result <- har.run(state, n.samples = 20000)
  samples <- result$samples
  par(mfrow = c(1, 1))
  plot(samples, xlab = "x1", ylab = "x2", pch = 19, cex = 0.25, main = "This should look like a uniformly-sampled square in Q1")
  hist(samples[, 1], breaks = 200)
  plot3d(samples)  #show 3d plane
  big_A %*% t(samples)
})
