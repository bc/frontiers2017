context("test_hitandrun.r")



test_that("hit and run works with the canonical 3 muscle, 1N output request", {

  num_muscles <- 3
  tension_range <- c(0, 1)
  task <- c(0.5)
  num_samples_desired <- 10000

  bound_constraints <- mergeConstraints(create_bound_constraints(constraints_width = num_muscles,
    tension_range))
  motor_task_constraint <- list(constr = c(10/3, -53/15, 2), dir = rep("=", 1),
    rhs = c(task))
  constr <- mergeConstraints(motor_task_constraint, bound_constraints)
  samples <- har_collect_points(constr, thin = 100, n = num_samples_desired)
  expect_equal(nrow(samples), num_samples_desired)
  plot3d(samples, xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1))
  # Select only the first column, that corresponds to the output force.
  predicted_forces <- predict_output_force(t(constr$constr), samples)[, 1]
  expect_equal(maximum_absolute_residual(vector = predicted_forces, desired_val = task),
    0, tol = 1e-05)
})

test_that("hit and run works with the canonical 3 muscle, 1N output request, with modularized function", {

  tension_range <- c(0, 1)
  task <- c(0.5)
  num_samples_desired <- 10000
  muscle_column_generators <- matrix(c(10/3, -53/15, 2), ncol=3)
  samples <- constraints_to_points(muscle_column_generators, tension_range, task, num_samples_desired, thin=100)
  expect_equal(nrow(samples), num_samples_desired)
  plot3d(samples, xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1))
  # Select only the first column, that corresponds to the output force.
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
  samples <- har_collect_points(constr, thin = 100, n = 20000)
  par(mfrow = c(1, 1))
  plot(samples, xlab = "x1", ylab = "x2", pch = 19, cex = 0.25, main = "This should look like a uniformly-sampled square in Q1")
})
