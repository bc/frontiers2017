##' Determine whether constraints are feasible by attempting to get a starting point
##' @param constr linear constraints list object. see hitandrun for composition
##' TODO make sure this function actually works. I don't think it works right now for some reason. We might need to make this more fuzzy (allow some error)
##' @return feasible TRUE or FALSE. If TRUE you can sample points from it.
##' @importFrom hitandrun createSeedPoint
constraints_are_feasible <- function(constr){
	attempt <- tryCatch(createSeedPoint(constr), error = function(e) e)
	!any(class(attempt) == 'error')
}



dir_vector <- function(task_force, num_muscles){
  dir_vector <- c(rep("=", length(task_force)), rep("<=", num_muscles),
  rep("<=", num_muscles))
}

compose_b_vector <- function(task_force, range_tension, num_muscles){
  as.numeric(c(task_force, rep(-range_tension[1], num_muscles),
    rep(range_tension[2], num_muscles)))
}
##' task_and_generators_to_constr
##' @return constr constraints in the format acceptable by hitandrun. See ?har.init
##' TODO fill in param descriptions'
##' TODO test
##' @param generator_columns_A_matrix TODO
##' @param muscle_constraints_matrix TODO
##' @param range_tension 2 element vector defining lower and upperbound for tension placed on all tendons. Assumes same bounds for all muscles.
##' @param task_force vector of numeric numbers representing the output task vector. dim is the same as the nrow(generator_columns_A_matrix)
task_and_generators_to_constr <- function(generator_columns_A_matrix,
  muscle_constraints_matrix, range_tension, task_force) {
  big_A <- rbind(generator_columns_A_matrix, -muscle_constraints_matrix, muscle_constraints_matrix)
  num_muscles <- ncol(big_A)
  big_b <- compose_b_vector(task_force, range_tension, num_muscles)
  dir <- dir_vector(task_force,num_muscles)
  return(list(constr = big_A, dir = dir, rhs = big_b))
}

##' Multiple Multicolor Convhulls
##' @param list_of_3d_matrices each a matrix with only 3 columns to show as XYZ. Must have attr(,"color") for each in the list
##' @param points logical - whether or not to show the xyz points within each convex hull.
rgl_convhulls <- function(list_of_3d_matrices, points){
  lapply(list_of_3d_matrices, function(mat) {
    xyz_points_with_convhull(mat, col = attr(mat, "color"), points = points)
  })
}

##' Canonical LInear System constraints example
##' @param task a single numeric task, positive or negative
##' @return constr constraints matrix and dir. See ?hitandrun
canonical_linear_system <- function(task) {
  big_A <- rbind(c(10/3, -53/15, 2),
                      c(1, 0, 0),
                      c(0, 1, 0),
                      c(0, 0, 1),
                      c(-1,0, 0),
                      c(0, -1, 0),
                      c(0, 0, -1)
                    )
  big_b <- c(task, 1, 0, 1, 0, 1, 0)
  dir <- c("=",rep("<=", 6))
  constr <- list(constr = big_A, dir = dir, rhs = big_b)
  return(constr)
  }
	##' create_upperbound_constraints
	##' @param constraints_width integer (if you have 3 muscles, this will be 3)
	##' @param upper_bound numeric single value
	##' @return constraints_list list of constr elements, see ?hitandrun::har
	##' @importFrom hitandrun upperBoundConstraint
	create_upperbound_constraints <- function(constraints_width, upper_bound){
	  lapply(1:constraints_width, function(column){
	    constr <- upperBoundConstraint(n = constraints_width, i = c(column), upper_bound)
	  })
	}

	##' create_lowerbound_constraints
	##' @param constraints_width integer (if you have 3 muscles, this will be 3)
	##' @param lower_bound numeric single value
	##' @return constraints_list list of constr elements, see ?hitandrun::har
	##' @importFrom hitandrun lowerBoundConstraint
	create_lowerbound_constraints <- function(constraints_width, lower_bound){
	  lapply(1:constraints_width, function(column){
	    constr <- lowerBoundConstraint(n = constraints_width, i = c(column), lower_bound)
	  })
	}
	##' Create bound constraints
	##' @param constraints_width integer (if you have 3 muscles, this will be 3)
	##' @param range_tension vector of two numeric values for (lowerbound,upperbound)
	##' @return constraints_list list of constr elements, see ?hitandrun::har
	##' @importFrom hitandrun lowerBoundConstraint
	create_bound_constraints <- function(constraints_width, range_tension){
	  c(create_lowerbound_constraints(constraints_width, range_tension[1]),
	  create_upperbound_constraints(constraints_width, range_tension[2]))
	}

	##' Collect points from hit and run (specified for frontiers2017 data)
	##' @param constr see ?hitandrun::har
	##' @param thin mixing time integer
	##' @param n int number of samples to produce
	##' @return samples matrix of dim [n,width(constr)]
	##' @importFrom hitandrun har.init har.run
har_collect_points <- function(constr, thin, n) {
	state <- har.init(constr, thin = thin       )
	result <- har.run(state, n.samples = n)
	samples <- result$samples
	return(samples)
}

##' Constraints_to_points
##' @param muscle_column_generators matrix where each column is a muscle generator, each row is a force output dimension
##' @param range_tension vector of two numeric values for (lowerbound,upperbound)
##' @param task vector of n numeric values. i.e. c(fx,fy)
##' @param num_samples_desired int
##' @param thin mixing time integer
##' @return samples matrix of samples, ncol == muscle_column_generators, nrow == num_samples_desired
constraints_to_points <- function(muscle_column_generators, range_tension, task, num_samples_desired, thin){
  num_muscles <- ncol(muscle_column_generators)
  bound_constraints <- mergeConstraints(create_bound_constraints(constraints_width = num_muscles,
    range_tension))
  motor_task_constraint <- list(constr = muscle_column_generators, dir = rep("=", nrow(muscle_column_generators)),
    rhs = task)
  constr <- mergeConstraints(motor_task_constraint, bound_constraints)
  samples <- har_collect_points(constr, thin = thin, n = num_samples_desired)
  return(samples)
}

##' N Histograms of the projections of the samples in N-dimensions
##' @param samples matrix of samples, where each row is a sample, with nmuscles columns
##' @param range_tension 2 element vector of min max in newton_values
##' @param task a vector of fx fy, ... values etc.
fas_histogram <- function(samples, range_tension, task, ...){
  lapply(1:ncol(samples), function(muscle_num) {
    hist(samples[, muscle_num], main = paste("M", muscle_num, "at",
    format(task, digits = 2), collapse = ""), xlab = "Tendon force (N)",
    xlim = range_tension, ...)
  })
}


##' Print show_l1_costs to help idenfity which tasks have multiple solutions
##' @param samples see constraints_to_points
##' @param task see constraints_to_points
show_l1_costs <- function(samples, task){
  message(paste("TASK:",task))
  message("-------------------------")
  message("Lowest l1 cost solution:")
  message(format(lowest_l1_cost_soln(samples), digits = 2))
  message("Highest l1 cost solution:")
  message(format(highest_l1_cost_soln(samples), digits = 2))
  message("===========================")
}
