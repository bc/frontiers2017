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
