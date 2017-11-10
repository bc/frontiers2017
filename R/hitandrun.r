##' Determine whether constraints are feasible by attempting to get a starting point
##' @param constr linear constraints list object. see hitandrun for composition
##' @return feasible TRUE or FALSE. If TRUE you can sample points from it.
##' @importFrom hitandrun createSeedPoint
constraints_are_feasible <- function(constr){
	attempt <- tryCatch(createSeedPoint(constr), error = function(e) e)
	!any(class(attempt) == 'error')
}
