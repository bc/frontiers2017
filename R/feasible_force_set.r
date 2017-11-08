##' xyz_points_with_convhull
##' @param mat matrix with 3 columns for xyz
##' @param col color to paint the convex hull object'=
##' @importFrom rgl points3d
xyz_points_with_convhull <- function(mat, col, points = TRUE) {
  if (points) {
    points3d(mat)
  }
  show_convhull(mat, col)
}

##' Add convex hull over points set onto active RGL device
##' @param mat matrix with 3 numeric columns
##' @param col color string
##' @importFrom geometry convhulln
##' @importFrom rgl rgl.triangles
show_convhull <- function(mat, col) {
  hull_indices <- t(convhulln(mat))
  convex1 <- rgl.triangles(mat[hull_indices, 1], mat[hull_indices, 2], mat[hull_indices,
    3], col = col, alpha = 0.6)
}

##' Start a new rgl device
##' sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
##' @param new.device a logical value. If TRUE, creates a new device
##' @param bg the background color of the device
##' @param width the width of the device
##' @importFrom rgl rgl.open par3d rgl.bg rgl.clear rgl.viewpoint
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) {
  if (new.device | rgl.cur() == 0) {
    rgl.open()
    par3d(windowRect = 50 + c(0, 0, width, width))
    rgl.bg(color = bg)
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

##' Force3d Matrix
##' @param df_with_force_columns df with JR3.FX cols, etc
##' @param force_dimension_names vector of strings, by default the first XYZ of JR3
##' @return mat 3-col matrix of the resultant vals.
force3d_matrix <- function(df_with_force_columns, force_dimension_names = force_column_names[1:3]) {
  mat <- matrix(cbind(df_with_force_columns[, force_dimension_names[1]], df_with_force_columns[,
    force_dimension_names[2]], df_with_force_columns[, force_dimension_names[3]]),
    ncol = 3)
  return(mat)
}

##' Plot rgl axes as colored lines based on observed ranges
##' @param list_of_3d_matrices each matrix has 3 columns for xyz
##' @param cols list of 3 strings for the colors to use
axes_for_multiple_sets <- function(list_of_3d_matrices, cols = c("red", "green",
  "blue")) {
  big_mat <- dcrb(list_of_mats)
  x <- big_mat[, 1]
  y <- big_mat[, 2]
  z <- big_mat[, 3]
  rgl.lines(c(min(x), max(x)), c(0, 0), c(0, 0), color = cols[1])
  rgl.lines(c(0, 0), c(min(y), max(y)), c(0, 0), color = cols[2])
  rgl.lines(c(0, 0), c(0, 0), c(min(z), max(z)), color = cols[3])
}

##' For each element of a list, add a color attribute
##' @param L list of objects'
##' @param gradient vector of strings, typically colors in hex codes.
##' @return L_with_colors list with a string in its color attribute
add_gradient_to_attrs <- function(L, gradient){
  for (i in seq(1, length(L))) {
    attr(L[[i]], "color") <- gradient[[i]]
  }
  return(L)
}