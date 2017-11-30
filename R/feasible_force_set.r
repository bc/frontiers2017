##' xyz_points_with_convhull
##' @param mat matrix with 3 columns for xyz
##' @param col color to paint the convex hull object'=
##' @param points logical- whether or not to show the actual XYZ points inside the convex hulls
##' @importFrom rgl points3d
xyz_points_with_convhull <- function(mat, col, points = TRUE,...) {
  if (points) {
    points3d(mat)
  }
  show_convhull(mat, col,...)
}

##' Add convex hull over points set onto active RGL device
##' @param mat matrix with 3 numeric columns
##' @param col color string
##' @param alpha_transparency transparency of the surface. numeric in 0 to 1
##' @importFrom geometry convhulln
##' @importFrom rgl rgl.triangles
show_convhull <- function(mat, col, alpha_transparency=0.6) {
  hull_indices <- t(convhulln(mat))
  convex1 <- rgl.triangles(mat[hull_indices, 1], mat[hull_indices, 2], mat[hull_indices,
    3], col = col, alpha=alpha_transparency)
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
  "blue"), sizes= c(1,1,1)) {
  big_mat <- dcrb(list_of_3d_matrices)
  x <- big_mat[, 1]
  y <- big_mat[, 2]
  z <- big_mat[, 3]
  rgl.lines(c(0, max(x)), c(0, 0), c(0, 0), color = cols[1], size=sizes[1])
  rgl.lines(c(0, 0), c(0, max(y)), c(0, 0), color = cols[2], size=sizes[2])
  rgl.lines(c(0, 0), c(0, 0), c(0, max(z)), color = cols[3], size=sizes[3])

  rgl.texts(c(max(x),0,0), text="X", col="black")
  rgl.texts(c(0,max(y),0), text="Y", col="black")
  rgl.texts(c(0,0, max(z)), text="Z", col="black")
}

##' RGL axes_for_defined_xyz_limits
##' @param xyz_range_vec vector of 3 elements, each a tuple of the range for i in xyz
##' @param cols vector of three colors that will be used for the XYZ axes. rgb by default.
axes_for_defined_xyz_limits <- function(xyz_range_vec, cols = c("red", "green",
  "blue")) {
  rgl.lines(xyz_range_vec[[1]], c(0, 0), c(0, 0), color = cols[1])
  rgl.lines(c(0, 0), xyz_range_vec[[2]], c(0, 0), color = cols[2])
  rgl.lines(c(0, 0), c(0, 0), xyz_range_vec[[3]], color = cols[3])
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
