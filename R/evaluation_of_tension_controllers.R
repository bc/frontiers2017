##' Plot reference versus measured force for all of the motors
##' useful for showing the deadband at low forces
##' @param input_output_data dataframe where there are reference and measured columns. 
##' @param xlimits 2 element vector of lowerbound and upperbound. ie.. c(1,200). passed to p + xlim(_,_) 
##' @return plot_grob a ggplot object/grob object with multiple plots. you can show it by calling plot on it.
plot_ref_vs_meas_linearity <- function(input_output_data, xlimits=NULL){

	force_matching <- lapply(muscle_names(), function(muscle_name){
		p0 <- ggplot(input_output_data, aes_string(reference(muscle_name), measured(muscle_name)))
		p0 <- p0 + geom_point(size=0.5) + geom_abline(slope=1, intercept=0) + theme_bw() + coord_fixed()
		if (is.null(xlimits)){
			return(p0)
		} else {
			return(p0 + xlim(xlimits[1], xlimits[2]) + ylim(xlimits[1], xlimits[2]))
		}
	})
 	figureGrob <- arrangeGrob(grobs = force_matching)
	return(figureGrob)
}