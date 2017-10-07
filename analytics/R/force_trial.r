forcetrial <- function(adept_x,adept_y,timeseries_df, full_df, stability_info=NULL)
{
      
      forcetrial_structure_object <- structure(timeseries_df, adept_x=adept_x, adept_y=adept_y, stability_info=NULL)
      return(forcetrial_structure_object)
}
