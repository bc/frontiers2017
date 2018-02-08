context('posture dependency figures for a matrix fits')
set.seed(4)
#parameters
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX","JR3_FY","JR3_FZ","JR3_MX","JR3_MY","JR3_MZ")
range_tension <-  c(0,10)
samples <- hand3_hand4_clean_static_samples()
A_fit_list <- calculate_and_display_A_fit_per_sample(samples, muscles_of_interest, force_names_to_predict, range_tension)
fit_evaluation_without_offset(A_fit_list[[1]], A_fit_list[[1]]$test_data)

 a_fits <- lapply(samples, function(hand_at_posture){
  evaluate_rmse_of_A_fit(A_fit_from_80_20_split(hand_at_posture, muscles_of_interest, force_names_to_predict))
})
dataset_and_Afitrmse <- rev(melt(a_fits))
dataset_and_Afitrmse$hand <- as.character(c(rep(3,4), rep(4,4)))
dataset_and_Afitrmse$posture <- as.factor(c('ultraextend',
'extend',
'flex',
'ultraflex',
'ultraextend',
'extend',
'flex',
'ultraflex'))
 rmse_set_ordered <- dataset_and_Afitrmse[c(4,8,3,7,2,6,1,5),]

 rmse_set_ordered_factors <- transform(rmse_set_ordered,
       posture=factor(posture,levels=c('ultraflex',
       'flex',
       'extend',
       'ultraextend')))


p <- ggplot(rmse_set_ordered_factors,aes(hand, value))
p <- p + geom_col(aes(fill=hand))
p <- p + facet_grid(~posture)
p <- p + ylab('RMSE of static model against test data (N)')
p <- p + geom_text(aes(label=sprintf("%0.2f", round(value, digits = 2))),hjust=0.45, vjust=1.2, col="#ffffff")
ggsave(to_output_folder("rmse_per_posture_per_hand.pdf"), p, width=5, height=5, limitsize=FALSE)
