context('posture dependency figures for a matrix fits')
set.seed(4)
#parameters
input_output_data <- hand3_hand4_clean_static_samples()$hand3_ultraflex
muscles_of_interest <- muscle_names()
num_muscles <- length(muscles_of_interest)
force_names_to_predict <- c("JR3_FX", "JR3_FY", "JR3_FZ", "JR3_MX", "JR3_MY", "JR3_MZ")
range_tension <- c(0, 10)
samples <- hand3_hand4_clean_static_samples()
a_fits <- lapply(samples, function(hand_at_posture) {
    mean_euclidian_error_against_test_set(A_fit_from_80_20_split(hand_at_posture,
        muscles_of_interest, force_names_to_predict))
})
test_that("we can produce the euclidian residual across posture", {

    dataset_and_Afitrmse <- rev(melt(a_fits))
    dataset_and_Afitrmse$hand <- as.character(c(rep(3, 4), rep(4, 4)))
    dataset_and_Afitrmse$posture <- as.factor(c("ultraextend", "extend", "flex",
        "ultraflex", "ultraextend", "extend", "flex", "ultraflex"))
    euclidian_err_set_ordered <- dataset_and_Afitrmse[c(4, 8, 3, 7, 2, 6, 1, 5),
        ]
    euclidian_err_set_ordered_factors <- transform(euclidian_err_set_ordered, posture = factor(posture,
        levels = c("ultraflex", "flex", "extend", "ultraextend")))
    p <- ggplot(euclidian_err_set_ordered_factors, aes(hand, value))
    p <- p + geom_col(aes(fill = hand))
    p <- p + facet_grid(~posture)
    p <- p + ylab("Mean euclidian residual (N). n=60") + ggtitle("static model performance across posture")
    p <- p + geom_text(aes(label = sprintf("%0.2f", round(value, digits = 2))), hjust = 0.5,
        vjust = 1.2, col = "#ffffff")
    ggsave(to_output_folder("static_A_mean_euclidian_error_per_posture_per_hand_against_test_set.pdf"),
        p, width = 5, height = 5, limitsize = FALSE)
})

test_that('We can produce a plot containing all the boxplots for the MSE of certain numbers of trials', {
   get_sample_size_histograms(input_output_data, muscles_of_interest, force_names_to_predict, 5, 300)
})
