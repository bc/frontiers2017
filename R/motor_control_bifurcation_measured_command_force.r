hand3_ultraflex <- read.csv("/Users/briancohn/Documents/GitHub/bc/frontiers2017/output/calibrated_and_0_centered_input_output_data_for_hand3_ultraflexed.csv")
M0 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M0, measured_M0))
M1 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M1, measured_M1))
M2 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M2, measured_M2))
M3 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M3, measured_M3))
M4 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M4, measured_M4))
M5 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M5, measured_M5))
M6 <- ggplot(as.data.frame(hand3_ultraflex)) + geom_point(aes(command_M6, measured_M6))

multiplot <- arrangeGrob(grobs=list(M0,M1,
M2,
M3,
M4,
M5,
M6)
)
