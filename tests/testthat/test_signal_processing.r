test_that("sine waves are attenuated when the stop frequency is lower than the frequency", {
  x_indices = seq(0,20*pi, length.out=1000)
  signal_y <- sin(x_indices)
  par(mfrow=c(2,1))
  plot(1:1000,signal_y, type='l', xlab="time (ms)", ylab="Amplitude", main="Low pass filtered result is gray")

  signal_y_lpf<- lpf(signal_y, 0.001, 9)
  lines(signal_y_lpf, col="gray")
  expect_equal(max(abs(lpf(signal_y, 0.001, 50))),1,tol=1e-1)
  expect_true(max(abs(lpf(signal_y, 0.001, 9))) < 0.8)
  expect_true(max(abs(lpf(signal_y, 0.001, 1))) < 0.2)

  f_stops_to_check <- seq(0,100, length.out=100)
  max_amplitudes <- lapply(f_stops_to_check, function(x){
    max(abs(lpf(signal_y, 0.001, x)))
  })
  #with fixed sine wave frequency at 10Hz, show the rolloff.
  plot(f_stops_to_check, max_amplitudes, type='l', xlab="Critical frequency for lpf (stop frequency, cutoff frequency) (Hz)", ylab="Max amplitude of resultant signal", main="Input frequency is 10Hz")
  abline(v=10)
})
