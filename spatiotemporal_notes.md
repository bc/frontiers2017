For each hand:

Spatiotemporal tunneling:
  Parallel uar activation - 500 points
  Replicability test - 5 points x 100 replicates (500 total points)
  Serial uar activation - 500 points
  Rampup, Rampdown 1536 points

Euclidian 6D JR3_to_fingertip_distance
residuals for LRM, NN
50 error points for each of them.
 lrm_vals <- mean(residuals_LRM)
 nn_vals  <- mean(residuals_NN)
x tensions
b wrench
 NN_b_to_x(b0) == LRM_b_to_x(b0)

 b len = 10
 you give me

 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)
 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)
 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)
 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)
 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)
 nn (data, nhidden1, nhidden2, summation_parameters, "Relu, sigmoid, tan", biases)

NN.(x to b) outperforms LRM by 50%

mean(residuals_NN)*1.5 = mean(residuals_LRM)
left is 50% higher than residuals_LRM

b to x NN is worse than linear model, unless you increase the layers.
it can only outperform it slightly
harder to solve from forces to activations

x to b is easier to learn compared to b to x

Would a NN perform better at specific areas of the FAS?
There are multiple areas in which our results can land. Since neural networks are range-sensitive
to the training set, that could be why b to A is not as effective.
But the linear mapping gets the value, so it might be good for all
spaces that hold within the same mapping.

Use LRM with values outside of observed ranges of training data.



NN tasks len 10 x's
LRN tasks len 10 x's





  inputs = x (muscle forces, 7-dimensional)
  model = NN, LRM, AMATRIX
  outputs = b (output forces, 6 dimensional)
b0 = (1.0,2.0,0.0,0.0,0.0,0.0)
 b = [b0, b2, b3, ..., b9]
  NN(b[0]) = x_to_check
  x_to_check should be length 10, where each element has 7 values between 0 and 10N.

  x0 = x_to_check[0]

  b_{0_empirical}<- plug_into_hand(x0)

  thats the error => abs(b0 - b0_empirical)

Cool things to try in the spatiotemporal paper:



  1. CDMRP Implant Study
for each of these conditions:
    Implant
    No implant
      {
        add force to the tendons of the flexor of index + middle
        adept spins an object that forces the fingers to twist
      }

      2. Voluntary Drive;
      populations of neurons
        FPGA's run code with gamma static, gamma dynamics, params etc.
          does a grid search

          index finger




Mean Error in FX = 0.7N
10,10,10,10,10,10,10 --> FX = [2.0N, 2.5N, 1.5N, 1.75N]

Our mean error with respect to the variance in the response variable Fx shows
that we are very accurate. Accurate means that our model fit error is close to
the variance in the error of the response variable



Ways I'm going to extract the generators

parallel uar activation - 500 points
serial uar activation - 500 points
serial rampup-rampdown activation - 7 points, 3 levels per point.
differential generator extraction: 30 points, 3 levels per point. (-10%, 0, +10%)
