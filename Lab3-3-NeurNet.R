#Function: calculates the number of weights in a normal neural network given the hidden
numberOfWeights <- function(nInputs, hiddenLayerVec, nOutputs) {
  nodeNumPrev <- nInputs
  weightNum <- 0
  
  for (nodeNum in hiddenLayerVec) {
    weightNum <- weightNum + (nodeNumPrev+1)*nodeNum
    nodeNumPrev <- nodeNum
  }
  
  weightNum <- weightNum + (nodeNumPrev+1)*nOutputs
  return(weightNum)
}

#Add the neutralnet package
library(neuralnet)

# Train a neural network to learn the trigonometric sine function. To do so, sample 500 points
# uniformly at random in the interval [0;10]. Apply the sine function to each point. The resulting
# pairs are the data available to you. Use 25 of the 500 points for training and the rest for test.
# Use any number of layers and hidden units that you consider appropriate. You do not need to
# apply early stopping. Plot the training and test data, and the predictions of the learned NN on
# the test data.
set.seed(1234567890)
Var <- runif(500, 0, 10) #500 random values between 0 and 10
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

hiddenNodes <- c(5,5) #Number of elements specifies how many hidden layers, while the elements specify how many neurons in each layer
weightNum <- numberOfWeights(nInputs = 1, hiddenLayerVec = hiddenNodes, nOutputs = 1)
winit <- runif(weightNum, -0.1,0.1) #Set random (low) start values on the weights
nn <- neuralnet(data = mydata, formula = Sin ~ Var, hidden = hiddenNodes, act.fct = "tanh", startweights = winit, rep = 10)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)


# Then, sample 500 points uniformly at random in the interval [0;20], and apply the sine
# function to each point. Use the previously learned NN to predict the sine function value for
# these new 500 points.

set.seed(1234567890)
Var <- runif(500, 0, 20)
mydata2 <- data.frame(Var, Sin = sin(Var))
plot(mydata2, col = "blue")
points(mydata2[,1], predict(nn,mydata2), col = "red", cex = 1)
points(tr, cex = 2)

# Finally, sample 500 points uniformly at random in the interval [0;10], and apply the sine
# function to each point. Use all these points as training points for learning a NN that tries to
# predict x from sin(x), i.e. unlike before when the goals was to predict sin(x) from x.

set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata3 <- data.frame(Sin = sin(Var), Var)
hiddenNodes3 <- c(5,5)
weightNum3 <- numberOfWeights(nInputs = 1, hiddenLayerVec = hiddenNodes3, nOutputs = 1)
winit3 <- runif(weightNum3, -0.1, 0.1) #Set random (low) start values on the weights

nn3 <- neuralnet(data = mydata3, Var ~ Sin, hidden = hiddenNodes, act.fct = "tanh", startweights = winit3, rep = 10)
plot(mydata3, col = "blue")
# points(mydata3[,1], predict(nn3, mydata3), col = "red", cex = 1) 
# Error in the package, see Fix bug when some replications don't converge #21 below
#https://stackoverflow.com/questions/56254321/error-in-ifncol-matrix-rep-argument-is-of-length-zero
