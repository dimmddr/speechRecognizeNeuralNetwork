# init <- function(input_size = 28 * 28, hidden_neurons_count = 15, output_size = 10, a = 1) {
#     #some magic number for start. They doesn't have any sence for now.
#     input_layer_size  <<- input_size                # 28x28 Input Images of Digits
#     hidden_layer_size <<- hidden_neurons_count      # hidden layer size
#     num_labels <<- output_size                      # 10 labels, from 1 to 10   
#     alfa <<- a                                      # learning speed
# }
# 
# 
# ## Function for predict value after learning
# #   theta1 - matrix of weights for all neurons in hidden layer
# #       [hidden_layer_size, input_layer_size]
# #   theta2 - matrix of weights for all neurons in output layer
# #       [num_labels, hidden_layer_size]
# #   x - input data
# #   return integer value - recognizible symbol
# predict <- function(x, theta1, theta2) {
#     x <- as.numeric(x_input[x_i,])
#     #   z_in - output from hidden layer neurons (and input into output layer)
#     z_in <- c()
#     for(hi in 1:hidden_layer_size) {
#         z_in <- c(z_in, neuronFunc(theta1[, hi], x))
#     }
#     z <- sigmoid(z_in)
#     z <- c(1, z)
#     #   y_in - output from output layer neurons
#     y_in <- c()
#     for(oi in 1:num_labels) {
#         y_in <- c(y_in, neuronFunc(theta2[, oi], z))
#     }
#     y <- sigmoid(y_in)
#     which.max(y)
# }
# 
# ##Calculation of weighed sum
# neuronFunc <- function(theta, x) {
#     sum(x * theta)
# }
# 
# ##Activation function
# sigmoid <- function(x) {
#     1 / (1 + exp(-x))
# }
# 
# ##Derivative of activation function
# sigmoidDeriv <- function(x) {
#     q <- sigmoid(x)
#     q * (1 - q)
# }
# 
# learning <- function(x_input, y_input) {
#     ##  x - examples
#     ##  y - labels
#     set.seed(16)    #magic number for seed.
#     #Clean and random set weights
#     theta1 <- c()
#     theta2 <- c()
#     x_input <- cbind(1, x_input)
#     for(i in 1:(input_layer_size + 1)) {
#         theta1 <- rbind(theta1, runif(hidden_layer_size, -0.5, 0.5))
#     }
#     for(i in 1:(hidden_layer_size + 1)) {
#         theta2 <- rbind(theta2, runif(num_labels, -0.5, 0.5))
#     }
#     
#     for(x_i in 1:nrow(x_input)) {
#         t <- rep(0, num_labels)
#         t[y_input[x_i]] <- 1
#         x <- as.numeric(x_input[x_i,])
#         #   z_in - output from hidden layer neurons (and input into output layer)
#         z_in <- c()
#         for(hi in 1:hidden_layer_size) {
#             z_in <- c(z_in, neuronFunc(theta1[, hi], x))
#         }
#         z <- sigmoid(z_in)
#         z <- c(1, z)
#         #   y_in - output from output layer neurons
#         y_in <- c()
#         for(oi in 1:num_labels) {
#             y_in <- c(y_in, neuronFunc(theta2[, oi], z))
#         }
#         y <- sigmoid(y_in)
#         
#         sigma2 <- errCalc(t - y, y_in)
#         
#         #TODO: vectorize it!!!
#         for(oi in 1:num_labels) {
#             theta2[, oi] <- theta2[, oi] + alfa * sigma2[oi] * z
#         }
#         
#         sigma_in <- c()
#         for(hi in 2:(hidden_layer_size + 1)) {
#             sigma_in <- c(sigma_in, neuronFunc(theta2[hi, ], sigma2))
#         }
#         
#         sigma1 <- errCalc(sigma_in, z_in)
#         for(hi in 1:hidden_layer_size ) {
#             theta1[, hi] <- theta1[, hi] + alfa * sigma1[hi] * x
#         }
#     }
#     
#     list(theta1, theta2)
# }
# 
# errCalc <- function(err, y_in) {
#     # err - error
#     # y_in - weighed sum for output neuron layer
#     err * sigmoidDeriv(y_in)
# }



input_layer_size <- 28 * 28
hidden_layer_size <- 15
output_layer_size <- 10
alfa <- 1
theta1 <- matrix(0, nrow = hidden_layer_size, ncol = input_layer_size)
theta2 <- matrix(0, nrow = output_layer_size, ncol = hidden_layer_size)

init <- function(input_size = 28 * 28, hidden_size = 15, output_size = 10, alfa_ = 1, seed = 16) {
    input_layer_size <<- input_size
    hidden_layer_size <<- hidden_size
    output_layer_size <<- output_size
    alfa <<- alfa_
    set.seed(seed)
    theta1 <- matrix(0, nrow = hidden_layer_size, ncol = input_layer_size)
    theta2 <- matrix(0, nrow = output_layer_size, ncol = hidden_layer_size)
    
}