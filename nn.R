#some magic number for start. They doesn't have any sence for now.
input_layer_size  <- 400    # 20x20 Input Images of Digits
num_labels <- 10            # 10 labels, from 1 to 10   

predict <- function(theta1, x) {
    for(hi in num_labels) {
        a1 <- neuronFunc(x, theta1[hi,])
    }
}

neuronFunc <- function(theta, x) {
    x * theta
}