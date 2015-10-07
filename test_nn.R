test_nn <- function() {
    source('nn.R')
    if(!exists("train")) {
        train <- read.csv("train.csv")
    }
    proc <- c()
    for(hl in seq.int(15, 50, 5)) {
        init(input_size = 28 * 28, hidden_size = 45, output_size = 10, alfa_ = 1, seed = hl)
        set.seed(16)
        l <- nrow(train)
        train_ind <- sample(seq_len(l), size = floor(l * 0.75))
        train_set <- train[train_ind, ]
        test_set <- train[-train_ind, ]
        
        lbl_train <- train_set[, 1]
        train_set[, 1] <- NULL
        lbl_test <- test_set[, 1]
        test_set[, 1] <- NULL
        lbl_test[lbl_test == 0] <- 10
        lbl_train[lbl_train == 0] <- 10 
        
        learning(x = train_set, lbl = lbl_train)
        res <- apply(test_set, 1, predict)
        s <- sum(res == lbl_test)
        proc_n <- s/length(lbl_test)
        print(hl)
        print(proc_n)
        proc <- c( proc, proc_n )
    }
    proc
}