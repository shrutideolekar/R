# set default flags
FLAGS <- flags(
  flag_numeric("nodes1", 290),
  flag_numeric("nodes2", 74),
  flag_numeric("lr", 0.01)
)

# model configuration
model <- keras_model_sequential() %>%
  layer_dense(units = 1134, input_shape = V, activation = "relu", name = "layer_1") %>% 
  layer_dense(units = FLAGS$nodes1, activation = "relu", name = "layer_2") %>% 
  layer_dense(units = FLAGS$nodes2, activation = "relu", name = "layer_3") %>%
  layer_dense(units = ncol(y_train),activation = "softmax",name = "layer_out") 

# Compiling the NN model
model %>% compile(loss = "categorical_crossentropy",metrics = "accuracy",
                  optimizer = optimizer_sgd(lr = FLAGS$lr),)

# Training the NN model
fit <- model %>% fit(x = x_train, y = y_train, 
                     validation_data = list(x_val_tun, y_val_tun),
                     epochs = 100, verbose = 1, callbacks = callback_early_stopping
                     (monitor = "val_accuracy", patience = 20))

# Evaluation <- store accuracy on test set for each run
score <- model %>% evaluate(x_test_tun, y_test_tun, verbose = 0)
