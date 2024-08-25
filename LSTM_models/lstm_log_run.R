
FLAGS = flags(flag_integer("lstm_units", 1),
              #flag_integer("lstm_units2",1),
              #flag_numeric("l_rate", 0.1),
              flag_integer("epoch",1))

model_st <- keras_model_sequential() %>%
  layer_lstm(units = FLAGS$lstm_units, 
             batch_input_shape = c(batch_size, X_shape2[[1]], X_shape3[[1]]), 
             return_sequences = T,
             stateful= TRUE
             )%>%
  #layer_dropout(rate = FLAGS$l_rate) %>% 
  #layer_lstm(units = FLAGS$lstm_units2,
             #return_sequences = T,
            # stateful = T) %>% 
  #layer_dropout(rate = FLAGS$l_rate) %>% 
  layer_dense(units = 1 ) %>% 
  compile(
    loss = 'mean_squared_error',
    optimizer = "adam"  
  ) %>% 
  fit(
    x = lstm_train_st_laglog[[1]]
    ,y = lstm_train_st_log[[1]]
    ,batch_size = batch_size
    ,epochs = FLAGS$epoch
    ,shuffle = FALSE)
