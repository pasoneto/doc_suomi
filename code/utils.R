library(ggplot2)
library(dplyr)
library(randomForest)
library(foreign)
library(caret)
library(keras)
library(data.table)
library(cluster)
library(DescTools)
library(lattice)
library(magrittr)
library(tensorflow)

set.seed(2020)
#Functions
z <- function(x){ 
    return((x-mean(x))/sd(x))
    }

rounder <- function(x){  
    ifelse(x %% 1 == .5, sample(c(floor),1)[[1]](x), round(x))
    }

segment <- function(x) {
    segment = rounder(length(x)/3)
    sections = c(rep("1st",segment), rep("2nd", segment), rep("3d", length(x)-segment*2)) 
    return(sections)
    }

segment2 <- function(x) {
    segment = rounder(length(x)/4)
    sections = c(rep("1st",segment), rep("2nd", segment), rep("3d", segment), rep("4th", length(x)-segment*3)) 
    return(sections)
    }

count <- function(x){ 
    count = 0 
    for(i in x){ if(i == 1){count = count+1}} 
    return(count/length(x)) 
    }

#################################################
############ Dissimilarity matrices #############
#################################################

dissim_gen = function(dt){
    for(i in 1:length(dt)) { 
        dt[[i]] = tidyr::pivot_wider(dt[[i]], 
                  names_from = album_id, 
                  values_from = c(valence, energy, loudness, tempo)) 

        dt[[i]] = dt[[i]][, 2:5] #selecting only columns related to features.

        dt[[i]] <- as.matrix(dt[[i]]) 

        dt[[i]] <- as.matrix(daisy(dt[[i]]))

        dt[[i]][dt[[i]] == 0] <- NA

    }
    return(matrix(dt))
}

album_splitter = function(data){
    dt %<>% dplyr::select(track_number, valence, energy, loudness, tempo) %>%
        split(data$album_id) 
    return(dt)
}

#element-wise matrix calculator
matrix_parser <- function(matrix_list, FUN){
    return(apply(simplify2array(matrix_list), 1:2, function(x){FUN(x)}))
}

# dt = fread("novo.csv")
# dt %<>%
#     group_by(album_id) %>% mutate(album_length = NROW(track_number)) %>%
#     dplyr::filter(album_length %in% c(6:15)) %>% 
#     dplyr::select(album_id, track_number, valence, energy, loudness, tempo) %>%
#     mutate(valence  = z(valence),  energy   = z(energy), loudness = z(loudness), tempo    = z(tempo),
#         position = segment2(track_number)) %>%
#     group_by(album_id, position)%>%
#     summarise(valence = mean(valence), energy = mean(energy),
#               loudness = mean(loudness), tempo = mean(tempo)) %>% ungroup()
# dt$position <- relevel(as.factor(dt$position), ref = "1st")

#CROSS VALIDATION MODULES
treino_teste <- function(dados){
    dados = split(dados, dados$album_id)
    size <- floor(0.75 * length(dados))
    train_ind <- sample(seq_len(length(dados)), size = size, replace = FALSE)
    dt_train <- dados[train_ind]; dt_train <- bind_rows(dt_train)
    dt_test <- dados[-train_ind]; dt_test <- bind_rows(dt_test)
    
    return(list(train = dt_train, test = dt_test))
}

#dados = a list of training and test data set; formulinha: string, FUN: model to use
model_build <- function(dado, FUN, var_int, var_pred){
    formulinha = as.formula(paste(var_int, var_pred, sep = "~"))
    model = FUN(formulinha, data = dado$train) 
    return(model)
}

#model_build(treino_teste(dt), lm, "valence_next", "valence+energy+loudness+tempo")

cross_val <- function(dataframe, FUN, var_int, var_pred, n_runs){
    p = c(); v = c(); rmserror = c()
    for(i in 1:n_runs){
        dado = treino_teste(dataframe)
        modelo = model_build(dado, FUN, var_int, var_pred)
        predicted = unname(predict(modelo, dado$test))
        true = c(dado$test[, var_int])[[1]]
        p=c(p, predicted)
        v=c(v, true)
        rmserror[[i]] = Metrics::rmse(p, v)
    }
    pv = data.frame(p = p, v = v)
    rmse_error = mean(rmserror, na.rm = TRUE)
    rmse_stde = sd(rmserror, na.rm = TRUE)/sqrt(length(rmserror))
    final = list(mean_rmse = rmse_error, stde_rmse = rmse_stde, pred_ver = pv)
    return(final)
}

minmax <- function(x) { return((x- min(x)) /(max(x)-min(x))) }

album_random = function(){
    inicios = seq(1,NROW(teste)-15, 15)
    i = sample(inicios, 1)
    return(seq(i, (i+15), 1))
    }

zrule = function(data, var_interest, var_pred){
    true = data[, var_interest]
    pred = mean(data[, var_interest])
    return(pred)
}

##################

#LSTM setup
# model = keras_model_sequential() %>% 
#     layer_lstm(
#         units = 124,
#         batch_input_shape = c(1, 1, 8),
#         dropout = 0.2,
#         recurrent_dropout = 0.5,
#         return_sequences = TRUE,
#         stateful = TRUE
#   ) %>%

#    layer_dense(units=16, activation="linear") %>%
#    layer_lstm(units = 8,return_sequences = TRUE, stateful = FALSE) %>%
#    layer_dense(units=1, activation="linear")
 
# model %>% compile(
#    loss = "mse",
#    optimizer =  "adam", 
#    metrics = list("mean_absolute_error")
#  )

# # Function receives data and returns fitted model
# lstm = function(var_interest, var_pred, model, treino, teste){
#         nfeatures = ncol(treino[, pred_vars])
#         y.treino = array(treino[, var_interest], dim = c(nrow(treino), 1, 1))
#         x.treino = array(treino[, var_pred], dim = c(nrow(treino), 1, 8))
#         y.teste =  array(teste[, var_interest], dim = c(1, 1, nrow(teste)))
#         x.teste =  array(teste[, var_pred], dim = c(1, nfeatures, nrow(teste)))
    
#     model %>% fit(x.treino, 
#                   y.treino, 
#                   epochs = 100,verbose = 0)
#     return(model)
# }

# model_eval = function(model_fitted, x.teste, y.teste){
#     pre = predict(model_fitted, x.teste)
#     return(Metrics::rmse(pre, y.teste))    
# }

##################

# #### data LSTM
# inter = fread("interpolated/nn_interpol.csv")
# colnames(inter) <- c("index", "valence", "energy", "loudness", "tempo", "album_id", "track_number")

# lstm_data = inter %>% group_by(album_id) %>%
#             mutate(position = segment(track_number)) %>%
#             group_by(album_id, position) %>%
#             mutate(track_number = seq(1, NROW(track_number), 1), 
#                    position = as.factor(position)) %>% ungroup() %>%
#             mutate(valence = minmax(valence), energy = minmax(energy), 
#                    loudness = minmax(loudness), tempo = minmax(tempo), 
#                    track_number = minmax(track_number)) %>% ungroup() %>% group_by(album_id) %>%
#             mutate(valence_next = shift(valence, -1), energy_next = shift(energy, -1), 
#                    loudness_next = shift(loudness, -1), tempo_next = shift(tempo, -1)) %>%
#             ungroup() %>% na.omit() %>%
#             mutate(position = as.factor(position)) %>%
#             fastDummies::dummy_cols(select_columns = "position", remove_selected_columns = TRUE)

# #Train test split
# treino_lstm = treino_teste(lstm_data)$train
# teste_lstm = treino_teste(lstm_data)$test

# treino_lstm = apply(as.matrix(lstm_data), 2, as.numeric)
# teste_lstm = apply(as.matrix(lstm_data), 2, as.numeric)

r2 = function(var_interest, pred_vars, model, data){
        pred = predict(model, data[, pred_vars])
        true = data[, var_interest]
    
        rss <- sum((pred - true) ^ 2)
        tss <- sum((true - mean(true)) ^ 2)
    
        r2 = 1-rss/tss
    
    return(r2)
}
setwd("/home/pasoneto/Documents/github/doc_suomi/code")