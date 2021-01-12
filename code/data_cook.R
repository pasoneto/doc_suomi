setwd("/home/pasoneto/Documents/github/doc_suomi/data/treated_data")

raw = fread("data.csv") %>% dplyr::select(!V1)
raw %<>% group_by(album_id, track_number) %>%
        summarise(valence = mean(valence), energy = mean(energy), 
                  loudness = mean(loudness), tempo = mean(tempo)) %>%
        mutate(album_length = NROW(track_number)) %>% ungroup() %>%
        filter(album_length %in% c(6:16))

############### Base for work
base = function(){
        raw %<>% group_by(album_id) %>% 
                mutate(album_length = NROW(track_number), 
                       section = segment(track_number)) %>%
                dplyr::select(album_id, album_length, track_number, valence, energy, loudness, tempo)
        return(raw)
}

z_scored = function(){
        raw %<>% group_by(album_id) %>%
        dplyr::select(album_id, album_length, track_number, valence, energy, loudness, tempo) %>%
        mutate(album_length = NROW(track_number), section = segment(track_number), 
               valence = z(valence), energy = z(energy), 
               loudness = z(loudness), tempo = z(tempo) ) %>%
        group_by(track_number, album_id) %>%
        arrange(album_id) %>% ungroup()
        return(raw)
}

min_maxed = function(){
        raw %<>% group_by(album_id) %>%
        dplyr::select(album_id, album_length, track_number, valence, energy, loudness, tempo) %>%
        mutate(section = segment(track_number), valence = minmax(valence),
               energy = minmax(energy), loudness = minmax(loudness), tempo = minmax(tempo) ) %>%
        group_by(track_number, album_id) %>%
        arrange(album_id) %>% ungroup()
        return(raw)
}

dissim_matrix = function(){
        dado = fread("dissim_matrix.csv", header = TRUE)
        colnames(dado) <- as.character(seq(1, 16, 1))
        rownames(dado) <- as.character(seq(1, 16, 1))
        return(dado)
}

upsampled_album = function(){
        file_list <- list.files(path="/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums")
        matrices = c()

        for(i in 1:length(file_list)){
                file = paste("/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums/", file_list[i], sep = "")
                matrices[[i]] = fread(file, header = TRUE)
        }
        matrices[[5416]] <- NULL
        matrices = bind_rows(matrices)
        return(matrices)
        }

list_upsampled = function(){
        file_list <- list.files(path="/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums")
        matrices = c()

        for(i in 1:length(file_list)){
                file = paste("/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums/", file_list[i], sep = "")
                matrices[[i]] = fread(file, header = TRUE)
        }
        matrices[[5416]] <- NULL
        return(matrices)
        }

list_dissim = function(){
        file_list <- list.files(path="/home/pasoneto/Documents/github/doc_suomi/code/data_processing/dissimilarity_matrices")
        matrices = c()

        for(i in 1:length(file_list)){
                file = paste("/home/pasoneto/Documents/github/doc_suomi/code/data_processing/dissimilarity_matrices/", file_list[i], sep = "")
                matrices[[i]] = fread(file, header = TRUE)
        }
        matrices[[5416]] <- NULL
        return(matrices)
        }


paste("How to use datasets",
        " ",
        "call    base()             for real values", 
        "call    z_scored()         for normalized", 
        "call    min_maxed()        for normalized2", 
        "call    dissim_matrix()    for dissimilarities", 
        "call    upsampled_album()  for binded upsamplped albums", 
        "call    list_upsampled()   for list of upsampled albums",
        "call    list_dissim()      for list of disssimilarities" ,sep = "\n")
