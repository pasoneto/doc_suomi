setwd("/home/pasoneto/Documents/github/doc_suomi/data/treated_data")

raw = fread("data.csv") %>% dplyr::select(!V1)
raw %<>% group_by(album_id, track_number) %>%
        summarise(valence = mean(valence), energy = mean(energy), 
                  loudness = mean(loudness), tempo = mean(tempo)) %>%
        mutate(album_length = NROW(track_number)) %>% ungroup()

############### Base for work
base = function(){
        raw %<>% group_by(album_id) %>% 
                mutate(album_length = NROW(track_number), 
                       section = segment(track_number)) %>%
                filter(album_length %in% c(6:16)) %>%
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

howto_data = paste("How to use datasets",
                   " ",
                   "call    base()          for real values", 
                   "call    z_scored()      for normalized", 
                   "call    min_maxed()     for normalized2", 
                   "call    dissim_matrix() for dissimilarities", sep = "\n")