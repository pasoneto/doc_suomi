setwd("/home/pasoneto/Documents/Ciência/PhD/data")

############### Dissimilarity matrix data


dissim_matrix = fread("no_duplicate.csv")
dissim_matrix %<>% group_by(album_id) %>% mutate(track_number = seq(1,length(album_id), 1), 
                                                 album_length = NROW(track_number)) %>%
        filter(album_length %in% c(6:16)) %>%
        dplyr::select(track_number, album_id, valence, energy, loudness, tempo)

dissim_matrix %<>% group_by(album_id) %>%
        dplyr::select(album_id, track_number, valence, energy, loudness, tempo) %>%
        mutate(valence = z(valence) , energy = z(energy), 
               loudness = z(loudness), tempo = z(tempo) ) %>%
        group_by(track_number, album_id) %>%
        arrange(album_id) %>% ungroup()