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
                mutate(section = segment(track_number)) %>%
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

upsampled_album = function(){
        file_list <- list.files(path="/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums")
        matrices = c()

        for(i in 1:length(file_list)){
                file = paste("/home/pasoneto/Documents/github/doc_suomi/code/data_processing/upsampled_albums/", file_list[i], sep = "")
                matrices[[i]] = fread(file, header = TRUE)
        }
        
        # for(i in 1:length(file_list)){
                
        #         if(sum(dim(matrices[[i]])) != 22){
        #                 print(i)
        #         }

        matrices[[3831]] <- NULL
        
        matrices = bind_rows(matrices)
        return(matrices)
        }

list_upsampled = function(){
        file_list <- list.files(path="/home/pasoneto/Documents/github/doc_suomi/data/dissimilarities/upsampled_bilinear")
        matrices = c()

        for(i in 1:length(file_list)){
                file = paste("/home/pasoneto/Documents/github/doc_suomi/data/dissimilarities/upsampled_bilinear/", file_list[i], sep = "")
                matrices[[i]] = fread(file, header = TRUE)
        }
        matrices[[3831]] <- NULL
        return(matrices)
        }

low_z = function(){
        low = fread("/home/pasoneto/Documents/github/doc_suomi/data/treated_data/data_low.csv")
        low %<>%
                select(!V1) %>% 
                group_by(album_id) %>%
                mutate(danceability = z(danceability), 
                energy = z(energy), 
                loudness = z(loudness_overall), 
                speechiness = z(speechiness), 
                acousticness = z(acousticness), 
                instrumentalness = z(instrumentalness), 
                liveness = z(liveness), 
                valence = z(valence), 
                tempo_overall = z(tempo_overall),
                time_signature = z(time_signature),
                loudness_continuouos = z(loudness_continuous), 
                tempo_continuous = z(tempo_continuous), 
                position = segment2(track_number),
                album_length = length(unique(track_number))) %>%
                filter(album_length %in% c(6:16))
        return(low)
        }


low_raw = function(){
        low = fread("/home/pasoneto/Documents/github/doc_suomi/data/treated_data/data_low.csv")
        low %<>% select(!V1)
        return(low)
        }

low_minmax = function(){
        low = fread("/home/pasoneto/Documents/github/doc_suomi/data/treated_data/data_low.csv")
        low %<>% select(!V1) %>%
                 #group_by(album_id) %>%
                 mutate_if(is.numeric, function(x){return(minmax(x, 1, 2))})
        return(low)
        }
      
howto_data = 
        paste("How to use datasets",
                " ",
                "call    base()             for real values", 
                "call    z_scored()         for normalized", 
                "call    min_maxed()        for normalized2", 
                "call    upsampled_album()  for binded upsamplped albums", 
                "call    list_upsampled()   for list of upsampled albums",
                "call    low_z()            for normalized low level",
                "call    low_raw()          for raw  low level", sep = "\n")
