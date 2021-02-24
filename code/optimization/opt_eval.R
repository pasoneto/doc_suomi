setwd("/home/pa/Documents/github/doc_suomi/code")
source("utils.R")
source("data_cook.R")
cat(howto_data)

original = fread("/home/pa/Documents/github/doc_suomi/data/optimization/original.csv")
reordered = fread("/home/pa/Documents/github/doc_suomi/data/optimization/reordered.csv")
control = album_splitter(original)
for(i in 1:length(control)){ control[[i]] = special_shuffler(control[[i]]) }
control = bind_rows(control)


junto = dplyr::bind_cols(original, reordered, control)
colnames(junto) = c("V1", "album_id1", "track_number_original", "valence_original", "energy_original", "loudness_original", "tempo_original",
                    "V2", "album_id2", "track_number_reor",     "valence_reor",     "energy_reor",     "loudness_reor", "tempo_reor",
                    "V3", "album_id3", "track_number_control", "valence_control", "energy_control", "loudness_control", "tempo_control")
junto %<>% 
    group_by(album_id1) %>%
    summarise(model_valence = match(valence_original, valence_reor), 
              model_energy = match(energy_original, energy_reor),
              model_loudness = match(loudness_original, loudness_reor),
              model_tempo = match(tempo_original, tempo_reor),      
              control_valence = match(valence_original, valence_control),
              control_energy = match(energy_original, energy_control),
              control_loudness = match(loudness_original, loudness_control),
              control_tempo = match(tempo_original, tempo_control))
              
              
junto %<>% melt(id.vars = c("album_id1"), 
                measure.vars = c("model_valence", 'model_energy', 
                                 "model_loudness", "model_tempo", 
                                 "control_valence", "control_energy", 
                                 "control_loudness", "control_tempo")) %>%
            separate(variable, c("condition", "feature"), "_")

junto %>% 
    ggplot(aes(x = value, fill = condition)) +
           facet_wrap(~feature)+
           geom_density(alpha = 0.8)

########################################
dd = album_splitter(z_scored())
dd = dd[round(length(dd)*0.8):length(dd)]
dd = dplyr::bind_rows(dd)
dd %<>% filter(album_id %in% unique(reordered$album_id)) %>% arrange(album_id)


reordered %<>% filter(album_id %in% unique(dd$album_id)) %>% arrange(album_id)


control = album_splitter(dd)
for(i in 1:length(control)){ control[[i]] = special_shuffler(control[[i]]) }
control = bind_rows(control)

model = left_join(reordered, dd, by=c("album_id", "track_number"))
control = left_join(control, dd, by=c("album_id", "track_number"))
comp = data.frame(valence_model = model$valence.y, 
                  valence_original = dd$valence,
                  valence_control = control$valence.x, 
                  album_id = dd$album_id)

comp = album_splitter(comp)

final = c()
for(i in 1:length(comp)){
    comp[[i]] %>% select(-album_id) %>% t() %>% data.frame() %>% daisy() -> oi
    final[[i]] = data.frame(as.matrix(oi))
}

model_eval = c()
control_eval = c()
for(i in 1:length(final)){
    model_eval[[i]] = final[[i]]$valence_model[2]
    control_eval[[i]] = final[[i]]$valence_model[3]
}

f = data.frame(model = unlist(model_eval), control = unlist(control_eval))
f %>% melt() %>% group_by(variable) %>%
    summarise(m_v = mean(value),
              stder = sd(value)/sqrt(length(value))) %>%
              ggplot(aes(x = m_v, y = m_v, fill = variable, color = variable))+
                geom_point()+
                geom_errorbar(aes(ymin = m_v-stder, ymax = m_v+stder)) 
