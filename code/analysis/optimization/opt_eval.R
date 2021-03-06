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

########################### DENSITY PLOT OF ACCURACY - DIERCTION ############################
junto %>% 
    group_by(album_id1) %>%
    summarise(model_valence = match(valence_original, valence_reor), 
              model_energy = match(energy_original, energy_reor),
              model_loudness = match(loudness_original, loudness_reor),
              model_tempo = match(tempo_original, tempo_reor),      
              control_valence = match(valence_original, valence_control),
              control_energy = match(energy_original, energy_control),
              control_loudness = match(loudness_original, loudness_control),
              control_tempo = match(tempo_original, tempo_control)) %>%

              melt(id.vars = c("album_id1"), 
                   measure.vars = c("model_valence", 'model_energy', 
                                    "model_loudness", "model_tempo", 
                                    "control_valence", "control_energy", 
                                    "control_loudness", "control_tempo")) %>%
              
              separate(variable, c("condition", "feature"), "_") %>%

              ggplot(aes(x = value, fill = condition)) +
                         facet_wrap(~feature)+
                         geom_density(alpha = 0.8)

direction_counter = function(x){
    count = 0
    for(i in 1:(length(x)-1)){
        if(x[[i]] == x[[i+1]]-1){
            count = count+1
        } 
    }
    return(count/(length(x)-1))
}
colnames(junto)
########################### ACCURACY - INCREASING SEQUENCE ############################
junto %>% 
    group_by(album_id1) %>%
    summarise(model = direction_counter(track_number_reor), 
              control = direction_counter(track_number_control)) %>%

              melt() -> boxplot

boxplot %>%
    ggplot(aes(x = variable, y = value, color = variable)) +
        geom_boxplot()

junto %>% 
    group_by(album_id1) %>%
    summarise(model = direction_counter(track_number_reor), 
              control = direction_counter(track_number_control)) %>%
    ungroup() %>%
    melt() %>% group_by(variable) %>%
    summarise(val = mean(value),
              stder = sd(value)/sqrt(length(value))) %>%
    ggplot(aes(x = variable, y = val))+
        geom_point()+
        geom_errorbar(aes(ymin = val-stder, ymax = val+stder))



########################### DISSIMILARITY - INCREASING SEQUENCE ############################
dd = album_splitter(z_scored())
dd = dd[round(length(dd)*0.8):length(dd)]
dd = dplyr::bind_rows(dd)
dd %<>% filter(album_id %in% unique(reordered$album_id)) %>% arrange(album_id)
# dd %>% View()
# reordered %>% View()
reordered %<>% filter(album_id %in% unique(dd$album_id)) %>% arrange(album_id)

control = album_splitter(dd)

for(i in 1:length(control)){ control[[i]] = special_shuffler(control[[i]]) }
control = bind_rows(control)

model = left_join(reordered, dd, by=c("album_id", "track_number"))
control = left_join(control, dd, by=c("album_id", "track_number"))
comp = data.frame(valence_model = model$valence.y, 
                  valence_original = dd$valence,
                  valence_control = control$valence.x,
                  energy_model = model$energy.y,
                  energy_original = dd$energy,
                  energy_control = control$energy.x,
                  loudness_model = model$loudness.y,
                  loudness_original = dd$loudness,
                  loudness_control = control$loudness.x,
                  tempo_model = model$tempo.y,
                  tempo_original = dd$tempo,
                  tempo_control = control$tempo.x,
                  album_id = dd$album_id)

v = c("valence_model", "valence_original", "valence_control")
e = c("energy_model", "energy_original", "energy_control")
l = c("loudness_model", "loudness_original", "loudness_control")
t =  c("tempo_model", "tempo_original", "tempo_control")

features = list(v,e,l,t)
comp = album_splitter(comp)

ha = c()
for(j in 1:length(features)){
    final = c()
    for(i in 1:length(comp)){
        comp[[i]] %>% select(features[[j]]) %>% t() %>% data.frame() %>% daisy() -> oi
        final[[i]] = data.frame(as.matrix(oi))
    }
    final = bind_rows(final)
    ha[[j]] = final
}

for(i in 1:length(ha)){
    colnames(ha[[i]]) <- c("model", "original", "control") 
}

ha[[1]]$feature = "valence"
ha[[2]]$feature = "energy"
ha[[3]]$feature = "loudness"
ha[[4]]$feature = "tempo"

cabo = bind_rows(ha)
cabo %>% select(!model) %>% filter(original != 0) %>% filter(control != 0) %>% melt(id.vars = "feature") %>% 
    group_by(feature, variable) %>%
    summarise(m_v = mean(value),
              stder = sd(value)/sqrt(length(value))) %>%
              ggplot(aes(x = feature, y = m_v, fill = variable, color = variable, group = variable))+
                    #  facet_wrap(~feature)+
                     geom_point(position = position_dodge(width = 0.9))+
                     geom_errorbar(position = position_dodge(width = 0.9), aes(ymin = m_v-stder, ymax = m_v+stder))+
                     ylab("Dissimilarity with model")



cabo %>% melt(id.vars = "feature") %>% View()
colnames(cabo)





























f %>% melt() %>% 
      group_by(variable) %>%
      ggplot(aes(x = variable, y = value, fill = variable)) +
             geom_boxplot() %>%
             ggsave(path = "/home/pa/Documents/github/doc_suomi/code/optimization/", device='tiff', dpi=700)

    

dd %>% View()