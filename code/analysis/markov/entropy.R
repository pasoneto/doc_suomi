setwd("/home/pa/Documents/github/doc_suomi/code")
source("utils.R")
source("data_cook.R")

dt = base() %>% 
        group_by(album_id) %>%
        mutate(valence = greater(valence),
               energy = greater(energy),
               loudness = greater(loudness),
               tempo = greater(tempo))

transition_matrices = 
    data.frame(first = c("greater", "greater", "smaller", "smaller", "start", "start"),
               second = c("greater", "smaller", "greater", "smaller", "greater", "smaller"),
               valence = c(0.3205376, 0.6794624, 0.6677269, 0.3322731, 0.5336611, 0.4663389),
               energy = c(0.3118086, 0.6881914, 0.6616254, 0.3383746, 0.5075643, 0.4924357),
               loudness = c(0.3223561, 0.6776439, 0.6515744, 0.3484256, 0.5279879, 0.4720121),
               tempo = c(0.3328422, 0.6671578, 0.6619533, 0.3380467, 0.5075643, 0.4924357), 
               model = rep("markov", 6))

base_matrix = 
    data.frame(first = c("greater", "greater", "smaller", "smaller", "start", "start"),
               second = c("greater", "smaller", "greater", "smaller", "greater", "smaller"),
               valence = c(0.4668268, 0.5331732, 0.5277928, 0.4722072, 0.4984871, 0.5015129),
               energy = c(0.4637097, 0.5362903, 0.5154295, 0.4845705, 0.4973525, 0.5026475),
               loudness = c(0.4595637, 0.5404363, 0.5268686, 0.4731314, 0.4780635, 0.5219365),
               tempo = c(0.4668530, 0.5331470, 0.5319954, 0.4680046, 0.4897882, 0.5102118),
               model = rep("baseline", 6))

both = bind_rows(transition_matrices, base_matrix)
both %<>%
    group_by(model) %>%
    summarise(valence = entropy(valence),
              energy = entropy(energy),
              loudness = entropy(loudness),
              tempo = entropy(tempo))

both %<>% melt()

ggplot(data = both, aes(x = variable, y = value, fill = model, color = model))+
    # facet_wrap(~variable)+
    geom_errorbar(aes(ymin = 2, ymax = value), color = "black", width = 0, position = position_dodge(width = 0.8), linetype = "dashed")+
    geom_point(size = 2.5, position = position_dodge(width = 0.8), shape = 23, fill = "white", stroke = 3)+
    ylim(c(2, 3.2))+
    geom_text(aes(label=round(value, 3)), vjust = -1.2, color = "black")+
    ylab("Entropy") + xlab("Feature")

#############################################
############ INFORMATION GAIN ###############
#############################################

valence_f =  unname(table(dt$valence)/nrow(dt)) 
energy_f =   unname(table(dt$energy)/nrow(dt))
loudness_f = unname(table(dt$loudness)/nrow(dt)) 
tempo_f =    unname(table(dt$tempo)/nrow(dt))

firsts = c("greater", "smaller", "start")
valence = c(); energy = c(); loudness = c(); tempo = c()
for(i in 1:length(firsts)){
    dado = filter(transition_matrices, transition_matrices$first == firsts[[i]])
    valence[i] = entropy(dado$valence)*valence_f[i]
    energy[i] = entropy(dado$energy)*energy_f[i]
    loudness[i] = entropy(dado$loudness)*loudness_f[i]
    tempo[i] = entropy(dado$tempo)*tempo_f[i]
}   

feature = c("valence", "energy", "loudness", "tempo")
overall_entropy = c(entropy(valence_f), entropy(energy_f), entropy(loudness_f), entropy(tempo_f))
group_entropy = c(sum(valence), sum(energy), sum(loudness), sum(tempo)) 
information_gain = overall_entropy - group_entropy

model = data.frame(feature = feature, 
                   overall_entropy = overall_entropy,
                   group_entropy = group_entropy,
                   information_gain = information_gain)


###################################
########## BASELINE ###############
###################################
valence = c(); energy = c(); loudness = c(); tempo = c()
for(i in 1:length(firsts)){
    dado = filter(base_matrix, base_matrix$first == firsts[[i]])
    valence[i] = entropy(dado$valence)*valence_f[i]
    energy[i] = entropy(dado$energy)*energy_f[i]
    loudness[i] = entropy(dado$loudness)*loudness_f[i]
    tempo[i] = entropy(dado$tempo)*tempo_f[i]
}   

feature = c("valence", "energy", "loudness", "tempo")
overall_entropy = c(entropy(valence_f), entropy(energy_f), entropy(loudness_f), entropy(tempo_f))
group_entropy = c(sum(valence), sum(energy), sum(loudness), sum(tempo)) 
information_gain = overall_entropy - group_entropy

baseline = data.frame(feature = feature, 
                   overall_entropy = overall_entropy,
                   group_entropy = group_entropy,
                   information_gain = information_gain)
model$model = "markov"
baseline$model = "baseline"
final = bind_rows(model, baseline)


final %<>% melt()

ggplot(data = final, aes(x = feature, y = value, fill = model, color = model))+
    facet_wrap(~variable)+
    geom_errorbar(aes(ymin = 0.2, ymax = value), color = "black", width = 0, position = position_dodge(width = 0.8), linetype = "dashed")+
    geom_point(size = 2.5, position = position_dodge(width = 0.8), shape = 23, fill = "white", stroke = 3)+
    ylim(c(0.2, 1.35))+
    geom_text(aes(label=round(value, 3)), vjust = -1.2, color = "black")

