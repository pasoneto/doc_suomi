setwd("/home/pa/Documents/github/doc_suomi/code")
source("utils.R")
source("data_cook.R")
cat(howto_data)

dt = z_scored()
dt = album_splitter(dt)
dt = dt[1:(length(dt)*0.8)]
dt = bind_rows(dt)

tn = 1
dt %>% 
    group_by(album_id) %>%
    filter(!album_id %in% c("2jxyIfyUY0yhPANWWrwnca", "4upuFjlQaEYaWyTWWaNBuq", "72vAPCga5Z8jT6NOxGrxpd")) %>%
    mutate(valence = segment3(valence),
           energy = segment3(energy), 
           loudness = segment3(loudness),
           tempo = segment3(tempo)) -> segmented

segmented %<>% filter(track_number == tn)
data.table(valence = unlist(unname(table(segmented$valence)/nrow(segmented))),
           energy = unlist(unname(table(segmented$energy)/nrow(segmented))),
           loudness = unlist(unname(table(segmented$loudness)/nrow(segmented))),
           tempo = unlist(unname(table(segmented$tempo)/nrow(segmented)))
           ) %>% select(valence.N, energy.N, loudness.N, tempo.N) -> freqs

colnames(freqs) <- c("valence", "energy", "loudness", "tempo")
redblack = colorRampPalette(c("red", "black"))(100)
freqs$quantile = c("1st", "2nd", "3d", "4th")
levelplot(as.matrix(freqs[1:4, 1:4]), col.regions = redblack)

freqs %>% 
    melt() %>% 
    ggplot(aes(x = quantile, y = value, color = variable, group = variable))+
    geom_line()+
    geom_point() + ylim(0.19, 0.41) +
    geom_hline(yintercept = 0.25, linetype = 2)+
    ylab("Probability")+
    xlab("Quantile") +
    annotate("text", x = c(4.1, 4.1, 4.1, 4.1, 4.1), y= c(seq(0.22, 0.19, -0.01), 0.24), 
             label = c(paste("Valence entropy:", round(entropy(freqs$valence), 3)), 
                       paste("Energy entropy:", round(entropy(freqs$energy), 3)), 
                       paste("Loudness entropy:", round(entropy(freqs$loudness), 3)), 
                       paste("Tempo entropy:", round(entropy(freqs$tempo), 3)),
                       paste("Baseline entropy:", round(entropy(c(0.25, 0.25, 0.25, 0.25)), 3))))