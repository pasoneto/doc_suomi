## Upsampling by album length (New)
# Here I upsampled each album to 16 tracks, but plotted the means separatelly for each album length.

setwd("/home/pa/Documents/github/doc_suomi/code")
source("utils.R")
source("data_cook.R")
cat(howto_data)

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/6/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/6/")
seis = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/7/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/7/")
sete = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/8/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/8/")
oito = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/9/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/9/")
nove = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/10/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/10/")
dez = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/11/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/11/")
onze = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/12/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/12/")
doze = lapply(files, function(x){fread(x, header = TRUE)})


setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/13/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/13/")
treze = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})


setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/14/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/14/")
quatorze = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/15/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/15/")
quinze = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

setwd("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/16/")
files = list.files("/home/pa/Documents/github/doc_suomi/data/dissimilarities/upsampled_album_length/16/")
dezesseis = lapply(files, function(x){fread(x, header = TRUE, col.names = c("track_number", "valence", "energy", "loudness", "tempo"))})

for(i in 1:length(seis)){
    seis[[i]]$album_id = rep(as.character(i), 16)
    seis[[i]]$track_number <- seq(1, 16, 1)
}


for(i in 1:length(sete)){
    sete[[i]]$album_id = rep(as.character(i), 16)
    sete[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(oito)){
    oito[[i]]$album_id = rep(as.character(i), 16)
    oito[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(nove)){
    nove[[i]]$album_id = rep(as.character(i), 16)
    nove[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(dez)){
    dez[[i]]$album_id = rep(as.character(i), 16)
    dez[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(onze)){
    onze[[i]]$album_id = rep(as.character(i), 16)
    onze[[i]]$track_number <- c(seq(1, 16, 1))
}


for(i in 1:length(doze)){
    doze[[i]]$album_id = rep(as.character(i), 16)
    doze[[i]]$track_number <- c(seq(1, 16, 1))
    doze[[i]] %<>% select(!"0")
    colnames(doze[[i]]) <- c("valence", "energy", "loudness", "tempo", "album_id", "track_number")
    doze[[i]] %<>% select(track_number, valence, energy, loudness, tempo, album_id)
}

for(i in 1:length(treze)){
    treze[[i]]$album_id = rep(as.character(i), 16)
    treze[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(quatorze)){
    quatorze[[i]]$album_id = rep(as.character(i), 16)
    quatorze[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(quinze)){
    quinze[[i]]$album_id = rep(as.character(i), 16)
    quinze[[i]]$track_number <- c(seq(1, 16, 1))
}

for(i in 1:length(dezesseis)){
    dezesseis[[i]]$album_id = rep(as.character(i), 16)
    dezesseis[[i]]$track_number <- c(seq(1, 16, 1))
    
}

dg = function(dt){
    for(i in 1:length(dt)){
        dt[[i]] = data.frame(matrix(unlist(dt[[i]]), nrow=16, ncol=6))
        colnames(dt[[i]]) <- c("track_number", "valence", "energy", "loudness", "tempo", "album_id")
        dt[[i]] = tidyr::pivot_wider(dt[[i]], 
            names_from = c(album_id), 
            values_from = c(valence, energy, loudness, tempo)) 

        dt[[i]] = dt[[i]][, 2:5]
        dt[[i]] %<>% mutate_if(is.character, as.numeric)
        dt[[i]] = as.matrix(dt[[i]])
        dt[[i]] = as.matrix(daisy(dt[[i]]))

        dt[[i]][dt[[i]] == 0] <- NA
    }
    return(dt)
}


seis = dg(seis)
sete = dg(sete)
oito = dg(oito)
nove = dg(nove)
dez = dg(dez)
onze = dg(onze)

for(i in 1:length(doze)){
    colnames(doze[[i]]) <- c("valence", "energy", "loudness", "tempo", "album_id", "track_number") 
}
doze = dg(doze)
treze = dg(treze)
quatorze = dg(quatorze)
quinze = dg(quinze)
dezesseis = dg(dezesseis)

seis = matrix_parser(seis,  function(x){mean(x, na.rm = TRUE)})
sete = matrix_parser(sete,  function(x){mean(x, na.rm = TRUE)})
oito = matrix_parser(oito,  function(x){mean(x, na.rm = TRUE)})
nove = matrix_parser(nove,  function(x){mean(x, na.rm = TRUE)})
dez = matrix_parser(dez,  function(x){mean(x, na.rm = TRUE)})
onze = matrix_parser(onze,  function(x){mean(x, na.rm = TRUE)})
doze = matrix_parser(doze,  function(x){mean(x, na.rm = TRUE)})
treze = matrix_parser(treze,  function(x){mean(x, na.rm = TRUE)})
quatorze = matrix_parser(quatorze,  function(x){mean(x, na.rm = TRUE)})
quinze = matrix_parser(quinze,  function(x){mean(x, na.rm = TRUE)})
dezesseis = matrix_parser(dezesseis,  function(x){mean(x, na.rm = TRUE)})
# std <- matrix_parser(ha, function(x){sd(x, na.rm = TRUE)/sqrt(length(x))})
redblack = colorRampPalette(c("red", "black"))(100)

seis = levelplot(seis, col.regions = redblack)
sete = levelplot(sete, col.regions = redblack)
oito = levelplot(oito, col.regions = redblack)
nove = levelplot(nove, col.regions = redblack)
dez = levelplot(dez, col.regions = redblack)
onze = levelplot(onze, col.regions = redblack)
doze = levelplot(doze, col.regions = redblack)
treze = levelplot(treze, col.regions = redblack)
quatorze = levelplot(quatorze, col.regions = redblack)
quinze = levelplot(quinze, col.regions = redblack)
dezesseis = levelplot(dezesseis, col.regions = redblack)

setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="seis.png")
print(seis)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="sete.png")
print(sete)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="oito.png")
print(oito)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="nove.png")
print(nove)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="dez.png")
print(dez)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="onze.png")
print(onze)

dev.off()
setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="doze.png")
print(doze)

dev.off()

setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="treze.png")
print(treze)

dev.off()

setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="quatorze.png")
print(quatorze)

dev.off()

setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="quinze.png")
print(quinze)

dev.off()

setwd("/home/pa/Documents/github/doc_suomi/presentations")
trellis.device(device="png", filename="dezesseis.png")
print(dezesseis)


dev.off()


dev.off()
