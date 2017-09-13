#This script performs a Latent Semantic Analysis on German party manifestos
# and plots the results in an interactive 3D scatter plot

# (c) Richard Kunert September 2017

######################################################################
# load packages

library(tm)#text mining
library(stringr)#word counts
library(lsa)#latent semantic analysis
library(plotly)#interactive plotting

######################################################################
# custom functions

readManifesto = function(manifesto_address){
  #read a political manifesto separating paragraphs into list entries
  line_break = '<<>><<>>'
  m = paste(readLines(manifesto_address), collapse = line_break)
  m = strsplit(m, split = paste(rep(line_break, 2), collapse = ''), fixed = T)
  m = gsub(line_break, ' ', m[[1]])
}

#######################################################################
#data loading

wd = 'https://raw.githubusercontent.com/rikunert/BTW2017_manifesto_analysis/master/cleaned_manifestos'

afd = readManifesto(paste(wd, "afd_cleaned.txt", sep = '/'))
cdu = readManifesto(paste(wd, "cdu_cleaned.txt", sep = '/'))
die = readManifesto(paste(wd, "die_cleaned.txt", sep = '/'))
fdp = readManifesto(paste(wd, "fdp_cleaned_ANSI.txt", sep = '/'))
gru = readManifesto(paste(wd, "gru_cleaned_ANSI.txt", sep = '/'))
spd = readManifesto(paste(wd, "spd_cleaned.txt", sep = '/'))

#remove too short paragraphs and put it all together
minimal_paragraph_length = 12#defined in words

df <- data.frame(text = c(afd[str_count(afd, '\\w+') > minimal_paragraph_length],
                          cdu[str_count(cdu, '\\w+') > minimal_paragraph_length],
                          die[str_count(die, '\\w+') > minimal_paragraph_length],
                          fdp[str_count(fdp, '\\w+') > minimal_paragraph_length],
                          gru[str_count(gru, '\\w+') > minimal_paragraph_length],
                          spd[str_count(spd, '\\w+') > minimal_paragraph_length]),
                 view = factor(c(rep('AFD', sum(str_count(afd, '\\w+') > minimal_paragraph_length)),
                                 rep('CDU/CSU', sum(str_count(cdu, '\\w+') > minimal_paragraph_length)),
                                 rep('Die Linke', sum(str_count(die, '\\w+') > minimal_paragraph_length)),
                                 rep('FDP', sum(str_count(fdp, '\\w+') > minimal_paragraph_length)),
                                 rep('Bündnis 90/Die Grünen', sum(str_count(gru, '\\w+') > minimal_paragraph_length)),
                                 rep('SPD', sum(str_count(spd, '\\w+') > minimal_paragraph_length))
                 )),
                 stringsAsFactors = FALSE)

#How many paragraphs included
summary(df$view)

#######################################################################
#Latent Semantic Analysis

#prepare corpus
corp <- Corpus(VectorSource(df$text),
               readerControl = list(language = 'german'))

#turn corpus into TDM
corp <- TermDocumentMatrix(corp, control = list(removePunctuation = TRUE,
                                                stopwords = T,#I am not sure this works well in German
                                                tolower = T,
                                                stemming = T,#I am not sure this works well in German
                                                removeNumbers = TRUE))

td.mat <- as.matrix(corp)

td.mat.lsa <- lw_bintf(td.mat) * gw_gfidf(td.mat)  # weighting: global frequency * inverse document frequency
lsaSpace <- lsa(td.mat.lsa, dims = 10)  # create LSA space

#######################################################################
#3D scatter plot

#get hover text of plot right (line breaks)
br_max = 60#maximal number of characters before line break

for (i in 1:length(df$text)){#for each paragraph
  paragraph = df$text[i]
  spaces = str_locate_all(pattern =' ', paragraph)[[1]][,1]#locate where spaces are in paragraph
  br_idx = unique(unlist(lapply(br_max * 1:ceiling(nchar(paragraph) / br_max),
                                function(x) max(spaces[spaces < x]))))#locate future line breaks
  
  #implement line breaks
  paragraph_sep <- unlist(strsplit(paragraph,""))#split string into separate characters
  paragraph_sep[br_idx] <- '<br>'#replace characters where line break should occur with html line break
  df$text[i] = paste0(paragraph_sep,collapse='')#put the string back together
  
}

#data frame for plotting paragraphs
points <- data.frame(x = lsaSpace$dk[,1],
                     y = lsaSpace$dk[,2],
                     z = lsaSpace$dk[,3],
                     c = df$view,
                     t = df$text)

#get colours of plot right
party_colours = c('black', 'red', 'magenta 3', 'orange', 'dark green', 'blue')
party_names = c("CDU/CSU", "SPD", "Die Linke", "FDP", "Bündnis 90/Die Grünen", "AFD")
points$c <- factor(points$c,levels= party_names) # add publication ready labels

#party centroids (this is probably better done with a tidyverse solution. If you know it, tell me!)
centroids = matrix(NA, nrow =10, ncol =6)
colnames(centroids) = party_names
for(i in 1:10){#for each LSA dimension
  centroids[i,] = unlist(lapply(party_names, function(x) mean(lsaSpace$dk[df$view == x,i])))
}

#dist(t(centroids))#the distances between party centroids

points_centroids <- data.frame(x = centroids[1,],
                     y = centroids[2,],
                     z = centroids[3,],
                     c = paste(party_names, ' centroid'))
points_centroids$c <- factor(points_centroids$c,levels= paste(party_names, ' centroid')) # add publication ready labels

p <- plot_ly() %>%
  add_markers(data = points, x = ~x, y = ~y, z = ~z, color = ~c,
              colors = rep(party_colours, 2),#requires repeating because plotly doesn't link paragraph and centroid legend entries
              hoverinfo = 'text',
              text = ~paste(c, ':<br>', t)) %>%
  add_markers(data = points_centroids, x = ~x, y = ~y, z = ~z, color = ~c,
              hoverinfo = 'text',
              text = ~paste(c),
              marker = list(symbol = 'cross')) %>%
  layout(scene = list(xaxis = list(title = 'Semantic<br>Dimension 1'),
                      yaxis = list(title = 'Semantic<br>Dimension 2'),
                      zaxis = list(title = 'Semantic<br>Dimension 3')),
         title = 'The semantic space of German election manifestos',
         annotations = list(list(x = 0, y = 0,#bottom left corner of frame
                                 text = '<a href="https://twitter.com/rikunert">@rikunert</a>',
                                 xref = 'paper', yref = 'paper',
                                 xanchor = 'left',#left aligned
                                 showarrow = F),
                            list(x = 1.02, y = 0.8,
                                 text = 'Click to show/hide',
                                 xanchor = 'left', yanchor = 'bottom',
                                 showarrow = F)),
         legend = list(y = 0.8, yanchor = 'top'))
#p#privately have a look at the plot in browser

plotly_POST(p, filename = "BTW2017_manifesto_LSA")#push plotly post to plotly website # Set up API credentials: https://plot.ly/r/getting-started