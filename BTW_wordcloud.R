#This script reads in the election manifestos of the biggest German political parties for the 2017 general election.
#It outputs a wordcloud for each manifesto, i.e. each party.

# (c) Richard Kunert (rikunert@gmail.com), August 2017

##################################################################################
# LOAD LIBRABRIES

if(!require(tm)){install.packages('tm')}
library(tm) # text mining

if(!require(wordcloud)){install.packages('wordcloud')}
library(wordcloud)

##################################################################################
# LOADING TEXTS

manifestos_pdf = c('https://www.cdu.de/system/tdf/media/dokumente/170703regierungsprogramm2017.pdf',
                   'https://www.spd.de/fileadmin/Dokumente/Bundesparteitag_2017/Es_ist_Zeit_fuer_mehr_Gerechtigkeit-Unser_Regierungsprogramm.pdf',
                   'https://www.die-linke.de/fileadmin/download/wahlen2017/wahlprogramm2017/die_linke_wahlprogramm_2017.pdf',
                   'https://www.fdp.de/sites/default/files/uploads/2017/08/07/20170807-wahlprogramm-wp-2017-v16.pdf',
                   'https://www.gruene.de/fileadmin/user_upload/Dokumente/BUENDNIS_90_DIE_GRUENEN_Bundestagswahlprogramm_2017.pdf',
                   'https://www.afd.de/wp-content/uploads/sites/111/2017/06/2017-06-01_AfD-Bundestagswahlprogramm_Onlinefassung.pdf')

files_txt = sapply(manifestos_pdf, function(x) {
  dest <- tempfile(pattern = substr(x, 13, 15), fileext = ".pdf") # prepare a tmp file which includes party in name
  download.file(x, dest, mode = "wb") # fill the tmp file with a downloaded election manifesto
  system(paste('"C:\\Program Files\\xpdf-tools-win-4.00\\bin64\\pdftotext.exe" -layout ', # make windows use this pdf-to-txt program
               "\"", dest, "\"", sep = ""), # on this election manifesto
         wait = F)
  sub(".pdf", ".txt", dest)}) #return name of final txt file

##################################################################################
# PREPROCESSING TEXTS (CORPUS BUILDING)

txt <- Corpus(URISource(files_txt),
              readerControl = list(reader = readPlain, language = 'german'))

txt.tdm <- TermDocumentMatrix(txt, control = list(removePunctuation = TRUE,
                                                  stopwords = T,#I am not sure this works well in German
                                                  tolower = T,
                                                  stemming = F,#disabled to get full words
                                                  removeNumbers = TRUE,
                                                  bounds = list(global = c(3, Inf))))#only words mentioned at least three times across manifestos

ft_matrix_prop = prop.table(as.matrix(txt.tdm), margin = 2)#column proportions
party_names = c("CDU/CSU", "SPD", "Die Linke", "FDP", "Grüne", "AFD")
colnames(ft_matrix_prop) <- party_names # add publication ready column labels

##################################################################################
# PLOTTING

party_colours = c('black', 'red', 'magenta 3', 'orange', 'dark green', 'blue')

comparison.cloud(ft_matrix_prop, max.words = 200, random.order = FALSE,
                 colors = party_colours, title.size = 1.5)
mtext('@rikunert', side = 1, line = 4, adj = 0) # caption

for (i in 1:length(party_names)) { 
  wordcloud(rownames(ft_matrix_prop), ft_matrix_prop[,i],
            min.freq = 0.001, max.words = 100,
            colors = party_colours[i],
            scale = c(3.2, .4))
  mtext('@rikunert', side = 1, line = 4, adj = 0) # caption
  mtext(party_names[i], side = 3, line = 3, adj = 0.5) # title
}