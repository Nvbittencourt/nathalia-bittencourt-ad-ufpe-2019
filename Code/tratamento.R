#-----------------------------------#
# PROJETO ANALISE DE DADOS EM R
#-----------------------------------#

# carregar pacotes
library(readxl); library(stringi); library(stringr); 
library(dplyr); library(stargazer)
install.packages("stargazer")
# carregar dados
face <- read_excel("dados2/facebook_2019.xlsx")
face2 <- read_excel("dados2/world_leaders_followers_atualizado.xlsx")
pop_speech <- read.csv("dados2/pop_speech_clean.csv")

#===================================#
# tratamento de dados
#==================================#

#### facebook 2019

# filtrar selecionar na base do facebook
face = face[face$`Personal Profile` == 1,]

# tirar nomes 'president' e 'prime minister'
face$leader = str_replace(face$`Page Section`, 'President ', '')
face$leader = str_replace(face$leader, 'Prime Minister ', '')

# remover caracteres para padronizacao e combinacao
face$leader = stri_trans_general(face$leader, "latin-ascii")
face$leader = tolower(face$leader)
face$leader = str_replace(face$leader, '-', ' ')

#### world leaders followers atualizado

# renomear colunas a partir da linha 2 e remover linha 1
colnames(face2) = face2[2,]
face2 = face2[-c(1:2),]

# transformar likes em numerico
face2$Likes = as.numeric(face2$Likes)

# remover caracteres para padronizacao e combinacao
face2$leader = stri_trans_general(face2$`World Leader`, "latin-ascii")
face2$leader = tolower(face2$leader)
face2$leader = str_replace(face2$leader, '-', ' ')

#=====================#
# combinar dados
#====================#

# selecionar variaveis de interesse
face = face[,c('leader','Likes')]
face2 = face2[,c('leader','Likes')]

# concatenar com a base facebook19
dataface = rbind(face, face2)

# remover casos dusplicados e casos faltantes 
dataface = dataface[!duplicated(dataface$leader),]
dataface = dataface[complete.cases(dataface$Likes),]

# combinar dados
dataset = merge(pop_speech, dataface, by='leader')

# selecionar variaveis e renomear
dataset = dataset[,c('leader', 'Likes','average.score', 'speech.category', 'region')]
colnames(dataset) = c('leader', 'face_likes','average_score' ,'speech', 'region')

#Salvando a nova base
write.csv(dataset, 'dataset.csv', row.names = F)
write_excel(dataset, 'dataset.xls')
library(xlsx)
library(readr)
install.packages("xlsx")

