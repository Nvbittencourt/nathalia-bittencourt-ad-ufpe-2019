########################## UFPE - MESTRADO - PPGCP #####################################
##################### TRABALHO FINAL - ANLISE DE DADOS #################################
####################### PROFESSOR: DAVI MOREIRA ########################################
#################### ALUNA: NATHALIA VIVIANI BITTENCOURT ###############################

#Downloading Packages
getwd()
library(readxl); library(stringi); library(stringr); 
library(tidyverse); library(stargazer)
library(readxl)

# Downloading final treated data - check tratamento.R  
data <- read_excel("dataset.xlsx")

# Two Plots to understand the data:
library(ggplot2)
ggplot(data = data, aes(x=leader, y=average_score)) +
  geom_col(fill = "#329999") +
  labs(title = "Pop_Speech_Dataset", x='leader', y= 'populism score') +
  theme_minimal() +
  coord_flip()

ggplot(data = data, aes(x=leader, y=face_likes)) +
  geom_col(fill = "#68132e") +
  geom_label(aes(label=face_likesc)) +
  labs(title = "Dataset_Face_Likes", x='leader', y= 'Likes') +
  theme_minimal() +
  coord_flip()

#Now, we execute the function mutate in order to creat the variables to our models:

# Variable Pop_VeryPop that creates a new column to separate populists and very populists from the other leaders
data <- mutate(data, Pop_VeryPop = ifelse(speech == "Populist" | speech == "Very Populist", 1, 0))


#Variable Pop_VeryPop_SomewPop that creates a new column to separate all kinds of populists from no populists at all
data <- mutate(data, Pop_VeryPop_SomewPop = ifelse(speech == "Populist" | 
                                            speech == "Very Populist" |
                                            speech == "Somewhat populist", 1, 0))

#Variable Not_populist that creates a new column to separate not populist leaders
data <- mutate(data, Not_populist = ifelse(speech == 'Not populist', 1, 0)) 

#Variable America that creates a new column to separate the two regions in analysis
data <- mutate(data, America = ifelse(region == "Latin America & Caribbean" |
                                        region == "North America", 1, 0))

#Variable Europe_CentralAsia that creates a new column to separate the two regions in analysis
data <- mutate(data, Europe_CentralAsia = ifelse(region == "Europe & Central Asia", 1, 0))


###### At last, the Models ########

# First Linear model: vd: face_likes; vi: Pop_VeryPop
Reg1_Pop_VeryPop = glm(face_likes ~ Pop_VeryPop, data = data)
summary(Reg1_Pop_VeryPop)
layout(matrix(c(1,2,3,4),2,2))
plot(Reg1_Pop_VeryPop)

#Interactive Regression plot:
install.packages('ggiraphExtra') 
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(Reg1_Pop_VeryPop,se=TRUE,interactive=TRUE)

# Second Linear model: vd: face_likes; vi: Pop_VeryPop_SomewPop
Reg2_Pop_VeryPop_SomewPop = glm(face_likes ~ Pop_VeryPop_SomewPop, data = data)
summary(Reg2_Pop_VeryPop_SomewPop)
layout(matrix(c(1,2,3,4),2,2))
plot(Reg2_Pop_VeryPop_SomewPop)

#Interactive Regression plot:
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(Reg2_Pop_VeryPop_SomewPop,se=TRUE,interactive=TRUE)

# Third Linear model: vd: face_likes; vi: Not_populist
Reg3_Not_populist = glm(face_likes ~ Not_populist, data = data)
summary(Reg3_Not_populist)
layout(matrix(c(1,2,3,4),2,2))
plot(Reg3_Not_populist)

#Interactive Regression plot:
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(Reg3_Not_populist,se=TRUE,interactive=TRUE)


# Multiple Linear Regression

## First Model: vd: face_likes; vi: Pop_VeryPop_SomewPop, vii: America
RegM1_America <- lm(face_likes ~ America + Pop_VeryPop_SomewPop, data = data)
summary(RegM1_America)
layout(matrix(c(1,2,3,4),2,2))
plot(RegM1_America)

#Interactive plot 1:
ggPredict(RegM1_America,colorAsFactor = TRUE,interactive=TRUE)

##Second Model: vd: face_likes; vi: Pop_VeryPop_SomewPop, vii: Europe_CentralAsia
RegM2_Europe_CentralAsia <- lm(face_likes ~ Europe_CentralAsia + Pop_VeryPop_SomewPop, data = data)
summary(RegM2_Europe_CentralAsia)
layout(matrix(c(1,2,3,4),2,2))
plot(RegM2_Europe_CentralAsia)

#Interactive plot 2:
ggPredict(RegM2_Europe_CentralAsia,colorAsFactor = TRUE,interactive=TRUE)

# Comparative analysis between the two models:
anova(RegM1_America, RegM2_Europe_CentralAsia)

# Stargazer tables in order to best interpret the results:
library(stargazer)

#Linear Regressions
stargazer(Reg1_Pop_VeryPop, Reg2_Pop_VeryPop_SomewPop, Reg3_Not_populist,
          type = "html", title = "Results", style = "ajps", p.auto=FALSE,  out="linearmodels.htm",
          covariate.labels = c("|Populist + Very Pop.|", "|Populist + Very Pop. + Somew. Pop.|", "|Not Populist"))

#Multiple Linear Regressions
stargazer(RegM1_America, RegM2_Europe_CentralAsia,
          type = "html", title = "Results 2", style = "ajps", p.auto=FALSE, out="multiplemodels.htm",
          covariate.labels = c("|America|", "|Europe and Central Asia|")) 

install.packages('ggiraphExtra') 
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(Reg1_Pop_VeryPop,se=TRUE,interactive=TRUE)
