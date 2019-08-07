data <- read.csv("dataset.csv")

#Função mutate e ifelse

#executar
data <- mutate(data, populist = ifelse(speech == "Populist" | speech == "Very Populist", 1, 0))
data <- mutate(data, populist2 = ifelse(speech == "Populist" | 
                                            speech == "Very Populist" |
                                            speech == "Somewhat populist", 1, 0))
data <- mutate(data, not_populist = ifelse(speech == 'Not populist', 1, 0)) 

###### Construção de modelos ########

model1 = glm(face_likes ~ populist, data = data)
summary(model1)

model2 = glm(face_likes ~ populist2, data = data)
summary(model2)

model3 = glm(face_likes ~ not_populist, data = data)
summary(model3)

library(stargazer)
stargazer(model1, model2, model3,
          type = "text", title = "Results", style = "ajps", p.auto=FALSE,
          column.labels = c("|Populist + Veru Pop.|", "|Populist + Very Pop. + Somew. Pop.|", "|Not Populist")) 

library(ggplot2)
ggplot(data = data, aes(x=leader, y=face_likes)) +
  geom_col() +
  geom_label(aes(label=face_likes)) +
labs(x='leader', y= 'N de seguidores') +
  coord_flip()
