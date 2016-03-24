### PARTIE 2 : ACP###
setwd("C:/Users/C/Desktop/GI04/SY09")

## 2.2 ##
#Initialisation des données
notesMath<-c(6.0,8.0,6.0,14.5,14.0,11.0,5.5,13.0,9.0)
notesScience<-c(6.0,8.0,7.0,14.5,14.0,10.0,7.0,12.5,9.5)
notesFrancais<-c(5.0,8.0,11.0,15.5,12.0,5.5,14.0,8.5,12.5)
notesLatin<-c(5.5,8.0,9.5,15.0,12.5,7.0,11.5,9.5,12.0)
notesDM<-c(8.0,9.0,11.0,8.0,10.0,13.0,10.0,12.0,18.0)
notes<-matrix(c(notesMath,notesScience,notesFrancais,notesLatin,notesDM),nrow=9,ncol=5)
colnames(notes) = c("math","scie","fran","lati","d-m")
rownames(notes) = c("jean","aline","annie","monique","didier","andre","pierre","brigitte","evelyne")

#Calcul de l'ACP
res<-princomp(notes) #graphe à faire. Qu'est ce que ca represente? Perform a ACP
sum<-summary(res)
# Standard deviation ?
# Proportion of Variance: pourcentages d'inertie expliquée par chaque axe.
# Cumulative Proportion: pourcentages d'inertie expliquée par les sous-espaces.
(res$sdev)^2 #valeurs propres
res$loadings #vesteurs propres -> pourquoi echange de signes?
res$scores#nouveau tableau individus-variables -> aussi l'inverse.
plot(res)#valeurs propres (variances)
biplot(res)
#Math et Science très corrélées, tout comme Francais et latin.
biplot(res,c(1,3))# L'axe trois montre le cote plus artiste des eleves. L'axe 1 correspond au coté matières "classiques".

biplot.princomp(res,c(1,3))
# Viens du mode de calcul (-1)*(-lambda) et (-lambda)


## 2.3 ##
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]

#Calcul de l'ACP
crabsACP<-princomp(crabsquant) #graphe à faire. Qu'est ce que ca represente? Perform a ACP
crabsSum<-summary(crabsACP)
# Standard deviation ?
# Proportion of Variance: pourcentages d'inertie expliquée par chaque axe.
# Cumulative Proportion: pourcentages d'inertie expliquée par les sous-espaces.
(crabsACP$sdev)^2 #valeurs propres
crabsACP$loadings #vesteurs propres -> pourquoi echange de signes?
crabsACP$scores#nouveau tableau individus-variables -> aussi l'inverse.
plot(crabsACP)#valeurs propres (variances)
biplot(crabsACP)
biplot(crabsACP,c(1,3))

crabsWithoutCl<-crabsquant[-3]
crabsWithoutClACP<-princomp(crabsWithoutCl) 
biplot(crabsWithoutClACP)
biplot(crabsWithoutClACP,c(1,3))

crabsWithoutFl<-crabsquant[-1]
crabsWithoutFlACP<-princomp(crabsWithoutFl) 
biplot(crabsWithoutFlACP)
biplot(crabsWithoutFlACP,c(1,3))

crabsWithoutRw<-crabsquant[-2]
crabsWithoutRwACP<-princomp(crabsWithoutRw) 
biplot(crabsWithoutRwACP)
biplot(crabsWithoutRwACP,c(1,3))

crabsWithoutCw<-crabsquant[-2]
crabsWithoutCwACP<-princomp(crabsWithoutCw) 
biplot(crabsWithoutCwACP)
biplot(crabsWithoutCwACP,c(1,3))

crabsWithoutRw<-crabsquant[-2]
crabsWithoutRwACP<-princomp(crabsWithoutRw) 
biplot(crabsWithoutRwACP)
biplot(crabsWithoutRwACP,c(1,3))

head(crabsquant)