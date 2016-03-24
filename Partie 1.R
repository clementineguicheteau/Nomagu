### PARTIE 1.1 ###
setwd("C:/Users/C/Desktop/GI04/SY09")
books <- read.csv("anonymous-betting-data.csv")
source("pretraitements.R")

### 1.1.1 ###
match_uids <- unique(books.sel$match_uid)  # unique match uids
nb_matchs <- length(match_uids)  # total number of matchs : 25993

player_uids <- unique(c((books.sel$winner), (books.sel$loser)))  # unique player uids
nb_players <- length(player_uids)  # total number of players : 1523

date_min <- min(books.sel$year)  # start date : 2009
date_max <- max(books.sel$year)  # end date : 2015

nb_bookmakers <- length(unique(books.sel$book))  # total number of bookmakers : 7


### 1.1.2 ###
# creation of a subset with only the match_uid and the players
matchs_winner_loser <- setNames(aggregate(books.sel$book, list(books.sel$match_uid, books.sel$winner, books.sel$loser), length), c("match_uid", "winner", "loser"))
# aggregating according to the winners to compute the number of win
winners <- setNames(aggregate(matchs_winner_loser$winner, list(matchs_winner_loser$winner), length), c("player_uid", "win"))
# aggregating according to the losers to compute the number of defeats
losers <- setNames(aggregate(matchs_winner_loser$loser, list(matchs_winner_loser$loser), length), c("player_uid", "lose"))

players_stats <- merge(winners, losers, all.y = T, all.x = T) # merge the winners and the losers 
players_stats[is.na(players_stats)] <- 0 # if the player is not in winners or losers, the column value will be "Na"
players_stats$total <- players_stats$win + players_stats$lose # compute the number of matches
players_stats$win.freq <- players_stats$win / players_stats$total # compute the frequency of winning
players_stats$lose.freq <- players_stats$lose / players_stats$total # compute the frequency of losing

# Divide the players into 7 categories according to their frequency to win
players_stats$category<-cut(players_stats$win.freq, 7)
# Aggregate by category to compute the mean of number of wins and the mean of number of defeats
nbDefeatsWinCategoryMean <- setNames(aggregate(list(players_stats$win, players_stats$lose), list(players_stats$category), FUN = mean), c("category", "nbWin", "nbDefeats"))

# Compute the number of players by category and add it to the dataset
nbPlayersCategory <- aggregate(players_stats$player_uid, list(players_stats$category), FUN = length)
nbDefeatsWinCategoryMean$nbPlayers <- nbPlayersCategory$x

# Cast to vectors to use barplot
nbWinVector<-as.vector(nbDefeatsWinCategoryMean$nbWin)
nbLostVector<-as.vector(nbDefeatsWinCategoryMean$nbDefeats)
LostWinMatrix<-rbind(nbLostVector,nbWinVector)
barplot(LostWinMatrix, names.arg = c("A","B","C","D","E","F","G"), legend.text = c("Défaites", "Victoires"), main="Nombre moyen de match gagnés et perdus par catégorie", xlab="Catégories", ylab="Nombre de match", col = c('blue', 'red'), beside=TRUE)
#A : (-0.001,0.143]
#B : (0.143,0.286]
#C : (0.286,0.429]
#D : (0.429,0.571]
#E : (0.571,0.714]
#F : (0.714,0.857]
#G : (0.857,1]
# RAPPORT: On remarque que la dernière catégorie est composée majoritairement de personnes ayant 100% de réussite tout en ayant joué que très peu de match (1 ou 2), ce qui fausserait les niveaux.


### 1.3 ###
## 1.3.a ##
# Compute the evolution of the probability of winning per book
implied_prob_winner_evolution<-books.sel$implied_prob_winner_close-books.sel$implied_prob_winner_open
# Compute the evolution of the probability of losing per book
implied_prob_loser_evolution<-books.sel$implied_prob_loser_close-books.sel$implied_prob_loser_open
# Compute the absolute value of this evolution
implied_prob_evolution<-abs(implied_prob_winner_evolution)

# Create a data frame whiwh gather the match_uid, the evolution of the probability of winning and of losing, and the absolute value of this evolution
match_probabilities <- data.frame(books.sel$match_uid, books.sel$year, books.sel$book, implied_prob_winner_evolution, implied_prob_loser_evolution, implied_prob_evolution)
# Set the column names
match_probabilities<-setNames(match_probabilities, c("match_uid","year","book","implied_prob_winner_evolution", "implied_prob_loser_evolution", "implied_prob_evolution"))
# Compute the suspect mach according to the probability evolution : 2798 match
suspect_book<-data.frame(match_probabilities$match_uid[match_probabilities$implied_prob_evolution > 0.1],match_probabilities$year[match_probabilities$implied_prob_evolution>0.1],match_probabilities$book[match_probabilities$implied_prob_evolution>0.1],match_probabilities$implied_prob_evolution[match_probabilities$implied_prob_evolution>0.1])
suspect_book<-setNames(suspect_book, c("match_uid","year","book","implied_prob_evolution"))
suspect_match<-setNames(aggregate(suspect_book$book, list(suspect_book$match_uid, suspect_book$year), length), c("match_uid","year","nbBookMakers")) # 2798 suspect matches

#Caracterise the suspect matches per year and per book (A PLOTER)
suspect_match_per_year_per_book<-setNames(aggregate(suspect_match$match_uid, list(suspect_match$year,suspect_match$nbBookMakers), length), c("year","book","NumberSuspectMatch"))
ggplot(data=suspect_match_per_year_per_book,aes(x=year, y=NumberSuspectMatch, group=book, colour=book)) + geom_line() + geom_point() + ggtitle("Nombre de Matchs suspects par Bookmaker et par Année")+ xlab("Année") + ylab("Nombre de Matchs")
plot(x=suspect_match_per_year_per_book$year, y=suspect_match_per_year_per_book$NumberSuspectMatch,type="b")

## 1.3.b ##

# rien que pour le premier match cinq bookmakers en jeu : B D A C F
# books.sel$book[which(books.sel$match_uid == 'a764419568')]
# books.sel$book[which(books.sel$match_uid == 'bdb50d4b91')]
# [1] B D A F G E

suspect_winner <- unique(books.sel$winner[which(match_prob$implied_prob_evolution>0.1)])
suspect_losers <- books.sel$loser[which(match_prob$implied_prob_evolution>0.1)]
suspect_loser_unique <- unique(books.sel$loser[which(match_prob$implied_prob_evolution>0.1)])
suspect_loser <- books.sel$loser[which(match_prob$implied_prob_evolution>0.1)]
suspect_players<-unique(c(suspect_loser, suspect_winner))
length(suspect_players)

suspect_defeats <- books.sel$match_uid[which(match_prob$implied_prob_evolution>0.1)]
suspect_defeats_games <- data.frame(suspect_loser, suspect_defeats)
suspect_defeats_games<-aggregate(suspect_defeats_games, list(suspect_defeats_games$suspect_loser), FUN = length)
suspect_defeat_players<-suspect_defeats_games$suspect_loser[which(suspect_defeats_games$suspect_defeats>10)]
length(suspect_defeat_players)


suspect_bookings<-data.frame(books.sel$match_uid[books.sel$implied_prob_evolution > 0.1], books.sel$book[books.sel$implied_prob_evolution > 0.1], books.sel$winner[books.sel$implied_prob_evolution > 0.1], books.sel$lose[books.sel$implied_prob_evolution > 0.1])
suspect_bookings<-setNames(suspect_bookings, c("match_uid", "book", "winner", "loser"))
aggregate(suspect_bookings$match_uid, list(suspect_bookings$book), FUN= length)
bookSuspect<-rbind(nbMatchBookVector, bookerVector)
barplot(bookSuspect, main="Nombre de match par bookers", names.arg=bookSuspect[2,], xlab="Bookers", ylab="Nombres de match")

## 1.2 ##
library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
head(crabs)
library(ggplot2)

# Combien de crabs? 200 crabes décrits par 8 variables
# Combien d'espèces différentes? 100 crabs de l'espece 0 et 100 rabes de l'espece B
aggregate(crabs$index, list(crabs$sp), length)
# Longueur lobe frontal
# Largeur arrière
# Longueur de la carapace
# Largeur de la carapace
# Epaisseur de la longueur du crabe
# Proportion des sexes? 100 filles et 100 garcons
aggregate(crabs$index, list(crabs$sex), length)
# taille selon l'espece et le sexe?
crabsquantBySpeciesSex<-aggregate(crabsquant, list(crabs$sp,crabs$sex), mean)
crabsquantBySpecies<-aggregate(crabsquant, list(crabs$sp), mean)
crabsquantBySex<-aggregate(crabsquant, list(crabs$sex), mean)
hist()
pairs(crabsquant) #montres que les données se chevauchent -> pas 2 trucs différents
#multiple par un vecteur qui correspond à la somme des variables?
