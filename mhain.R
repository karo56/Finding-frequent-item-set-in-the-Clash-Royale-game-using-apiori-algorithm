library(ggplot2)
library(stringr)
library(arules)
library(arulesViz)
library(data.table)
library(forecast)
library(formattable)
library(factoextra)


mln_clash <- read.csv("~/DM projekt1/mln_clash.csv",stringsAsFactors = FALSE, header = TRUE)
clash_table = fread("~/DM projekt1/clash_royale_games_Jan_2019.csv", header = TRUE, sep= ",")

###########hours##########
hours = substr(mln_clash[,1],12,13)
tab = as.data.frame(table(hours))

ggplot(data=tab, aes(x=hours, y=Freq,group=1)) +
  geom_line()+
  geom_point()+ theme_minimal()
###########hours##########

cards2 = prcomp(cards)

###########the frequency of occurrence of a card##########
cards = strsplit(mln_clash[,5], "_")
freq = as.data.frame(table(unlist(cards)))

ggplot(freq, aes(x=Var1, y=Freq, fill=Var1))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1,size = 8))+
  ylab("the frequency of occurrence of a card")+
  xlab("")+
  guides(fill=FALSE)

freq = freq[order(freq$Freq,decreasing = TRUE),]
###########the frequency of occurrence of a card##########


###########players##########
players = mln_clash[[4]]
unic_players = unique(players)


daily_players = as.data.frame(table(unlist(players)))


daily_players = daily_players[order(daily_players$Freq,decreasing = TRUE),]


ggplot(daily_players[1:10,], aes(x=Var1, y=Freq, fill=Var1))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1,size = 8))+
  ylab("the frequency of occurrence of a card")+
  xlab("")+
  guides(fill=FALSE)


unic_decks = unique(cards)
deck_average = length(unic_decks) / length(unic_players)

###########players##########


x = substr(mln_clash[[1]],start = 1, stop = 10)
mln_clash[[1]] = x
x = unique(x)
x = x[-1]

N = length(x)


######daily players######

palyers_freq = as.data.frame(table(unlist(mln_clash[[1]])))

ggplot(palyers_freq, aes(x=Var1, y=Freq,group=1)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1,size = 8))+
  geom_point()



######daily players######


####daly card using#######
daily_using = data.frame()
z = freq[1:30,1]
for (i in 1:N) {
  
  data2 <- mln_clash[mln_clash$timestamp == x[i],]
  daily_cards = strsplit(data2[,5], "_")
  day_freq = as.data.frame(table(unlist(daily_cards)))
  day_freq = day_freq[z,]
  
  
  
  buff = data.frame(x[i],day_freq[[1]],day_freq[[2]])
  daily_using = rbind(daily_using, buff) 

}
  
y <- c("date", "cards", "freq")
colnames(daily_using) <- y
daily_using = daily_using[order(daily_using$date),]




ggplot(daily_using, aes(date, freq, group = cards,colour =cards)) + 
  geom_point() + geom_line()+
  scale_fill_brewer(palette="Set2")

####daily card using#######






####id using######
id_freq = as.data.frame(table(unlist(mln_clash[[2]])))

y <- c("id","freq")
colnames(id_freq) <- y

arena_using= data.frame()


N = length(z)
for (i in 1:N)
{
  idk = clash_table[grep(z[i],player_deck),.N, by = arena_id]
  idk =idk[order(idk$arena_id),]
  
  buff = data.frame(z[i],idk[[1]],idk[[2]])
  arena_using = rbind(arena_using, buff) 
  print(i)
}

y <- c("card","id","freq")
colnames(arena_using) <- y



for (i in id_freq[[1]])
{
  arena_using[arena_using$id==i,3] = arena_using[arena_using$id==i,3]/id_freq[id_freq$id==i,2]
  print(i)
}

ggplot(arena_using, aes(id, freq, group = card,colour =card)) + 
  geom_point() + geom_line()+
  scale_fill_brewer(palette="Set2")
####id using######

plot(subrules_lift2, method = "graph")



plot(rules[1:50], data = Epub, method="graph", engine="htmlwidget", shading = "lift")


###basket analyse####
cards = fread("decks.txt", header = FALSE, sep = "_")
unique_cards  = sort(unique(unlist(cards, use.names = FALSE)))
card_dict = 1:length(unique_cards)
names(card_dict) = unique_cards


cards = sparseMatrix(i = rep(1:nrow(cards), each = 8, j = card_dict[unlist(cards, use.names = FALSE)]))
colnames(cards) = unique_cards
cards = as(t(cards), "itemMatrix")
                     

rules <- apriori (cards, parameter = list(supp = 0.001, conf = 0.5, maxtime=50000), control = list(memopt = TRUE))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
rules_lift <- sort (rules, by="lift", decreasing=TRUE)
rules_support <- sort (rules, by="support", decreasing=TRUE)


subrules_conf <- rules[quality(rules)$confidence > 0.90]
subrules_lift <- rules[quality(rules)$lift > 20]
subrules_support <- rules[quality(rules)$support >0.03]

subrules_lift2 <- head(rules, n = 10, by = "lift")

plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence",interactive = TRUE)
plot(rules, method = "two-key plot")
plot(subrules_conf, method = "matrix3D", measure = "lift")
plot(rules, method = "grouped")
plot(rules, method = "grouped", control = list(k = 50))
plot(subrules_lift2, method = "graph")
plot(subrules_lift2, method = "paracoord")
plot(subrules_lift2, method = "paracoord", control = list(reorder = TRUE))

#(subrules_conf[which(size(subrules_conf) == 3)])
#inspect(head(rules_conf[which(size(rules_conf) == 3)]))


itemFrequencyPlot(items(rules),support=0.05,col = rainbow(30))

######basekt analyse######





######win ratio########
clash_copy = clash_data
clash_copy[clash_copy$has_won==1,5] = paste(clash_copy[clash_copy$has_won==1,5],"_win",sep="")
clash_copy[clash_copy$has_won==1,5] = paste(clash_copy[clash_copy$has_won==0,5],"_los",sep="")
v = as.vector(clash_copy[[5]])
writeLines(v, "decks2.txt")

cards = fread("decks2.txt", header = FALSE, sep = "_", fill = TRUE)
unique_cards  = sort(unique(unlist(cards, use.names = FALSE)))
card_dict = 1:length(unique_cards)
names(card_dict) = unique_cards

cards = sparseMatrix(i = rep(1:nrow(cards), each = 10), j = card_dict[unlist(cards, use.names = FALSE)])

colnames(cards) = unique_cards
cards = as(t(cards), "itemMatrix")

rules2 <- apriori (cards, parameter = list(supp = 0.001, conf = 0.5, maxtime=50000), control = list(memopt = TRUE))


rules_subset = subset(rules2,(rhs %in% "win"))
best_decks = rules_subset[which(size(rules_subset) == 9)]

best_decks <- sort (best_decks, by="confidence", decreasing=TRUE)

rules_subset = subset(rules2,(rhs %in% "win"))

best_decks = rules_subset[which(size(rules_subset) == 9)]
best_decks <- sort (best_decks, by="confidence", decreasing=TRUE)
inspect(best_decks)
head(best_decks[1])

itemFrequencyPlot(items(best_decks),support=0.05,col = rainbow(30)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))
######win ratio########
