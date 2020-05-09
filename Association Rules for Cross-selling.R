#Install necessary packages
pacman::p_load(arules, arulesViz)

#Load Cross-Selling data
setwd("C:/Users/Erin/Documents/Masters Program/R Directories")
cross.sell <- read.csv("CatalogCrossSell.csv", nrows = 4998)
cross.sell <- cross.sell[,2:10]
head(cross.sell)
tail(cross.sell)
dim(cross.sell)


#Check for NA values and class
table(is.na(cross.data))
head(sapply(cross.data, class))


#View individual item names
t(t(names(cross.sell)))

#Create bar plot to view distribution of sales
cross.data <- as.matrix(cross.sell)
barplot(cross.data, main = "Individual Item Sales")

#Create transaction database
cross.trans <- as(cross.data, "transactions")

#Create Association Rules
rules.highconf <- apriori(cross.trans, parameter = list(supp = 0.01, conf = 0.5, maxlen = 14, target = "rules"))

#Inspect the top 10 rules
inspect(head(sort(rules.highconf, by = "lift"), n = 10))

#Limit rules those with lift above 2.5
subrules <- subset(rules.highconf, lift > 2.5)

inspect(sort(subrules, by = "lift"))

#Matrix of top 50 Rules
plot(subrules, method = "matrix", measure = "lift")

#Matrix of top 10 Rules
plot(subrules[1:10], method = "matrix", measure = "lift")

#Graph of top 10 rules
plot(subrules[1:10], method = "graph", measure = "lift")

#Scatterplot of top 50 rules
plot(subrules, jitter = 0)

#Scatterplot of all rules
plot(rules.highconf, jitter = 0)

#Create Association Rules with confidence = 0.10
rules.lowconf <- apriori(cross.trans, parameter = list(supp = 0.01, conf = 0.1, maxlen = 14, target = "rules"))

#Inspect the top 10 rules
inspect(head(sort(rules.lowconf, by = "lift"), n = 10))

#Limit rules those with lift above 3
subrules2 <- subset(rules.lowconf, lift > 3)

inspect(sort(subrules2, by = "lift"))

#Matrix of top 50 Rules
plot(subrules, method = "matrix", measure = "lift")

#Matrix of top 10 Rules
plot(subrules[1:10], method = "matrix", measure = "lift")

#Graph of top 10 rules
plot(subrules[1:10], method = "graph", measure = "lift")

#Scatterplot of top 50 rules
plot(subrules, jitter = 0)

#Scatterplot of all rules
plot(rules.highconf, jitter = 0)

