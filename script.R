library(dplyr)

batting <- read.csv("Batting.csv")
salaries <- read.csv("Salaries.csv")

#AVG = H/AB
#OBP = (H+BB+HBP)/(AB+BB+HBP+SF)
#SLG = (1B + 2*X2B + 3*X3B + 4*HR)/AB

df <- merge(batting, salaries) %>% mutate(AVG=H/AB) %>% mutate(OBP = (H+BB+HBP)/(AB+BB+HBP+SF)) %>% mutate(SLG = (H + 2*X2B + 3*X3B + 4*HR)/AB) %>% filter(yearID==2001)


oldboys <- filter(df, playerID=="giambja01" | playerID=="damonjo01" | playerID=="saenzol01") %>% select(playerID, salary, AB, OBP)
sum(oldboys$salary) #15, actually 11
sum(oldboys$AB) #1469
mean(oldboys$OBP) #.364


newboys <- filter(df, playerID!="giambja01" & playerID!="damonjo01" & playerID!="saenzol01") %>% select(playerID, salary, AB, OBP) %>% filter(salary<=10^7, AB>=200, OBP>=.300)


trios <- data.frame(t(utils::combn(newboys$playerID, 3, simplify = T)))

names <- newboys$playerID

trios$salary1 <- NA
trios$salary2 <- NA
trios$salary3 <- NA
trios$AB1 <- NA
trios$AB2 <- NA
trios$AB3 <- NA
trios$OBP1 <- NA
trios$OBP2 <- NA
trios$OBP3 <- NA


for (i in 1:length(names)) {
  if (length(which(trios[,1] == names[i]))>0) {
    trios$salary1[which(trios[,1] == names[i])] <- newboys$salary[newboys$playerID==names[i]]
    trios$AB1[which(trios[,1] == names[i])] <- newboys$AB[newboys$playerID==names[i]]
    trios$OBP1[which(trios[,1] == names[i])] <- newboys$OBP[newboys$playerID==names[i]]
  }
  if (length(which(trios[,2] == names[i]))>0) {
    trios$salary2[which(trios[,2] == names[i])] <- newboys$salary[newboys$playerID==names[i]]
    trios$AB2[which(trios[,2] == names[i])] <- newboys$AB[newboys$playerID==names[i]]
    trios$OBP2[which(trios[,2] == names[i])] <- newboys$OBP[newboys$playerID==names[i]]
  }
  if (length(which(trios[,3] == names[i]))>0) {
    trios$salary3[which(trios[,3] == names[i])] <- newboys$salary[newboys$playerID==names[i]]
    trios$AB3[which(trios[,3] == names[i])] <- newboys$AB[newboys$playerID==names[i]]
    trios$OBP3[which(trios[,3] == names[i])] <- newboys$OBP[newboys$playerID==names[i]]
  }
}

trios <- trios %>% mutate(salary=salary1+salary2+salary3, AB=AB1+AB2+AB3, OBP=(OBP1+OBP2+OBP3)/3) %>% 
  filter(salary<=1.5*10^7, AB>=1469, OBP>=.364) # keep only those trios that meet or exceed the standards of the oldboys




library(ggplot2)
pl <- ggplot(trios)+ 
  geom_point(aes(x=salary, y=OBP, col=AB)) +
  xlab("Comined Salary for 2001") + ylab("Average On Base Percentage") + scale_fill_continuous(name = "At Base Sum")+
  scale_x_continuous(breaks=c(0, 5000000, 10000000, 15000000)) + ggtitle("Moneyball Trios")

ply <- plotly::ggplotly(pl)
print(ply)

#Probably the best solution at Salary = 10,088,333, At Base =	1773, and OBP = .430:
# --> berkmla01,	gonzalu01,	heltoto01




