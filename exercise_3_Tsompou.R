dat = read.csv("C:/Users/ak1002/Box/courses/Statistics for Biologists/data/butterflies.csv")
summary(dat)      #quick summary of all variables


#addition of M and L to seperate data
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
means #mean values

ses = tapply(dat$DevelopmentTime, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
ses 

colMeans(means)
rowMeans(means)


#perform two ways anova with interaction
m = lm(DevelopmentTime~MaternalHost*LarvalHost, data=dat)
anova(m)


#plot the means and standard errors 
par(font.main = 1)
plot(c(0.97, 1.03), means[,1], ylim=c(18, 40), xlim=c(0.8, 2.2),
     xlab="Larval host", 
     ylab="Developmental time (days)",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3)

segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])

points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")

legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))
title ("Maternal and lavra host in regard to developmental time", line=0.5)


library(ggplot2)
# Create a boxplot for maternal and lavra interaction to dev time

ggplot(dat, aes(x = LarvalHost, y = DevelopmentTime)) +
  geom_boxplot(fill="grey")+
  labs(title = "Lavra host in regards to depevopment time",
       x = "Lavra Host", y = "Development time",
       fill = "Maternal Host") +theme_minimal() + theme(
         axis.text = element_text(size = 16), 
         axis.title.y = element_text(size = 16),
         axis.title.x = element_text(size = 16),
         plot.title = element_text(size = 16, hjust = 0.5))



ggplot(dat, aes(x = MaternalHost,y = DevelopmentTime,)) +
  geom_boxplot(fill="grey")+
  labs(title = "Maternal host in regards to depevopment time",
       x = "Maternal Host", y = "Development time",
       fill = "Maternal Host") + theme_minimal() +theme(
         axis.text = element_text(size = 14),
         plot.title = element_text(size = 14, hjust = 0.5),
         axis.title.y = element_text(size = 16),
         axis.title.x = element_text(size = 16))




#boxplot with all of them                                                          
ggplot(dat, aes(x = LarvalHost,y = DevelopmentTime, fill=MaternalHost)) +
  geom_boxplot()+
  labs(title = "",
       x = "Lavra Host", y = "Development time",
       fill = "Maternal Host") + theme_minimal() + theme(axis.text = element_text(size = 12),plot.title = element_text(size = 14, hjust = 0.5))




# growth rate dependency
names(dat)
m1 = lm(GrowthRate~MaternalHost*LarvalHost, data=dat)
m2 = lm(GrowthRate~MaternalHost+LarvalHost, data=dat)
anova(m1)
anova(m2)


# check which model is the best. Lower AIC value is best model
library(AICcmodavg)
model.set = list(m1, m2)
model.names = c("interaction", "addition")
aictab(model.set, modnames = model.names)

means_gr = tapply(dat$GrowthRate, list(dat$MaternalHost, dat$LarvalHost), mean)
means_gr #mean values

ses_gr = tapply(dat$GrowthRate, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
ses_gr 

colMeans(means_gr)
rowMeans(means_gr)


#plot the means and standard errors 
plot(c(0.05, 0.07), means_gr[,1], ylim=c(0.05,0.09), xlim=c(0.8, 2.2),
     xlab="Larval host", 
     ylab="Growth rate",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))

arrows(c(0.97,1.03), means_gr[,1]-ses_gr[,1], c(0.97,1.03), 
       means_gr[,1]+ses_gr[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means_gr[,2]-ses_gr[,2], c(1.97,2.03), 
       means_gr[,2]+ses_gr[,2], length=0.01, angle=90, code=3)

segments(0.97, means_gr[1,1], 1.97, means_gr[1,2], lty=2)
segments(1.03, means_gr[2,1], 2.03, means_gr[2,2])

points(c(0.97,1.03), means_gr[,1], pch=c(21,16), bg="white")
points(c(1.97,2.03), means_gr[,2], pch=c(21,16), bg="white")

legend("topright", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))
title ("Maternal and lavra host in regard to growth rate", line=0.5)

#make gg plot for that system

ggplot(dat, aes(x = LarvalHost, y = GrowthRate)) +
  geom_boxplot(fill="grey")+
  labs(title = "Lavra host in regards to growth weight",
       x = "Lavra Host", y = " GrowthRate") +theme_minimal() + theme(
         axis.text = element_text(size = 14),
         plot.title = element_text(size = 14, hjust = 0.5),
         axis.title.y = element_text(size = 16),
         axis.title.x = element_text(size = 16))

ggplot(dat, aes(x = MaternalHost, y = GrowthRate)) +
  geom_boxplot(fill="grey")+
  labs(title = "Maternal host in regards to growth weight",
       x = "Maternal Host", y = " GrowthRate") +theme_minimal() + theme(
         axis.text = element_text(size = 14),
         plot.title = element_text(size = 14, hjust = 0.5),
         axis.title.y = element_text(size = 16),
         axis.title.x = element_text(size = 16))

#Additional statistics that are not used in the exersise
# adult weight dependency
m3 = lm(AdultWeight~Sex*LarvalHost, data=dat)
m4 = lm(AdultWeight~LarvalHost, data=dat)
anova(m3)
anova(m4)

# check which model is the best. Lower AIC value is best model
library(AICcmodavg)
model.set = list(m3, m4)
model.names = c("interaction", "addition")
aictab(model.set, modnames = model.names)

ggplot(dat, aes(x = LarvalHost, y = AdultWeight)) +
  geom_boxplot(fill="grey")+
  labs(title = "Lavra host in regards to adult weight",
       x = "Lavra Host", y = " AdultWeight")  +theme_minimal() +theme(
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5,face = "plain"),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          plot.title.position = "panel", plot.margin = margin(t = 20))


    