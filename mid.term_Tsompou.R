#add all necessary data 
dat = read.delim("C:/Users/ak1002/Box/courses/Statistics for Biologists/data/tephritis.txt")
data = data.frame(dat$Sex, dat$Wing_length, dat$Wing_width, dat$Melanized_area)
pairs(data [-1])      #quickly check how the plots look like 

 #check residuals in 3 parameters to be studied
hist(log(dat$Wing_area))
hist(dat$Wing_area)
hist(dat$Melanized_area)

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(AICcmodavg) # for comparison of models
library(ggplot2)    # plots

#                                PART 1: morphological interactions and changes between sex


#fit the linear model
model1=lm(dat$Melanized_area ~ dat$Wing_area) 

#extract coef from model1
summary(model1)
cf = model1$coef             
cf 
predvals = cf[1] + cf[2]*dat$Wing_area

# to see the change of y for a stdev of x
x_sd_change=(cf[2]*(mean(dat$Wing_area) + sd(dat$Wing_area)) - cf[2] * mean (dat$Wing_area))
x_sd_change

#plot the overall behavior
plot(dat$Wing_area, dat$Melanized_area, las=1,    
     ylab="Melanized area (mm)",
     xlab="Wing area (mm)",
     main= "",
     col='black', pch=21, bg="lightblue")
abline(model1)  
segments(dat$Wing_area,dat$Melanized_area,dat$Wing_area,predvals, col="black")

#check if the behavior is different in males and females

#seperate data into females and males 
males=dat[dat$Sex=="Male",]
females= dat[dat$Sex=="Female",]

#fit linear models
mm = lm(Melanized_area ~ Wing_area, data=males)
mf = lm(Melanized_area ~ Wing_area, data=females)

summary(mm)
summary(mf)

#histograms of residueal
hist(residuals(mm))
hist(residuals(mf))

#make the lines through data 
xxm = seq(min(males$Wing_area), max(males$Wing_area), 
         length.out=100)
xxf = seq(min(females$Wing_area), max(females$Wing_area), 
          length.out=100)

yym = mm$coef[1] + mm$coef[2]*xxm
yyf = mf$coef[1] + mf$coef[2]*xxf

plot(males$Wing_area, males$Melanized_area,
     xlab="Wing area (mm)",
     ylab="Melanized area (mm)",
     las=1, pch=21, col="black", bg="lightblue")
lines(xxm, yym, lwd=2)
points(females$Wing_area, females$Melanized_area,
       pch=21, col="black", bg="firebrick")
lines(xxf, yyf, lwd=2, lty=2)
legend("bottomright", pch=c(16,16), 
       col=c("lightblue", "firebrick"),
       legend=c("Males","Females"))

#anCova between the sexes
md1=lm(dat$Melanized_area ~ dat$Wing_area * dat$Sex)
anova(md1)
summary(md1)


md2=lm(dat$Melanized_area~dat$Wing_area + dat$Sex) #better fit
anova(md2)
summary(md2)

#model selection
model.set = list(md1, md2)
model.names = c("1", "2")
aictab(model.set, modnames = model.names)

#find the means and CV to calculate deviaton from mean values
sexmeans = tapply(dat$Melanized_area, dat$Sex, mean, na.rm=T)
sexmeans
sd(sexmeans)*100
sexcvs = tapply(dat$Melanized_area, dat$Sex, 
                function(x) 100*sd(x, na.rm=T))
sexcvs
mean(sexcvs)  #cv from the means for the two sexes

#see how the model is fitted (residuals)
par(mfrow=c(2,2))
plot(md2)
par(mfrow=c(1,1))

#                          2nd part of data: ANOVA on the patry and sex

m = lm(Melanized_area ~ Patry * Sex  , data=dat) 
m1 = lm(Melanized_area ~ -Patry + Sex , data=dat)

#model selection
model.set = list(m, m1)
model.names = c("0", "1")
aictab(model.set, modnames = model.names)

anova(m)
summary(m)

#see how the model is fitted (residuals)
par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))


#See the mean values
means_p = tapply(dat$Melanized_area, list(dat$Patry), mean)
means_p 

means_s = tapply(dat$Melanized_area, list(dat$Sex), mean)     #same as sexmeans
means_s

err_p = tapply(dat$Melanized_area, 
               list(dat$Patry), 
               function(x) sd(x)/sqrt(sum(!is.na(x))))
err_p

err_s = tapply(dat$Melanized_area, 
               list(dat$Sex), 
               function(x) sd(x)/sqrt(sum(!is.na(x))))
err_s 

#plot the means and standard errors (bars)

#melanin vs sex
Gender = c("Female", "Male")
df = data.frame(gender = Gender, means_s = means_s, err_s = err_s)

ggplot(df, aes(x = Gender, y = means_s, fill = "Gender")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_s - err_s, ymax = means_s + err_s),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "Melanized Area by Gender", y = "Melanized Area") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 5.2))


#melanin vs patry 
Patry= c("Allopatry", "Sympatry")
df = data.frame(Patry = Patry, means_p = means_p, err_p = err_p)

ggplot(df, aes(x = Patry, y = means_p, fill = "Patry")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_p - err_p, ymax = means_p + err_p),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "Melanized Area by Allo/Sym-Patry", y = "Melanized Area") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 5.2))

#ancova with everything to see if i missing any interactions

ex1 = lm(Melanized_area ~ Patry + Sex + Wing_area , data=dat)
ex2 = lm(Melanized_area ~ Patry * Wing_area + Sex  , data=dat)

#model selection
model.set = list(ex1, ex2)
model.names = c("1", "2")
aictab(model.set, modnames = model.names)


