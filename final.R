dat = read.csv("C:/Users/ak1002/Box/courses/Statistics for Biologists/exam/exam2023_data.csv")
head(dat) 
summary(dat)

#libraries in use
library(ggplot2)    # plots
library(AICcmodavg) # for comparison of models
library(MASS)       # negative binomial
library(car)        # for residuals
library(pscl)       # Hurdle model

#check residuals in 3 parameters to be studied
hist(dat$euc_sdlgs0_50cm)
hist(dat$euc_sdlgs50cm.2m)
hist(dat$euc_sdlgs.2m)

#total number of trees found
sum(dat$euc_sdlgs0_50cm)
sum(dat$euc_sdlgs50cm.2m)
sum(dat$euc_sdlgs.2m)

#                      PART 1 (ANOVA)

#effect of season and landscape on different heights
model1 = lm(dat$euc_sdlgs0_50cm ~ dat$Season + dat$Landscape.position) 
model2 = lm(dat$euc_sdlgs50cm.2m ~ dat$Season + dat$Landscape.position) 
model3 = lm(dat$euc_sdlgs.2m ~ dat$Season + dat$Landscape.position) 

anova(model1)  #anova and analysis of first model
summary(model1)

#find the intercept for each parameter
model1 = lm(dat$euc_sdlgs0_50cm ~ dat$Season + dat$Landscape.position-1)
summary(model1)$coef

anova(model2) #anova and analysis of second model
summary(model2)

#find the intercept for each parameter
model2 = lm(dat$euc_sdlgs50cm.2m ~ dat$Season + dat$Landscape.position-1)
summary(model2)$coef

anova(model3) #anova and analysis of third model
summary(model3) 

#find the intercept for each parameter
model3 = lm(dat$euc_sdlgs.2m ~ dat$Season + dat$Landscape.position-1)
summary(model3)$coef

#Find the mean and stdev values for model 1 (season and landscape)
means_s1 = tapply(dat$euc_sdlgs0_50cm, list(dat$Season), mean)
means_s1 

err_s1 = tapply(dat$euc_sdlgs0_50cm, 
              list(dat$Season), 
              function(x) sd(x)/sqrt(sum(!is.na(x))))
err_s1

means_l1 = tapply(dat$euc_sdlgs0_50cm, list(dat$Landscape.position), mean)
means_l1 

err_l1 = tapply(dat$euc_sdlgs0_50cm, 
                list(dat$Landscape.position), 
                function(x) sd(x)/sqrt(sum(!is.na(x))))
err_l1

#Find the mean and stdev values for model 2 (season and landscape)
means_s2 = tapply(dat$euc_sdlgs50cm.2m, list(dat$Season), mean)
means_s2 

err_s2 = tapply(dat$euc_sdlgs50cm.2m, 
              list(dat$Season), 
              function(x) sd(x)/sqrt(sum(!is.na(x))))
err_s2

means_l2 = tapply(dat$euc_sdlgs50cm.2m, list(dat$Landscape.position), mean)
means_l2 

err_l2 = tapply(dat$euc_sdlgs50cm.2m, 
              list(dat$Landscape.position), 
              function(x) sd(x)/sqrt(sum(!is.na(x))))
err_l2

#Find the mean and stdev values for model 3 (season and landscape)
means_s3 = tapply(dat$euc_sdlgs.2m, list(dat$Season), mean)
means_s3 

err_s3 = tapply(dat$euc_sdlgs.2m, 
              list(dat$Season), 
              function(x) sd(x)/sqrt(sum(!is.na(x))))
err_s3

means_l3 = tapply(dat$euc_sdlgs.2m, list(dat$Landscape.position), mean)
means_l3 

err_l3 = tapply(dat$euc_sdlgs.2m, 
              list(dat$Landscape.position), 
              function(x) sd(x)/sqrt(sum(!is.na(x))))
err_l3

#plot models - season


#model 1
season= c("Autumn 2007","Spring 2006","Winter 2006" )
df = data.frame(season = season, means_s1 = means_s1, err_s1 = err_s1)

ggplot(df, aes(x = season, y = means_s1, fill = "Season")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_s1 - err_s1, ymax = means_s1 + err_s1),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees between 0 - 50 cm ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 2))

landscape= c("crest","flat","slope ", "toe to slope" )
df = data.frame(landscape = landscape, means_l1 = means_l1, err_l1 = err_l1)

ggplot(df, aes(x = landscape, y = means_l1, fill = "landscape")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_l1 - err_l1, ymax = means_l1 + err_l1),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees between 0 - 50 cm ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 3))

#model 2
season= c("Autumn 2007","Spring 2006","Winter 2006" )
df = data.frame(season = season, means_s2 = means_s2, err_s2 = err_s2)

ggplot(df, aes(x = season, y = means_s2, fill = "Season")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_s2 - err_s2, ymax = means_s2 + err_s2),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees between 50cm - 2 m ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 2))

landscape= c("crest","flat","slope ", "toe to slope" )
df = data.frame(landscape = landscape, means_l2 = means_l2, err_l2 = err_l2)

ggplot(df, aes(x = landscape, y = means_l2, fill = "landscape")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_l2 - err_l2, ymax = means_l2 + err_l2),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees between 50cm - 2 m ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 3))

#model 3

season= c("Autumn 2007","Spring 2006","Winter 2006" )
df = data.frame(season = season, means_s3 = means_s3, err_s3 = err_s3)

ggplot(df, aes(x = season, y = means_s3, fill = "season")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_s3 - err_s3, ymax = means_s3 + err_s3),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees above 2 m ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 0.6))

landscape= c("crest","flat","slope ", "toe to slope" )
df = data.frame(landscape = landscape, means_l3 = means_l3, err_l3 = err_l3)

ggplot(df, aes(x = landscape, y = means_l3, fill = "landscape")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7,fill = "lightblue") +
  geom_errorbar(aes(ymin = means_l3 - err_l3, ymax = means_l3 + err_l3),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", y = "N of trees above 2 m ") +
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14),  
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 3))


#                  PART 2 (Hurdle model)

#effect of native and perennial  grass on  0.5-2m trees

#both nb and hurdle are good choices thus model selection:
sum(dat$euc_sdlgs50cm.2m < 1)
sum(dat$euc_sdlgs50cm.2m > 1)

#models
nb_model = glm.nb(dat$euc_sdlgs50cm.2m ~ dat$NativePerennialGrass_cover + dat$ExoticPerennialGrass_cover)       
summary(nb_model)
1-(nb_model$deviance/nb_model$null.deviance)

hurdle_model = hurdle(dat$euc_sdlgs50cm.2m ~ dat$NativePerennialGrass_cover + dat$ExoticPerennialGrass_cover, dist = "negbin")
summary(hurdle_model)

sum(predict(hurdle_model, type = "prob")[,1])      #predicts 274 of 275 zeros of the data

# First 5 expected counts
predict(hurdle_model, type = "response")[1:5]


#model selection
AIC_hurdle <- AIC(hurdle_model)
BIC_hurdle <- BIC(hurdle_model)

AIC_nb <- AIC(nb_model)
BIC_nb <- BIC(nb_model)

cat("AIC - Hurdle:", AIC_hurdle, "\n")
cat("BIC - Hurdle:", BIC_hurdle, "\n")
cat("\n")
cat("AIC - Negative Binomial:", AIC_nb, "\n")
cat("BIC - Negative Binomial:", BIC_nb, "\n")

#not conclusive -- AIC is less for Hurdle -- choose Hurdle!


#histograms of residuals
hist(residuals(hurdle_model))

qqPlot(hurdle_model$residuals,
       id = FALSE # remove point identification
)

