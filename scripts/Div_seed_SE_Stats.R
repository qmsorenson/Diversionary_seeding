library(dplyr)
library(lme4)
library(lmerTest)
library(lsmeans)
library(effects)
library(ggplot2)
library(tidyr)
library(phia)
library(AICcmodavg)

load(file = "G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_seeding_SE/Div_seed_SE_data.RData") #load data

### Binomial model for Tephrosia virginiana  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
glmt1 <- glmer(bintep ~ cc*time + scale(shrub)*time + cc*et + (1|site/tray), 
               data = dise[dise$wrong == "N",], control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), family = "binomial")
summary(glmt1)
plot(allEffects(glmt1))
testInteractions(glmt1, custom = c(list(cc = c(1, -1)), list(time = c(1, 0))), adjustment="none") # D-U time 1
testInteractions(glmt1, custom = c(list(cc = c(1, -1)), list(time = c(0, 1))), adjustment="none") # D-U time 2
testInteractions(glmt1, custom = c(list(time = c(1, 0))), slope ="scale(shrub)",  adjustment="none") # shrub slope time 1
testInteractions(glmt1, custom = c(list(time = c(0, 1))), slope ="scale(shrub)",  adjustment="none") # shrub slope time 2

hist(log(dise$cor))


test <- lmer(cor~ time*cc + (1|site), data = dise[dise$wc == "C" & dise$wrong == "N",])
summary(test)
plot(allEffects(test))

glmt2 <- glmer(bintep ~ cc + scale(shrub) + (1|site/tray), 
               data = dise[dise$time == "1" & dise$wrong == "N",], family = "binomial")
summary(glmt2)
plot(allEffects(glmt2))

glmt3 <- glmer(bintep ~ cc + scale(shrub) + (1|site/tray)+ (1|obs), 
               data = dise[dise$time == "2" & dise$wrong == "N",], family = "binomial")
summary(glmt3)
plot(allEffects(glmt3))


### Binomial model for Vernonia angustifolia ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

glmv1 <- glmer(binver ~ scale(shrub)*time + et+cc*time + (1|site/tray), 
               data = dise[dise$wrong == "N",], control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), family = "binomial")
summary(glmv1)
plot(allEffects(glmv1))
testInteractions(glmv1, custom = c(list(cc = c(1, -1)), list(time = c(1, 0))), adjustment="none") # D-U time 1
testInteractions(glmv1, custom = c(list(cc = c(1, -1)), list(time = c(0, 1))), adjustment="none") # D-U time 2

testInteractions(glmv1, custom = c(list(et = c(1, -1)), list(time = c(0, 1)), list(cc = c(1, 0))), adjustment="none") # F-O time 1
testInteractions(glmv1, custom = c(list(et = c(1, -1)), list(time = c(1, 0)), list(cc = c(0, 1))), adjustment="none") # F-O time 2



testInteractions(glmv1, custom = c(list(time = c(1, 0))), slope ="scale(shrub)",  adjustment="none") # shrub slope time 1
testInteractions(glmv1, custom = c(list(time = c(0, 1))), slope ="scale(shrub)",  adjustment="none") # shrub slope time 2


glmv2 <- glmer(binver ~ cc+scale(shrub)*time + et +(1|site/tray), 
               data = dise[dise$time == "1" & dise$wrong == "N",], family = "binomial")
summary(glmv2)
plot(allEffects(glmv2))

glmv3 <- glmer(binver ~ cc + scale(shrub) + et +(1|site/tray), 
               data = dise[dise$time == "2" & dise$wrong == "N",], family = "binomial")
summary(glmv3)
plot(allEffects(glmv3))

### Creating predicted values for figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predict_tep <- dise %>%
  expand(nesting(cc, et),shrub)
predict_ver <- dise %>%
  expand(nesting(cc, et),shrub)

predicted_tep <- cbind(predict_tep, as.data.frame(predictSE.merMod(glmt1, newdata = predict_tep, level = 0, type = "response", se.fit = TRUE)))

predicted_ver <- cbind(predict_ver, as.data.frame(predictSE.merMod(glmv1, newdata = predict_ver, level = 0, type = "response", se.fit = TRUE)))


save(glmt1, glmv1, predicted_tep, predicted_ver, file = "G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_seeding_SE/Div_seed_SE_modpred.RData")




###############################################################################################################
##################################################   TRASH   ##################################################
###############################################################################################################

#lmt1 <- lmer(arctep ~ cc + shrub*time + (1|site/tray), data = dise[dise$wrong == "N",])
#summary(lmt1)
#lsmt1 <- lsmeansLT(lmt1)
#plot(allEffects(lmt1))
#plot(lmt1)


#lmt3 <- lmer(arcver ~ cc + shrub*time + wc + (1|site/tray), data = dise[dise$wrong == "N",])
#summary(lmt3)


lmt2 <- lmer(tep ~ cc+et + (1|site) + (1|time), data = dise[dise$wrong == "N",])
summary(lmt2)
lsmeansLT(lmt2)
plot(allEffects(lmt2))

lmv1 <- lmer(ver ~ cc+et + wc+ (1|site), data = dise[dise$time == "2" & dise$wrong == "N",])
anova(lmv1)
lsmv1 <- lsmeansLT(lmv1)
plot(allEffects(lmv1))