
library(dplyr)
library(lme4)
library(lmerTest)

sdrem <- read.csv("G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_Seeding_WI_seed_removal.csv")
sdsur <- read.csv("G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_Seeding_WI_seedling_surveys.csv")
spplist <- read.csv("spplistchecked.csv")

sdrem <- sdrem %>% 
  rename(site = Site, cc = Cracked.corn., trap = Rodent.or.Bird, des = Desmodium, ver = Vernonia, pn = Pair.number)
spplist <- spplist %>%
  select(-order)
sdsur <- sdsur %>%
  rename(site = Site, pn = Pair.., cc = Cracked.corn, quad = Quad.., spp = Species, count = Count) %>%
  left_join(spplist, by = "spp")
          


des1 <- lmer(des ~ cc + (1|site/pn), sdrem[sdrem$trap == "R",])
ver1 <- lmer(ver ~ cc + (1|site/pn), sdrem[sdrem$trap == "R",])

summary(des1)
plot(resid(des1))
hist(resid(des1))
anova(des1)

sdsur.ag.rich <- aggregate(spp ~ site + pn + cc + quad + seeded, data = sdsur, FUN = length)
sdsur.ag.abun <- aggregate(count ~ site + pn + cc + quad + seeded, data = sdsur, FUN = sum)

sdnum <- lmer(count ~ cc + (1|site/pn/quad), data = sdsur.ag.abun %>% filter(seeded =="y"))
sdspp <- lmer(spp ~ cc + (1|site/pn/quad), data = sdsur.ag.rich %>% filter(seeded == "y"))

anova(sdnum)
anova(sdspp)
lsmeansLT(sdnum)
lsmeansLT(sdspp)
