
library(dplyr)
library(lme4)
library(lmerTest)
library(reshape2)
library(ggplot2)

sdrem <- read.csv("G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_Seeding_WI_seed_removal.csv")
sdsur <- read.csv("G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_Seeding_WI_seedling_surveys.csv")
spplist <- read.csv("spplistchecked.csv")
sdmas <- read.csv("G:/My Drive/Graduate School/Research/Diversionary seeding/data/BatchedDispSeedTraits.csv")

sdrem <- sdrem %>% 
  rename(site = Site, cc = Cracked.corn., trap = Rodent.or.Bird, des = Desmodium, ver = Vernonia, pn = Pair.number)
spplist <- spplist %>%
  select(-order)
sdmas <- sdmas %>% 
  rename(dm = DispersuleMass_mg, dis = DispersuleSameAsSeed, sm = SeedMass_mg)
sdmas$sm <- ifelse(sdmas$dis == "TRUE", sdmas$dm, sdmas$sm)
sdmas <- aggregate(sm ~ spp, data = sdmas, FUN = mean)
sdsur <- sdsur %>%
  rename(site = Site, pn = Pair.., cc = Cracked.corn, quad = Quad.., spp = Species, count = Count) %>%
  left_join(spplist, by = "spp") 

###### data exploration

new <- spplist %>% 
  filter(seeded == "y") %>%
  left_join(sdmas, by ="spp") 
  


######

sdsur.y <- sdsur[sdsur$seeded == "y",]

sdsur.y$spp <- ifelse(sdsur.y$spp == "EUPALT", "EUPPER", sdsur.y$spp)
sdsur.y$spp <- ifelse(sdsur.y$spp == "BAPALB", "BAPBRA", sdsur.y$spp)
sdsur.y$spp <- ifelse(sdsur.y$spp == "SYMOBL", "SYMNOV", sdsur.y$spp)
sdsur.y$spp <- ifelse(sdsur.y$spp == "SYMOOL", "SYMNOV", sdsur.y$spp)
sdsur.y$spp <- ifelse(sdsur.y$spp == "SOLSPP", "SOLNEM", sdsur.y$spp)
sdsur.y$spp <- ifelse(sdsur.y$spp == "KUHEUP", "BRIEUP", sdsur.y$spp)
sdsur.cast <- dcast(sdsur.y, site + pn + cc + quad ~ spp, value.var = "count", fun.aggregate = mean)
sdsur.cast <- sdsur.cast[!is.na(sdsur.cast$pn),]
sdsur.cast[is.na(sdsur.cast)] <- 0
sdsur.melt <- melt(sdsur.cast, id = c("site", "pn", "cc", "quad"), variable.name = "spp")
sdsur.melt <- left_join(sdsur.melt, sdmas, by ="spp")
sdsur.melt <- sdsur.melt[!is.na(sdsur.melt$sm),]
sdsur.melt$pa <- ifelse(sdsur.melt$value == 0, 0, 1)

sdsur.melt[sdsur.melt$site == "ANDERSON" & sdsur.melt$quad = 

des1 <- lmer(des ~ cc + (1|site/pn), sdrem[sdrem$trap == "R",])
ver1 <- lmer(ver ~ cc + (1|site/pn), sdrem[sdrem$trap == "R",])

summary(des1)
lsdes1 <- lsmeansLT(des1)
lsver1 <- lsmeansLT(ver1)

ggplot(data = lsdes1$lsmeans.table, 
       aes(x = factor(cc, labels = c("N" = "Hungry", "Y" = "Fed")), y = Estimate, fill = cc)) + 
  scale_fill_manual(values=c("Grey50", "white")) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymax = Estimate +`Standard Error`, ymin=Estimate -`Standard Error`), position=position_dodge(.9),  width = 0.1) +
  #guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = expression(paste("Desmodium canadense"))) +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 3 )) +
  #ylab("") + 
  ylab("# Seeds remaining") + 
  xlab(NULL) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"), legend.position="none") 

ggplot(data = lsver1$lsmeans.table, 
       aes(x = factor(cc, labels = c("N" = "Hungry", "Y" = "Fed")), y = Estimate, fill = cc)) + 
  scale_fill_manual(values=c("Grey50", "white")) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymax = Estimate +`Standard Error`, ymin=Estimate -`Standard Error`), position=position_dodge(.9),  width = 0.1) +
  #guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = expression(paste("Vernonia facsiculata"))) +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 3 )) +
  #ylab("") + 
  ylab("# Seeds remaining") + 
  xlab(NULL) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"), legend.position="none") 





plot(resid(des1))
hist(resid(des1))
anova(des1)

sdsur.ag.rich <- aggregate(spp ~ site + pn + cc + quad + seeded, data = sdsur, FUN = length)
sdsur.ag.abun <- aggregate(count ~ site + pn + cc + quad + seeded, data = sdsur, FUN = sum)
sdsur.nonasm <- sdsur[!is.na(sdsur$sm),]

sdnum <- lmer(count ~ cc + (1|site/pn/quad), data = sdsur.ag.abun %>% filter(seeded =="y"))
sdspp <- lmer(spp ~ cc + (1|site/pn/quad), data = sdsur.ag.rich %>% filter(seeded == "y"))

lsnum <- lsmeansLT(sdnum)
lsspp <- lsmeansLT(sdspp)

ggplot(data = lsnum$lsmeans.table, 
       aes(x = factor(cc, labels = c("N" = "Hungry", "Y" = "Fed")), y = Estimate, fill = cc)) + 
  scale_fill_manual(values=c("Grey50", "white")) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymax = Estimate +`Standard Error`, ymin=Estimate -`Standard Error`), position=position_dodge(.9),  width = 0.1) +
  #guides(fill = guide_legend(title = NULL), color = NULL ) +
  #labs(title = "Seeded species \n abundance") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 3 )) +
  #ylab("") + 
  ylab("Abundance of seeded seedlings \nper 0.25 m2") + 
  xlab(NULL) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"), legend.position="none") 

ggplot(data = lsspp$lsmeans.table, 
       aes(x = factor(cc, labels = c("N" = "Hungry", "Y" = "Fed")), y = Estimate, fill = cc)) + 
  scale_fill_manual(values=c("Grey50", "white")) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymax = Estimate +`Standard Error`, ymin=Estimate -`Standard Error`), position=position_dodge(.9),  width = 0.1) +
  #guides(fill = guide_legend(title = NULL), color = NULL ) +
  #labs(title = "Seeded species \n richness") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 3 )) +
  #ylab("") + 
  ylab("Richness of seeded species \nper 0.25 m2") + 
  xlab(NULL) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size = 24, color = "black"), legend.position="none") 




mlmsm <- glmer(value ~ cc*scale(sm) + (1 + cc|spp) + (1|site/pn/quad), data = sdsur.melt, family = poisson)
summary(mlmsm)

anova(sdnum)
anova(sdspp)
lsmeansLT(sdnum)
lsmeansLT(sdspp)
