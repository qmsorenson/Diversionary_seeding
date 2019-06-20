library(reshape2)
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(Hmisc)
library(MuMIn)
library(lsmeans)
library(effects)
library(ggplot2)
library(grid)
library(gridExtra)

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

rotatedAxisElementText = function(angle,position='x',Size,Color){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust, size = Size, color = Color)
}

load(file = "G:/My Drive/Graduate School/Research/Diversionary seeding/data/Diversionary_seeding_SE/Div_seed_SE_modpred.RData")

##############################################################################################################
######################################           Figure 1           ##########################################
##############################################################################################################



ggplot(data = predicted_tep %>% filter(shrub == 20, time == "1"), 
       aes(x = cc, y = 1-fit, color = cc, fill= cc)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#8AA1B1","#B9D8C2"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 
### 3.16, 3.93 size of actual 1.85 x 2.30

ggplot(data = predicted_tep %>% filter(shrub == 20, time == "2"), 
       aes(x = cc, y = 1-fit, color = cc, fill= cc)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#8AA1B1","#B9D8C2"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 

ggplot(data = predicted_ver %>% filter(shrub == 20, time == "2", et == "O"), 
       aes(x = cc, y = 1-fit, color = cc, fill= cc)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#8AA1B1","#B9D8C2"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 


ggplot(data = predicted_tep %>% filter(shrub == 20, et == "O"), 
       aes(x = cc, y = 1-fit, color = cc, fill= cc)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#8AA1B1","#B9D8C2"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 

ggplot(data = predicted_tep %>% filter(shrub == 20, cc == "D"), 
       aes(x = et, y = 1-fit, color = et, fill= et)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#ACBDBA","#ECE4B7"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 
#"#818D92","#ADC698"#D3D0CB
## 3.82 x 3.49

ggplot(data = predicted_tep %>% filter(shrub == 20, cc == "U"), 
       aes(x = et, y = 1-fit, color = et, fill= et)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#ACBDBA","#ECE4B7"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 

ggplot(data = predicted_ver %>% filter(shrub == 20, cc == "D"), 
       aes(x = et, y = 1-fit, color = et, fill= et)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#ACBDBA","#ECE4B7"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 

ggplot(data = predicted_ver %>% filter(shrub == 20, cc == "U"), 
       aes(x = et, y = 1-fit, color = et, fill= et)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 7, shape = 21, color = c("#483D3F","#4A5043"), fill = c("#ACBDBA","#ECE4B7"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 
