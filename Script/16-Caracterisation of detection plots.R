rm(list = ls())


#~~~~~~#
library(furrr); library(tidyverse); library(lubridate)
library(viridis); library(RColorBrewer); library(ggsci); library(cowplot)
#~~~~~~#

mydf <- readRDS("Data/Fulmar/detect_final_zoned.rds") #Change to output path of script n?12 #!!#
mydf <- mydf[-which(is.na(mydf$zone)),] 

#Fig of duration and probability for colonies ####
# Group by session####
grpsess <- mydf %>% group_by(session_id) %>% dplyr::summarise(colony = min(colony), encounter = n(), Yt = min(Yt), duration = mean(duration), zone = first(zone))
grpsess$encwinter <- grpsess$encounter/grpsess$Yt

#Group by colony####
grpcol <- grpsess %>% group_by(colony) %>%
  dplyr::summarise(n = n(), meandur = mean(duration), sddur = sd(duration), meanenc = mean(encwinter), sdenc = sd(encwinter), Yt = sum(Yt))

#add se
grpcol$sedur <- grpcol$sddur/sqrt(grpcol$n)
grpcol$seenc <- grpcol$sdenc/sqrt(grpcol$n)

#order west to east
grpcol$colony <- c("Alkefjellet",
                   "Bj?rn?ya",
                   "Breid. & R.",
                   "Eynhallow",
                   "Faroe Islands",
                   "Jan Mayen",
                   "Jarsteinen",
                   "Lang. & S. & G.",
                   "Papey & H.")
grpcol$colony <- as.factor(grpcol$colony)
grpcol$colony <- ordered(grpcol$colony, levels = c("Breid. & R.",
                                                   "Lang. & S. & G.",
                                                   "Papey & H.",
                                                   "Jan Mayen",
                                                   "Faroe Islands",
                                                   "Eynhallow",
                                                   "Jarsteinen",
                                                   "Alkefjellet",
                                                   "Bj?rn?ya"))

#Plot ####
fig4B <- ggplot(grpcol, aes(x = colony, y = meandur, ymin = meandur-sedur, ymax = meandur+sedur))+
  geom_point(size = 7)+
  geom_segment(aes(x = 1, xend = 9, y = 43, yend = 43), size = 1) +
  geom_text(x = 5, y = 45, label = "ns", size = 10)+
  geom_errorbar(width = 0.25, size = 1)+
  coord_cartesian(ylim = c(18,45))+
  theme_light() +
  labs(x = "Colony", y = "Mean duration of light encounters\n(min)", title = "B.")+
  theme(text = element_text(size = 35),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_line(color = "#C4C4C4"))

plot(fig4B)

#Using modeled values
tabmod$colony <- c("Alkefjellet",
                   "Bj?rn?ya",
                   "Breid. & R.",
                   "Eynhallow",
                   "Faroe Islands",
                   "Jan Mayen",
                   "Jarsteinen",
                   "Lang. & S. & G.",
                   "Papey & H.")
tabmod$colony <- as.factor(tabmod$colony)
tabmod$colony <- ordered(tabmod$colony, levels = c("Breid. & R.",
                                                   "Lang. & S. & G.",
                                                   "Papey & H.",
                                                   "Jan Mayen",
                                                   "Faroe Islands",
                                                   "Eynhallow",
                                                   "Jarsteinen",
                                                   "Alkefjellet",
                                                   "Bj?rn?ya"))

fig4A <- ggplot(tabmod, aes(x = colony, y = estperc, ymin = SEmin, ymax =SEmax, label = signif))+
            geom_point(size = 7)+
            geom_errorbar(width = 0.25, size = 1)+
            geom_text(aes(y= stat(ymax)),vjust=-1, size = 10)+
            coord_cartesian(ylim = c(0,1.2))+
            labs(x = "Colony", y = "Probability of encounter (%)", title = "A.")+
            theme_light()+
            theme(text = element_text(size = 35),
                  panel.grid = element_line(color = "#C4C4C4"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank())

plot_grid(fig4A, fig4B, nrow = 2)
ggsave(filename = "fig4.svg", units = "in", width = 16, height = 18)
