# Info --------------------------------------------------------------------
##
##Script name: 04_graph
##
##Purpose of script: Preparing the data for the paper "Where the God Particles touches the ground - The local economic impact of RI procurement " 
##
##Author: Gabriele Piazza
##
##Date Created: 2023-02-20
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes:
##   
##



# Load Libraries ----------------------------------------------------------

options(scipen = 999)
require("devtools")
install.packages("remotes")
install.packages("kbal")
install.packages("stringi")
install.packages("eeptools")
devtools::install_github("chadhazlett/KBAL")
remotes::install_github("xuyiqing/tjbal")
install.packages("Hmisc")
install.packages("skimr")
install.packages("tabulator")
packages <- c("easycsv", "janitor", "tidyverse")
library(easycsv)
library(janitor)
library(eeptools)
library(stringi)
library(foreign)
library(haven)
library(tidyverse)
library(sf)
library(tmap)
library(here)
library(cowplot)
library(readxl)
library(tjbal)
library(data.table)
library(cowplot)
setwd(here())
`%notin%` <- Negate(`%in%`)

# Load Data ---------------------------------------------------------------

manufacturing_mun_2012<- readRDS(here( "results", "output", "out_manufacturing_2012_mun.rds"))
placebo_manufacturing_2012_mun <- read_csv(here("Analysis", "results","output", "out_placebo_manufact.csv"))
placebo_manufacturing_2012_mun<- placebo_manufacturing_2012_mun %>% rename(RMSPE_ratio = 'postloss_manufacturing/preloss_manufacturing')
manufacturing_orbis_25 <- readRDS(here("Analysis", "results", "output", "out_manufacturing_orbis_25.rds"))
placebo_orbis_25<- read_csv(here("Analysis","results", "output", "out_placebo_orbis_25.csv")) %>% 
  rename(RMSPE_ratio = 'postloss_orbis_mun/preloss_orbis_mun')
non_tradable_mun_2012<-readRDS(here("Analysis","results", "output", "out_non_tradable_mun.rds"))
placebo_non_tradable<- read_csv(here("Analysis","results", "output", "out_placebo_non_tradable.csv"))
manufacturing_mun_2010<- readRDS(here("results", "output", "out_manufacturing_2010.rds"))
placebo_manufacturing_2010_mun<- read_csv(here("results", "output", "out_placebo_manufact_2010.csv"))
placebo_manufacturing_2008_mun<- read_csv(here("results", "output", "out_placebo_manufact_2008.csv"))
leave1O_manufact_mun<- readRDS(here("Analysis","results","output", "out_manufacturing_l1o.rds"))
leave1o_mat_mun<- read.table(here("Analysis","results", "output", "leave_one_out_mun_mat.txt"),header = F)
manufacturing_lma_2012<- readRDS(here("Analysis","results", "output", "out_manufacturing_2012_lma.rds"))
placebo_manufacturing_2012_lma<- read_csv(here("results", "output", "out_placebo_lma_manufact.csv")) %>% 
  rename(RMSPE_lmas_manufact =  "postloss_lma_manufacturing/preloss_lma_manufacturing")

manufacturing_mun_2012_weights<-readRDS(here("Analysis","results", "output", "out_manufacturing_2012_mun_weights.rds"))
placebo_manufacturing_weights<- read_csv(here("Analysis","results", "output", "out_placebo_manufact_weights.csv")) %>% 
  rename(RMSPE_mun_manufact="postloss_manufacturing_selected/preloss_manufacturing_selected")

non_tradable_mun_2012_weights<-readRDS(here("Analysis","results", "output", "out_non_tradable_2012_weights.rds"))
placebo_non_tradable_weights<- read_csv(here("Analysis","results", "output", "out_placebo_non_tradable_weights.csv")) %>% 
rename(RMSPE_mun_non_tradable="postloss_non_tradable_selected/preloss_non_tradable_selected")

tradable_mun_2012_weights<- readRDS(here("Analysis","results", "output", "out_tradable_mun_2012_covariates_weights.rds"))
placebo_tradable_weights<- read_csv(here("Analysis","results", "output", "out_placebo_tradable_weights.csv")) %>% 
  rename(RMSPE_mun_tradable="postloss_tradable_selected/preloss_tradable_selected")
# Figure 2 ----------------------------------------------------------------
## Panel A
fig_2_panel_a<- plot(manufacturing_mun_2012, type = "counterfactual", xlab = "Year", 
                               ylab = "Log Manufacturing employees",ylim = c(8.7, 9.1),
                               legend.pos = "top",
                               main="Panel a",
                               legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1,cex.main= 0.7, cex.text = 1, 
                               cex.lab = 1 ,count = FALSE)

fig_2_panel_a<- fig_2_panel_a+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())



## Panel B

fig_2_panel_b<- ggplot(data = placebo_manufacturing_2012_mun, aes(y =sample(seq_along(RMSPE_ratio)), x = RMSPE_ratio)) +
  ggtitle("Panel b: RMSPE-ratio distribution")+
  geom_point(data = subset(placebo_manufacturing_2012_mun, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_manufacturing_2012_mun, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_manufacturing_2012_mun, municipalities_names == "SCHIO"), aes(label = municipalities_names), hjust = 1.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #panel.background = element_blank())+
  ylab(NULL)+
  xlab("RMSPE")

theme()

# Putting Panel A and Panel B together 
fig_2<- plot_grid(fig_2_panel_a, fig_2_panel_b)
ggsave(here("results","figures","fig_2.png"), plot = fig_2, dpi = 300, width = 9, height = 6, limitsize = F)



# Figure 3 ----------------------------------------------------------------

schio_sector_analysis<- fread(here("data_proc","schio_sector_analysis.csv"),header= TRUE)
schio_sector_analysis[schio_sector_analysis =="13 - Industrie tessili"]<- "13 - Textile Production"
schio_sector_analysis[schio_sector_analysis =="17 - Fabbricazione di carta"]<- "17 - Papermaking"
schio_sector_analysis[schio_sector_analysis =="25- Prodotti in metallo"] <- "25 - Metal products"
schio_sector_analysis[schio_sector_analysis =="28 - Macchinari ed apparecchiature"]<- "28 - Machinery & Equipment"
schio_sector_analysis[schio_sector_analysis =="31 - Fabbricazione di mobili"]<- "31 - Furniture manufacturing"
schio_sector_analysis[schio_sector_analysis =="33 - Riparazione di macchine"]<- "33 - Machine repair"

schio_sector_analysis<- schio_sector_analysis %>% mutate(zanon_sector =case_when(Code =="28 - Machinery & Equipment" ~ 1,
                                                                                 Code !="28 - Machinery & Equipmente" ~ 0)) 

schio_sector_analysis$zanon_sector<- as.factor(schio_sector_analysis$zanon_sector)
fig_3<- ggplot(schio_sector_analysis, aes(y = change_2012_2019, x = reorder(Code, -change_2012_2019),
                                          fill = Code, alpha = zanon_sector)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(legend.position = "none", 
        legend.text = element_text(colour = "blue", size = 4),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14)) +
  labs(y = "Change number of employees 2012-2019", x = "") +
  scale_alpha_manual(values = c(0.6, 1.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



fig_3<- fig_3 +
  annotate("text", x = 8, y = 300, label = "Industry with largest input coefficient") +
  annotate("text", x = 15, y = 200, label = "E. Zanon's industry")

arrows <- 
  tibble(
    x1 = c(5, 2),
    x2 = c(16, 16),
    y1 = c(390, 200), 
    y2 = c(250, 10)
  )

fig_3<- fig_3 + geom_curve(data = arrows, aes(x= x1, y=y1, xend = x2, yend = y2), arrow = arrow(length= unit(0.08, "inch")),size =0.5,
                          curvature = 0.3)

ggsave(here("results","figures","fig_3.png"), plot = fig_3, dpi = 300, width = 9, height = 6, limitsize = F)

ggsave(here("results","figures", "fig_3.pdf"), , plot = fig_3, width = 15, height = 10, limitsize= F, dpi = 300, device = "pdf")

fig_3 <- ggplot(schio_sector_analysis, aes(y = change_2012_2019, x = reorder(Code, -change_2012_2019),
                                           fill = Code, alpha = zanon_sector)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(legend.position = "none", legend.text = element_text(colour = "blue", size = 4),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Change number of employees 2012-2019", x = "") +
  scale_alpha_manual(values = c(0.6, 1.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

fig_3 <- fig_3 +
  annotate("text", x = 5, y = 400, label = "Add text here") +
  annotate("text", x = 16, y = 300, label = "More text here")

arrows <- 
  data.frame(
    x1 = c(5, 2),
    x2 = c(16, 16),
    y1 = c(390, 200), 
    y2 = c(250, 10)
  )

fig_3 <- fig_3 + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrows)
,
                            arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
                            curvature = 0.3)




# Figure 4 ----------------------------------------------------------------

fig_4_panel_a<- plot(out_manufacturing_shift_2012_orbis_25_cov.kbal, type = "counterfactual", xlab = "Year", 
                         ylab = "Log Average turnover", main="Panel a: Trends in average turnover in Metal products (25)",
                         legend.pos = "top",
                         legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, ylim = c(13, 17),
                         cex.lab = 1 ,count = FALSE)


fig_4_panel_a<- fig_4_panel_a+ggtitle("Panel a: Trajectories in Metal Products")+
  theme(axis.title=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())


fig_4_panel_b<- ggplot(data = placebo_orbis_25, aes(y =sample(seq_along(RMSPE_ratio)), x = (RMSPE_ratio))) +
  geom_point(data = subset(placebo_orbis_25, mun_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_orbis_25, mun_names != "SCHIO"))+
  geom_text(data = subset(placebo_orbis_25, mun_names == "SCHIO"), aes(label = mun_names),hjust = 1.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_4 <- plot_grid(fig_4_panel_a, fig_4_panel_b)
ggsave(here("results","figures","fig_4.png"), plot = fig_4, dpi = 300, width = 9, height = 6, limitsize = F)



# Figure 5 ----------------------------------------------------------------
fig_5_panel_a<- plot(non_tradable_mun_2012, type = "counterfactual", xlab = "Year", 
                     ylab = " Log Non-tradable employees", main="Panel a: Trends in (log) non_tradable, municipality",ylim = c(8.1, 8.5),
                     legend.pos = "top",
                     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.05, 
                     cex.lab = 1 ,count = FALSE)
fig_5_panel_a<- fig_5_panel_a +ggtitle("Panel a: Trajectories in Non-tradable")+
  theme(axis.title=element_text(size=12))

fig_5_panel_b<- ggplot(data = placebo_non_tradable, aes(y =sample(seq_along(RMSPE)), x = (RMSPE))) +
  geom_point(data = subset(placebo_non_tradable, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_non_tradable, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_non_tradable, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_5 <- plot_grid(fig_5_panel_a, fig_5_panel_b)
ggsave(here("Analysis","results","figures","fig_5.png"), plot = fig_5, dpi = 300, width = 9, height = 6, limitsize = F)


# Figure 6 ----------------------------------------------------------------
fig_6_panel_a<- plot(manufacturing_mun_2010, type = "counterfactual", xlab = "Year",
                                              ylab = "Manufacturing employees", main="Panel a: Trends in (log) manufacturing, municipality",
                                              legend.pos = "top",
                                              legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, ylim = c(8.7,9.1),
                                              cex.lab = 1 ,count = FALSE)

fig_6_panel_a<- fig_6_panel_a<+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12))

placebo_manufacturing_2010_mun<- placebo_manufacturing_2010_mun %>% filter(RMSPE<2281751314.332510)
fig_6_panel_b<- ggplot(data = placebo_manufacturing_2010_mun, aes(y =sample(seq_along(RMSPE)), x = (RMSPE))) +
  geom_point(data = subset(placebo_manufacturing_2010_mun, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_manufacturing_2010_mun, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_manufacturing_2010_mun, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_6 <- plot_grid(fig_6_panel_a, fig_6_panel_b)
ggsave(here("Analysis","results","figures","fig_6.png"), plot = fig_6, dpi = 300, width = 9, height = 6, limitsize = F)


# Figure 7 ----------------------------------------------------------------

Y1<-leave1O_manufact_mun$Y.bar[,1]
Y0<-leave1O_manufact_mun$Y.bar[,2]
Yleave<-as.matrix(cbind(Y1,Y1,Y1,Y1,Y1,Y1,Y1,Y1, Y1, Y1))
Yleave<- as.matrix(Yleave)
Yleave<-as.matrix(Yleave-leave1o_mat_mun)

fig_7<- plot(2004:2017, Y1,
                                        type="l",ylim=c(8.7,9.1), col="black",lty="solid",
                                        ylab ="Log Manufacturing employees",
                                        xlab ="Year",
                                        cex.lab = 1.6, cex.main = 1.6, cex.axis=1.6,
                                        main = NULL,
                                        xaxs = "i", yaxs = "i",
                                        lwd=2
)
abline(v=2012,lty="solid", col="grey", lwd=3)
for(i in 1:7){
  lines(2004:2017,Yleave[,i],col="orange",lty="solid")
}
lines(2004:2017,Y0,col="blue",lty="dashed", lwd=3)
lines(2004:2017,Y1,col="black",lty="solid",lwd=2)
legend(x="bottomleft",
       legend=c("Schio",
                "Synthetic Schio",
                "Synthetic Schio (leave-one-out)")
       ,lty=c("solid","dashed","solid"),
       col=c("black","blue","orange"),
       cex=1, bg="white",lwdc(3,4,3), bty = "n")


leave_one_out_manufacturing_est<- mean(storegapsL_manufacturing) 

ggsave(here("results","figures","fig_7.png"), plot = fig_7, dpi = 300, width = 9, height = 6, limitsize = F)


# Figure 8 ----------------------------------------------------------------

italy<- st_read(here("data_raw","LAU_RG_01M_2020_4326.shp", "LAU_RG_01M_2020_4326.shp"))
italy<- italy   %>% filter(CNTR_CODE == "IT")
plot(st_geometry(italy))

borders<- st_intersects(italy, italy)
namesof = lapply(borders, function(n){italy$LAU_NAME[n]})
id_of =lapply(borders,function(n){italy$LAU_ID[n]})
schio_neighbours<-as.vector(namesof[2984])

schio_neighbours<- schio_neighbours[[1]]
schio_neighbours<- str_to_upper(schio_neighbours)
schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")


schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")
plot(st_geometry(schio_boundaries))
ggplot(schio_boundaries)+geom_sf()


bordering_label <- c("Bordering","Bordering","Bordering","Bordering",
                     "Bordering","Bordering","Bordering","Bordering",
                     "Bordering","Bordering","Schio")
schio_neighbours_label <- cbind.data.frame(schio_neighbours,bordering_label)
schio_neighbours_label<- schio_neighbours_label %>% rename(municipality = schio_neighbours)
## Load Schio SLL
schio_sll<- read_csv("/Users/gabrielepiazza/Dropbox/PhD/Local_impact_procurement/Schio_SLL.csv")
schio_sll<- schio_sll %>% 
  rename(municipality="COMUNE", 
         sll = "DEN_SLL_2011_2018")

italy<- italy %>% 
  rename(municipality ="LAU_NAME")
schio_municipalities<- as.vector(schio_sll$municipality)


schio_municipalities <- italy %>% filter(municipality %in% schio_municipalities)
schio_municipalities<- schio_municipalities %>% left_join(schio_neighbours_label) 


ggplot(schio_municipalities)+geom_sf()
gg <- ggplot(schio_municipalities) +
  geom_sf(data = schio_municipalities, aes(fill = bordering_label), color = "black") + geom_label(aes(label = municipality))+
  theme_void()

tm_shape(schio_municipalities) +
  tm_polygons("bordering_label", legend.show =FALSE,palette="BuGn")+
  tm_basemap(server="OpenStreetMap",alpha=0.5)+
  tm_text("municipality", size = 0.75)



# # Figure 9  -------------------------------------------------------------


fig_9_panel_a <- plot(manufacturing_mun_2012_weights,type = "counterfactual", xlab = "Year", 
ylab = "Log manufacturing employees", main="Panel b: Trends in (log) manufact. employees, municipality",ylim = c(8.7, 9.1),
legend.pos = "top",
legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, cex.lab = 1 ,count = FALSE)

fig_9_panel_a<- fig_9_panel_a+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12))


fig_9_panel_b<-  ggplot(data = placebo_manufacturing_weights, aes(y =sample(seq_along(RMSPE_mun_manufact)), x = log(RMSPE_mun_manufact))) +
  geom_point(data = subset(placebo_manufacturing_weights, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_manufacturing_weights, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_manufacturing_weights, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_9<- plot_grid(fig_9_panel_a, fig_9_panel_b)
ggsave(here("Analysis","results","figures","fig_9.png"), plot = fig_9, dpi = 300, width = 9, height = 6, limitsize = F)


# Figure 13 ---------------------------------------------------------------

fig_13_panel_a<- plot(manufacturing_lma_2012, type = "counterfactual", xlab = "Year", 
                               ylab = "Log manufacuring employees", main="Panel b: Trends in (log) manufact. employees, LMA",ylim = c(9.5, 9.99),
                               legend.pos = "top",
                               legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, 
                               cex.lab = 1 ,count = FALSE)

fig_13_panel_a<- fig_13_panel_a+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12))

fig_13_panel_b<-  ggplot(data = placebo_manufacturing_weights, aes(y =sample(seq_along(RMSPE_mun_manufact)), x = log(RMSPE_mun_manufact))) +
  geom_point(data = subset(placebo_manufacturing_weights, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_manufacturing_weights, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_manufacturing_weights, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_13<- plot_grid(fig_13_panel_a, fig_13_panel_b)
ggsave(here("Analysis","results","figures","fig_13.png"), plot = fig_13, dpi = 300, width = 9, height = 6, limitsize = F)

# Figure 12 ---------------------------------------------------------------
italy<- st_read(here("data_raw","LAU_RG_01M_2020_4326.shp", "LAU_RG_01M_2020_4326.shp"))
italy<- italy   %>% filter(CNTR_CODE == "IT")
plot(st_geometry(italy))

borders<- st_intersects(italy, italy)
namesof = lapply(borders, function(n){italy$LAU_NAME[n]})
id_of =lapply(borders,function(n){italy$LAU_ID[n]})
schio_neighbours<-as.vector(namesof[2984])

schio_neighbours<- schio_neighbours[[1]]
schio_neighbours<- str_to_upper(schio_neighbours)
schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")


schio_boundaries<- italy %>% filter(LAU_NAME=="Schio")
plot(st_geometry(schio_boundaries))
ggplot(schio_boundaries)+geom_sf()


bordering_label <- c("Bordering","Bordering","Bordering","Bordering",
                     "Bordering","Bordering","Bordering","Bordering",
                     "Bordering","Bordering","Schio")
schio_neighbours_label <- cbind.data.frame(schio_neighbours,bordering_label)
schio_neighbours_label<- schio_neighbours_label %>% rename(municipality = schio_neighbours)
## Load Schio SLL
schio_sll<- read_csv("/Users/gabrielepiazza/Dropbox/PhD/Local_impact_procurement/Schio_SLL.csv")
schio_sll<- schio_sll %>% 
  rename(municipality="COMUNE", 
         sll = "DEN_SLL_2011_2018")

italy<- italy %>% 
  rename(municipality ="LAU_NAME")
schio_municipalities<- as.vector(schio_sll$municipality)


schio_municipalities <- italy %>% filter(municipality %in% schio_municipalities)
schio_municipalities<- schio_municipalities %>% left_join(schio_neighbours_label) 


ggplot(schio_municipalities)+geom_sf()
gg <- ggplot(schio_municipalities) +
  geom_sf(data = schio_municipalities, aes(fill = bordering_label), color = "black") + geom_label(aes(label = municipality))+
  theme_void()

tm_shape(schio_municipalities) +
  tm_polygons("bordering_label", legend.show =FALSE,palette="BuGn")+
  tm_basemap(server="OpenStreetMap",alpha=0.5)+
  tm_text("municipality", size = 0.75)



# Figure 14 ---------------------------------------------------------------

fig_14_panel_a <- plot(tradable_mun_2012_weights,type = "counterfactual", xlab = "Year", 
                      ylab = "Log tradable employees", main="Panel b: Trends in (log) tradable. employees, municipality",ylim = c(7.8, 8.2),
                      legend.pos = "top",
                      legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, cex.lab = 1 ,count = FALSE)

fig_14_panel_a<- fig_14_panel_a+ggtitle("Panel a: Trajectories in Tradable")+
  theme(axis.title=element_text(size=12))


fig_14_panel_b<-  ggplot(data = placebo_tradable_weights, aes(y =sample(seq_along(RMSPE_mun_tradable)), x = log(RMSPE_mun_tradable))) +
  geom_point(data = subset(placebo_tradable_weights, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_tradable_weights, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_tradable_weights, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_14<- plot_grid(fig_14_panel_a, fig_14_panel_b)
ggsave(here("Analysis","results","figures","fig_14.png"), plot = fig_14, dpi = 300, width = 9, height = 6, limitsize = F)


# Figure 15 ---------------------------------------------------------------
fig_15_panel_a <- plot(non_tradable_mun_2012_weights,type = "counterfactual", xlab = "Year", 
                       ylab = "Log non-tradable employees", main="Panel b: Trends in (log) tradable. employees, municipality",ylim = c(8.0, 8.4),
                       legend.pos = "top",
                       legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1.2,cex.main= 0.7, cex.text = 0.1, cex.lab = 1 ,count = FALSE)

fig_15_panel_a<- fig_15_panel_a+ggtitle("Panel a: Trajectories in Non-Tradable")+
  theme(axis.title=element_text(size=12))


fig_15_panel_b<-  ggplot(data = placebo_non_tradable_weights, aes(y =sample(seq_along(RMSPE_mun_non_tradable)), x = log(RMSPE_mun_non_tradable))) +
  geom_point(data = subset(placebo_non_tradable_weights, municipalities_names == "SCHIO"), color = "red") +
  geom_point(data = subset(placebo_non_tradable_weights, municipalities_names != "SCHIO"))+
  geom_text(data = subset(placebo_non_tradable_weights, municipalities_names == "SCHIO"), aes(label = municipalities_names),hjust = 0.5, vjust = 1.5)+
  theme(legend.position = "none",axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title=element_text(size=12,face="bold"))+
  ylab(NULL)+
  xlab("RMSPE Ratio") +ggtitle("Panel b: RMSPE-ratio distribution")

fig_15<- plot_grid(fig_15_panel_a, fig_15_panel_b)
ggsave(here("Analysis","results","figures","fig_15.png"), plot = fig_15, dpi = 300, width = 9, height = 6, limitsize = F)


# Create the plot
fig_spaghetti_2010<- ggplot(spaghetti_data_2010_long, aes(x = Year, y = Value, group = City)) +
  geom_line(data = filter(data_long, City != "SCHIO"), aes(color = "Other Municipalities"), size = 0.2) + # Set other cities to light grey
  geom_line(data = filter(data_long, City == "SCHIO"), aes(color = "SCHIO"), size = 1) + # Highlight Schio with a thicker, dark grey line
  geom_vline(xintercept = 2010, color = "grey", linetype = "dashed", size = 0.5) + # Treatment line, thinner
  scale_color_manual(values = c("SCHIO" = "darkgrey", "Other Municipalities" = "lightgrey")) + # Define manual colors
  theme_minimal(base_size = 12) + # Use a minimal theme
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_blank(), # Remove legend title
        plot.title = element_text(size = 14, face = "bold"), # Customize title
        axis.title = element_text(size = 12), # Customize axis titles
        axis.text = element_text(size = 10)) + # Customize axis text
  labs(title = "",
       x = "Year",
       y = "Placebo gaps in (log) Manufacturing Employees")

ggsave(here("results","figures","fig_spaghetti_2010.png"), plot = fig_spaghetti_2010, dpi = 300, width = 9, height = 6, limitsize = F)

fig_16<- ggplot(data_long, aes(x = Year, y = Value, group = City)) +
  geom_line(data = filter(data_long, City != "SCHIO"), aes(color = "Other Municipalities"), size = 0.2) + # Set other cities to light grey
  geom_line(data = filter(data_long, City == "SCHIO"), aes(color = "SCHIO"), size = 1) + # Highlight Schio with a thicker, dark grey line
  geom_vline(xintercept = 2008, color = "grey", linetype = "dashed", size = 0.5) + # Treatment line, thinner
  scale_color_manual(values = c("SCHIO" = "darkgrey", "Other Municipalities" = "lightgrey")) + # Define manual colors
  theme_minimal(base_size = 12) + # Use a minimal theme
  theme(legend.position = "bottom",  # Position the legend at the bottom
        legend.title = element_blank(), # Remove legend title
        plot.title = element_text(size = 14, face = "bold"), # Customize title
        axis.title = element_text(size = 12), # Customize axis titles
        axis.text = element_text(size = 10)) + # Customize axis text
  labs(title = "",
       x = "Year",
       y = "Placebo gaps in (log) Manufacturing Employees")

ggsave(here("results","figures","fig_16.png"), plot = fig_16, dpi = 300, width = 9, height = 6, limitsize = F)

# Figures draft -----------------------------------------------------------------

fig_x_panel_a<- plot(out_manufacturing_2010_centered_covariates.kbal, type = "counterfactual", xlab = "Year", 
                     ylab = "Log Manufacturing employees",ylim = c(8.7, 9.1),
                     legend.pos = "top",
                     main="Panel a",
                     legend.labs = c("Schio", "Synthetic Schio"), cex.legend = 1,cex.main= 0.7, cex.text = 1, 
                     cex.lab = 1 ,count = FALSE)

fig_2_panel_a<- fig_2_panel_a+ggtitle("Panel a: Trajectories in Manufacturing")+
  theme(axis.title=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())


