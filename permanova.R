mergulhadores <- read.csv("satisfação2.csv", header = TRUE, sep = ";" )

fat <- mergulhadores$categoria
met <- mergulhadores$subcategoria
mergulhadores$subcategoria <- NULL
mergulhadores$categoria <- NULL

##substituindo os valores NA pelas médias dos atributos
### baseado na discussão do research Link:
###https://www.researchgate.net/post/How_can_I_account_for_missing_data_on_an_18_Item_Scale
###The simplest methods are replacement for the average (or mean of the scale, 
###the item or subject). For you it would be a simple and basic possibility.  
###If you have incomplete data and its loss is not very large (less than 10%) 
###it seems that gets better population data is replacement method for the average 
###of the subject. It is based on a basic principle in psychology: 
###usually the best predictor of future behavior of a person is precisely the
###past behavior of that person.

mergulhadores$Acesso[is.na(mergulhadores$Acesso)] = signif(mean(mergulhadores$Acesso,na.rm=TRUE),digits=1);
mergulhadores$Regras[is.na(mergulhadores$Regras)] = signif(mean(mergulhadores$Regras,na.rm=TRUE),digits=1);
mergulhadores$Local[is.na(mergulhadores$Local)] = signif(mean(mergulhadores$Local,na.rm=TRUE),digits=1);
mergulhadores$Serviço[is.na(mergulhadores$Serviço)] = signif(mean(mergulhadores$Serviço,na.rm=TRUE),digits=1);
mergulhadores$Briefing[is.na(mergulhadores$Briefing)] = signif(mean(mergulhadores$Briefing,na.rm=TRUE),digits=1);
mergulhadores$Técnicas[is.na(mergulhadores$Técnicas)] = signif(mean(mergulhadores$Técnicas,na.rm=TRUE),digits=1);
mergulhadores$Aglomeração[is.na(mergulhadores$Aglomeração)] = signif(mean(mergulhadores$Aglomeração,na.rm=TRUE),digits=1);
mergulhadores$Lixo[is.na(mergulhadores$Lixo)] = signif(mean(mergulhadores$Lixo,na.rm=TRUE),digits=1);
mergulhadores$Pesca[is.na(mergulhadores$Pesca)] = signif(mean(mergulhadores$Pesca,na.rm=TRUE),digits=1);
mergulhadores$Visibilidade[is.na(mergulhadores$Visibilidade)] = signif(mean(mergulhadores$Visibilidade,na.rm=TRUE),digits=1);
mergulhadores$Diversidade[is.na(mergulhadores$Diversidade)] = signif(mean(mergulhadores$Diversidade,na.rm=TRUE),digits=1);
mergulhadores$Encontros[is.na(mergulhadores$Encontros)] = signif(mean(mergulhadores$Encontros,na.rm=TRUE),digits=1);
mergulhadores$Coral_sol[is.na(mergulhadores$Coral_sol)] = signif(mean(mergulhadores$Coral_sol,na.rm=TRUE),digits=1);
mergulhadores$Satisfação[is.na(mergulhadores$Satisfação)] = signif(mean(mergulhadores$Satisfação,na.rm=TRUE),digits=1);

install.packages("MVN")
library(MVN)
metv <- c(met)
#testando a normalidade multipla
result3 <- mvn(data = mergulhadores, subset = metv, mvnTest = "hz")


merg.mat<-sqrt(mergulhadores)#square root transform
merg.dist<-vegdist(merg.mat, method='gower')

#permanova
merg.div<-adonis2(merg.dist~ met, data=mergulhadores, permutations = 10000, method = 'gower', strata="PLOT")

mergMDS<-metaMDS(merg.mat, distance="gower", k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
mergMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(mergMDS)

library(ggplot2)

##pull points from MDS
NMDS1 <- mergMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- mergMDS$points[,2]
merg.plot<-cbind(mergulhadores, NMDS1, NMDS2, met)

#plot ordination
p<-ggplot(merg.plot, aes(NMDS1, NMDS2, color=met))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()




