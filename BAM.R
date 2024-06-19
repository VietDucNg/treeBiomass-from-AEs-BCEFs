#prepare workspace
getwd()
setwd("E:/OneDrive_HNEE/01_study/03_Master_FIT/02_semester_2/Biomass_assessment_and_modelling/exam/result")

data_tree <- read.csv(paste0(getwd(),"/data/Scots_pine_2_trees.csv"), sep=";")
data_plot <- read.csv(aste0(getwd(),"/data/Scots_pine_2_plots.csv"), sep=";")


library(systemfit)
library(ggplot2)
library(pastecs)
library(writexl)
library(gridExtra)

#data preparation
data_tree$D<-data_tree$DBH
data_tree$D2<-data_tree$D^2
data_tree$H<-data_tree$H_m
data_tree$H2<-data_tree$H^2
data_tree$D2H<-data_tree$D2*data_tree$H
data_tree$SDB<-data_tree$SDB_kg
data_tree$BDB<-data_tree$BDB_kg
data_tree$LDB<-data_tree$LDB_kg

data_tree$SMALL <- SDB+BDB+LDB

tree<-with(data_tree, data.frame(Plot,Tree,Site,Age_class,H,D,D2,H2,D2H,SDB,BDB,LDB))
attach(tree)


#data description
stat.desc(data_plot)
summary(data_plot)
stat.desc(data_tree)
summary(data_tree)

p1 <- ggplot(data = tree, aes(D)) +
  geom_histogram()+theme_classic()+labs(title = "(a)", x="DBH (mm)")
p2 <- ggplot(data = tree, aes(H)) +
  geom_histogram()+theme_classic()+labs(title = "(b)", x="H (m)")
p3 <-ggplot(data = tree, aes(D,H)) +
  geom_point()+theme_classic()+labs(title = "(c)", x="DBH (mm)", y="H (m)")
p4 <-ggplot(data = tree, aes(D,SDB)) +
  geom_point()+theme_classic()+labs(title = "(D)", x="DBH (mm)", y="SDB (kg)")
p5 <-ggplot(data = tree, aes(D,BDB)) +
  geom_point()+theme_classic()+labs(title = "(e)", x="DBH (mm)", y="BDB (kg)")
p6 <-ggplot(data = tree, aes(D,LDB)) +
  geom_point()+theme_classic()+labs(title = "(f)", x="DBH (mm)", y="LDB (kg)")
p7 <-ggplot(data = tree, aes(H,SDB)) +
  geom_point()+theme_classic()+labs(title = "(g)", x="H (m)", y="SDB (kg)")
p8 <-ggplot(data = tree, aes(H,BDB)) +
  geom_point()+theme_classic()+labs(title = "(h)", x="H (m)", y="BDB (kg)")
p9 <-ggplot(data = tree, aes(H,LDB)) +
  geom_point()+theme_classic()+labs(title = "(i)", x="H (m)", y="LDB (kg)")

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/02.method/3.tree_description.jpg",
     res=500,width=5000,height=5000)
tree_description <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol=3)
dev.off()

####################SUR#################################################

#regression modles for stem dry tree
Ms1 <- nls(SDB~b0+b1*D,start=list(b0=1,b1=1),data=tree)
Ms2 <- nls(SDB~b0+b1*D2,start=list(b0=1,b1=1),data=tree)
Ms3 <- nls(SDB~b0+b1*H,start=list(b0=1,b1=1),data=tree)
Ms4 <- nls(SDB~b0+b1*D+b2*H,start=list(b0=1,b1=1,b2=1),data=tree)
Ms5 <- nls(SDB~b0+b1*D2+b2*H2,start=list(b0=1,b1=1,b2=1),data=tree)
Ms6 <- nls(SDB~b0+b1*D2H,start=list(b0=1,b1=1),data=tree)
Ms7 <- nls(SDB~b0+b1*D2+b2*H+b3*D2H,start=list(b0=1,b1=1,b2=1,b3=1),data=tree)
Ms8 <- nls(SDB~b0*D^b1,start=list(b0=1,b1=1),control=list(maxiter=100),data=tree)
Ms9 <- nls(SDB~b0*D^b1*H^b2,start=list(b0=1,b1=2,b2=1),data=tree,algorithm="port",control=list(maxiter=100))
Ms10 <- nls(SDB~b0*D2H^b1,start=list(b0=1,b1=1),data=tree)

Ms1r2 <- 1-(sum({summary(Ms1)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms2r2 <- 1-(sum({summary(Ms2)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms3r2 <- 1-(sum({summary(Ms3)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms4r2 <- 1-(sum({summary(Ms4)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms5r2 <- 1-(sum({summary(Ms5)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms6r2 <- 1-(sum({summary(Ms6)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms7r2 <- 1-(sum({summary(Ms7)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms8r2 <- 1-(sum({summary(Ms8)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms9r2 <- 1-(sum({summary(Ms9)$residuals}^2)/sum({SDB-mean(SDB)}^2))
Ms10r2 <- 1-(sum({summary(Ms10)$residuals}^2)/sum({SDB-mean(SDB)}^2))

Mssrrse <- c(summary(Ms1)$sigma,summary(Ms2)$sigma,summary(Ms3)$sigma,summary(Ms4)$sigma,summary(Ms5)$sigma,summary(Ms6)$sigma,summary(Ms7)$sigma,summary(Ms8)$sigma,summary(Ms9)$sigma,summary(Ms10)$sigma)
Mssraic <- AIC(Ms1,Ms2,Ms3,Ms4,Ms5,Ms6,Ms7,Ms8,Ms9,Ms10)$AIC
Mssrr2 <- c(Ms1r2,Ms2r2,Ms3r2,Ms4r2,Ms5r2,Ms6r2,Ms7r2,Ms8r2,Ms9r2,Ms10r2)

Mssrrse
Mssraic
Mssrr2

summary(Ms9)

### Branches dry tree - BDB [kg] ###
Mb1 <- nls(BDB~b0+b1*D,start=list(b0=1,b1=1),data=tree)
Mb2 <- nls(BDB~b0+b1*D2,start=list(b0=1,b1=1),data=tree)
Mb3 <- nls(BDB~b0+b1*H,start=list(b0=1,b1=1),data=tree)
Mb4 <- nls(BDB~b0+b1*D+b2*H,start=list(b0=1,b1=1,b2=1),data=tree)
Mb5 <- nls(BDB~b0+b1*D2+b2*H2,start=list(b0=1,b1=1,b2=1),data=tree)
Mb6 <- nls(BDB~b0+b1*D2H,start=list(b0=1,b1=1),data=tree)
Mb7 <- nls(BDB~b0+b1*D2+b2*H+b3*D2H,start=list(b0=1,b1=1,b2=1,b3=1),data=tree)
Mb8 <- nls(BDB~b0*D^b1,start=list(b0=1,b1=1),control=list(maxiter=100),data=tree)
Mb9 <- nls(BDB~b0*D^b1*H^b2,start=list(b0=1,b1=2,b2=1),data=tree,algorithm="port",control=list(maxiter=100))
Mb10 <- nls(BDB~b0*D2H^b1,start=list(b0=1,b1=0),data=tree,control=list(maxiter=100))

Mb1r2 <- 1-(sum({summary(Mb1)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb2r2 <- 1-(sum({summary(Mb2)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb3r2 <- 1-(sum({summary(Mb3)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb4r2 <- 1-(sum({summary(Mb4)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb5r2 <- 1-(sum({summary(Mb5)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb6r2 <- 1-(sum({summary(Mb6)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb7r2 <- 1-(sum({summary(Mb7)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb8r2 <- 1-(sum({summary(Mb8)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb9r2 <- 1-(sum({summary(Mb9)$residuals}^2)/sum({BDB-mean(BDB)}^2))
Mb10r2 <- 1-(sum({summary(Mb10)$residuals}^2)/sum({BDB-mean(BDB)}^2))

Mbsrrse <- c(summary(Mb1)$sigma,summary(Mb2)$sigma,summary(Mb3)$sigma,summary(Mb4)$sigma,summary(Mb5)$sigma,summary(Mb6)$sigma,summary(Mb7)$sigma,summary(Mb9)$sigma,summary(Mb10)$sigma)
Mbsraic <- AIC(Mb1,Mb2,Mb3,Mb4,Mb5,Mb6,Mb7,Mb9,Mb10)$AIC
Mbsrr2 <- c(Mb1r2,Mb2r2,Mb3r2,Mb4r2,Mb5r2,Mb6r2,Mb7r2,Mb9r2,Mb10r2)

Mbsrrse
Mbsraic
Mbsrr2

summary(Mb9)

### Leaves dry tree - LDB [kg] ###
Ml1 <- nls(LDB~b0+b1*D,start=list(b0=1,b1=1),data=tree)
Ml2 <- nls(LDB~b0+b1*D2,start=list(b0=1,b1=1),data=tree)
Ml3 <- nls(LDB~b0+b1*H,start=list(b0=1,b1=1),data=tree)
Ml4 <- nls(LDB~b0+b1*D+b2*H,start=list(b0=1,b1=1,b2=1),data=tree)
Ml5 <- nls(LDB~b0+b1*D2+b2*H2,start=list(b0=1,b1=1,b2=1),data=tree)
Ml6 <- nls(LDB~b0+b1*D2H,start=list(b0=1,b1=1),data=tree)
Ml7 <- nls(LDB~b0+b1*D2+b2*H+b3*D2H,start=list(b0=1,b1=1,b2=1,b3=1),data=tree)
Ml8 <- nls(LDB~b0*D^b1,start=list(b0=1,b1=0),data=tree)
Ml9 <- nls(LDB~b0*D^b1*H^b2,start=list(b0=1,b1=2,b2=1),data=tree)
Ml10 <- nls(LDB~b0*D2H^b1,start=list(b0=1,b1=0),data=tree)

Ml1r2 <- 1-(sum({summary(Ml1)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml2r2 <- 1-(sum({summary(Ml2)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml3r2 <- 1-(sum({summary(Ml3)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml4r2 <- 1-(sum({summary(Ml4)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml5r2 <- 1-(sum({summary(Ml5)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml6r2 <- 1-(sum({summary(Ml6)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml7r2 <- 1-(sum({summary(Ml7)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml8r2 <- 1-(sum({summary(Ml8)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml9r2 <- 1-(sum({summary(Ml9)$residuals}^2)/sum({LDB-mean(LDB)}^2))
Ml10r2 <- 1-(sum({summary(Ml10)$residuals}^2)/sum({LDB-mean(LDB)}^2))

Mlsrrse <- c(summary(Ml1)$sigma,summary(Ml2)$sigma,summary(Ml3)$sigma,summary(Ml4)$sigma,summary(Ml5)$sigma,summary(Ml6)$sigma,summary(Ml7)$sigma,summary(Ml8)$sigma,summary(Ml9)$sigma,summary(Ml10)$sigma)
Mlsraic <- AIC(Ml1,Ml2,Ml3,Ml4,Ml5,Ml6,Ml7,Ml8,Ml9,Ml10)$AIC
Mlsrr2 <- c(Ml1r2,Ml2r2,Ml3r2,Ml4r2,Ml5r2,Ml6r2,Ml7r2,Ml8r2,Ml9r2,Ml10r2)

Mlsrrse
Mlsraic
Mlsrr2

summary(Ml5)

###########################   Seemingly Unrelated Regression - SUR    #################################

Ms.formula<- SDB~b0*D^b1*H^b2
Mb.formula<- BDB~b3*D^b4*H^b5
Ml.formula<- LDB~b6+b7*D2+b8*H2

equations <- list(Ms.formula,Mb.formula,Ml.formula)
start.values <- c(b0=0.0005357  ,b1=1.6941255  ,b2=1.2137352  
                  ,b3=5.215e-06,b4=3.609e+00,b5=-1.532e+00,
                  b6=1.224e+00, b7=2.484e-04,b8 = -9.576e-03)
labels <- list("SDB","BDB","LDB")

SUR<-nlsystemfit(method ="SUR",equations,start.values,data=tree,eqnlabels=labels)

print(SUR)
summary(SUR)
SUR$eq[3]
SUR$resids
SUR$nlmest

library(readxl)
ADB_predicted <- read_excel("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/ADB_predicted.xlsx")

#residuals plot
p10 <- ggplot(data = ADB_predicted, aes(ADB_predicted$pridicted, ADB_predicted$residuals))+
  geom_point()+theme_bw()+labs(title = "(a)", x="Predicted ADB (kg)", y="Residuals")

#ADB measured and predicted plot
write_xlsx(as.data.frame(data_tree$ADB_kg),"E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/data/ADB_compare.xlsx")
library(readxl)
ADB_compare <- read_excel("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/data/ADB_compare.xlsx")
library(ggpmisc)
p11 <- ggplot(data = ADB_compare, aes(ADB_compare$Measured,ADB_compare$predicted))+
  geom_point()+labs(title = "(b)",x="Observed ADB (kg)", y="Predicted ADB (kg)")+
  geom_smooth(method='lm',se = FALSE, color="black")+
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_bw()

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/04.ADB_compare.jpg",
     res=500,width=4000,height=2000)
grid.arrange(p10,p11,ncol=2)
dev.off()


###############################################################
###           BEF: Biomass Expansion Factors                ###
###############################################################
library(gridExtra)
library(lmtest)
library(dplyr)
library(tidyr)
library(grid)

plot <- with(data_plot, data.frame(Plot,Site, Tree.species, Age,BA_m2_ha,
                                   N_trees_ha,Dg_mm,Hg_m,V_m3_ha,SDB_Mg_ha,
                                   BDB_Mg_ha,LDB_Mg_ha,ADB_Mg_ha))
plot$age <- plot$Age
plot$BA <- plot$BA_m2_ha
plot$Dg <- plot$Dg_mm
plot$Hg <- plot$Hg_m
plot$V <- plot$V_m3_ha
plot$ADB <- plot$ADB_Mg_ha
plot$SDB <- plot$SDB_Mg_ha
plot$BDB <- plot$BDB_Mg_ha
plot$LDB <- plot$LDB_Mg_ha

#data discription

p12 <- ggplot(data = plot, aes(V,ADB))+
  geom_point()+ labs(title = "(a)", x ="V (m3)" , y="ADB (Mg_ha)")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()
p13 <- ggplot(data = plot, aes(V,SDB))+
  geom_point()+ labs(title = "(b)", x ="V (m3)" , y="SDB (Mg_ha)")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()
p14 <- ggplot(data = plot, aes(V,BDB))+
  geom_point()+ labs(title = "(c)", x ="V (m3)" , y="BDB (Mg_ha)")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()
p15 <- ggplot(data = plot, aes(V,LDB))+
  geom_point()+ labs(title = "(d)", x ="V (m3)" , y="LDB (Mg_ha)")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/05.V~bioComponent.jpg",
     res=500,width=3000,height=3000)
grid.arrange(p12,p13,p14,p15,ncol=2)
dev.off()

#biomass component ~ stand characteristics
###### stem dry biomass ######

plot$BEF_SDB<-plot$SDB/plot$V

p16 <- ggplot(data = plot, aes(BEF_SDB,age))+
  geom_point()+ labs(title = "SDB",x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p17 <- ggplot(data = plot, aes(BEF_SDB,BA))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p18 <- ggplot(data = plot, aes(BEF_SDB,Dg))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p19 <- ggplot(data = plot, aes(BEF_SDB, Hg))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p20 <- ggplot(data = plot, aes(BEF_SDB, V))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()
  
###### branch dry data_plot ######
plot$BEF_BDB <- plot$BDB/plot$V

p21 <- ggplot(data = plot, aes(BEF_BDB,age))+
  geom_point()+ labs(title = "BDB",x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),)

p22 <- ggplot(data = plot, aes(BEF_BDB,BA))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),)

p23 <- ggplot(data = plot, aes(BEF_BDB,Dg))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p24 <- ggplot(data = plot, aes(BEF_BDB, Hg))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p25 <- ggplot(data = plot, aes(BEF_BDB, V))+
  geom_point()+ labs(x="",y="")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

###### leaves dry data_plot ######
plot$BEF_LDB<-plot$LDB/plot$V 

p26 <- ggplot(data = plot, aes(BEF_LDB,age))+
  geom_point()+ labs(title = "LDB",x="",y="Age")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position = "right")

p27 <- ggplot(data = plot, aes(BEF_LDB,BA))+
  geom_point()+ labs(x="",y="BA")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position = "right")

p28 <- ggplot(data = plot, aes(BEF_LDB,Dg))+
  geom_point()+ labs(x="",y="Dg")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position = "right")

p29 <- ggplot(data = plot, aes(BEF_LDB, Hg))+
  geom_point()+ labs(x="",y="Hg")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position = "right")

p30 <- ggplot(data = plot, aes(BEF_LDB, V))+
  geom_point()+ labs(x="",y="V")+
  geom_smooth(method = "lm",se = FALSE, color = "black")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position = "right")

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/06.StandCharacteristics~bioComponent.jpg",
     res=800,width=6000,height=8000)
grid.arrange(p16,p21,p26,p17,p22,p27,p18,p23,p28,p19,p24,p29,p20,p25,p30,
             ncol=3, left = textGrob("BEF",rot = 90, gp=gpar(fontsize=16)))
dev.off()



###### nls models for BEF_SDB ######
summary(plot$BEF_SDB)

# age #
BEF_SDB<-plot$BEF_SDB
x<-plot$age

library(minpack.lm)
M1<-nls(BEF_SDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_SDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_SDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_SDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_SDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# BA #
BEF_SDB<-plot$BEF_SDB
x<-plot$BA

M1<-nls(BEF_SDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_SDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_SDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_SDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_SDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# Dg #
BEF_SDB<-plot$BEF_SDB
x<-plot$Dg

M1<-nls(BEF_SDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_SDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_SDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_SDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_SDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

summary(M4)

# Hg #
BEF_SDB<-plot$BEF_SDB
x<-plot$Hg

M1<-nls(BEF_SDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_SDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_SDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_SDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_SDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse
summary(M1)

# V #
BEF_SDB<-plot$BEF_SDB
x<-plot$V

M1<-nls(BEF_SDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_SDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_SDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_SDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_SDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_SDB-mean(BEF_SDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse



###### nls models for BEF_BDB ######
# age #
BEF_BDB<-plot$BEF_BDB
x<-plot$age

M1<-nls(BEF_BDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_BDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_BDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_BDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_BDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# BA #
BEF_BDB<-plot$BEF_BDB
x<-plot$BA

M1<-nls(BEF_BDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_BDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_BDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_BDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_LDB~a+b*exp(-x*c),start=list(a=0.03,b=0.7,c=0.5))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# Dg #
BEF_BDB<-plot$BEF_BDB
x<-plot$Dg

M1<-nls(BEF_BDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_BDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_BDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_BDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_BDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# Hg #
BEF_BDB<-plot$BEF_BDB
x<-plot$Hg

M1<-nls(BEF_BDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_BDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_BDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_BDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_BDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse
summary(M1)

# V #
BEF_BDB<-plot$BEF_BDB
x<-plot$V

M1<-nls(BEF_BDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_BDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_BDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_BDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_BDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_BDB-mean(BEF_BDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse


###### nls models for BEF_LDB ######
# age #
BEF_LDB<-plot$BEF_LDB
x<-plot$age

M1<-nls(BEF_LDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_LDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_LDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_LDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_LDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# BA #
BEF_LDB<-plot$BEF_LDB
x<-plot$BA

M1<-nls(BEF_LDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_LDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_LDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_LDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_LDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

# Dg #
BEF_LDB<-plot$BEF_LDB
x<-plot$Dg

M1<-nls(BEF_LDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_LDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_LDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_LDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nls(BEF_LDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

summary(M4)

# Hg #
BEF_LDB<-plot$BEF_LDB
x<-plot$Hg

M1<-nls(BEF_LDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_LDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_LDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_LDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_LDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse


# V #
BEF_LDB<-plot$BEF_LDB
x<-plot$V

M1<-nls(BEF_LDB~a*x^b,start=list(a=1,b=1))
M2<-nls(BEF_LDB~a+b*log(x),start=list(a=1,b=1))
M3<-nls(BEF_LDB~a+b/x,start=list(a=1,b=1))
M4<-nls(BEF_LDB~exp(a+b/x),start=list(a=1,b=1))
M5<-nlsLM(BEF_LDB~a+b*exp(-x*c),start=list(a=1,b=1,c=1))

### diagnose ###

M1_r2 <- 1-(sum({summary(M1)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M2_r2 <- 1-(sum({summary(M2)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M3_r2 <- 1-(sum({summary(M3)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M4_r2 <- 1-(sum({summary(M4)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))
M5_r2 <- 1-(sum({summary(M5)$residuals}^2)/sum({BEF_LDB-mean(BEF_LDB)}^2))

R2<-c(M1_r2,M2_r2,M3_r2,M4_r2,M5_r2)
rse<- c(summary(M1)$sigma,summary(M2)$sigma,summary(M3)$sigma,summary(M4)$sigma,summary(M5)$sigma)
R2
rse

###########################   Seemingly Unrelated Regression - SUR    #################################
########################### For BEF###################################
Ms.formula<- BEF_SDB~b0*Hg^b1
Mb.formula<- BEF_BDB~b2*Hg^b3
Ml.formula<- BEF_LDB~exp(b4+b5/Dg)

equations <- list(Ms.formula,Mb.formula,Ml.formula)
start.values <- c(b0=0.99532  ,b1=-0.26835  ,b2=0.119390
                  ,b3=-0.338020,b4=-4.351000,b5=150.518260)
labels <- list("BEF_SDB","BEF_BDB","BEF_LDB")

SUR<-nlsystemfit(method ="SUR",equations,start.values,data=plot,eqnlabels=labels)

print(SUR)
summary(SUR)
SUR$eq[3]
SUR$resids
SUR$nlmest

BEF_predicted <- read_excel("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/data/BEF_predicted.xlsx")

#residuals plot
p31 <- ggplot(data = BEF_predicted, aes(BEF_predicted$SDB_pre, BEF_predicted$SDB_re))+
  geom_point()+theme_bw()+labs(title = "SDB", x="Predicted BEF", y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))
p32 <- ggplot(data = BEF_predicted, aes(BEF_predicted$BDB_pre, BEF_predicted$BDB_re))+
  geom_point()+theme_bw()+labs(title = "BDB", x="Predicted BEF", y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))
p33 <- ggplot(data = BEF_predicted, aes(BEF_predicted$LDB_pre, BEF_predicted$LDB_re))+
  geom_point()+theme_bw()+labs(title = "LDB", x="Predicted BEF", y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))
p34 <- ggplot(data = BEF_predicted, aes(BEF_predicted$ADB_pre, BEF_predicted$ADB_re))+
  geom_point()+theme_bw()+labs(title = "ADB", x="Predicted BEF", y="Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/09.BEF_residuals.jpg",
     res=500,width=4000,height=3000)
grid.arrange(p31,p32,p33,p34,ncol=2)
dev.off()

#ADB measured and predicted plot

write_xlsx(as.data.frame(plot$V), "E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/data/BEF_ADB_compare.xlsx")

BEF_ADB_compare <- read_excel("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/data/BEF_ADB_compare.xlsx")

p35 <- ggplot(data = BEF_ADB_compare, aes(BEF_ADB_compare$ADB,BEF_ADB_compare$ADB_predicted))+
  geom_point()+labs(x="Observed ADB (kg)", y="Predicted ADB (kg)")+
  geom_smooth(method='lm',se = FALSE, color="black")+
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_bw()

jpeg("E:/Google Drive_University/M.Sc_Environment_and_Sustainable_development/Eberswalde_exchange_program/Semester_2/Biomass_assessment_and_modelling/exam/result/03.result/10.BEF_ADB_compare.jpg",
     res=500,width=4000,height=2000)
p35
dev.off()

