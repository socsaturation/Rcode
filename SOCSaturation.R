########### Data###########
library(ggplot2)
library(dplyr)
library(broom)
library(lmtest)
library(mgcv)
library(nlstools)
library(MuMIn)

mt <- read.delim("Manure_treatments_analyse.txt", header = T)
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }
library(ggplot2)
library(dplyr)
library(reshape)
mt$Cinputy <- mt$manureC.input..Mg..Ha.y. + 0.000001
mt$cum.input <- mt$duration..y. * mt$Cinputy
mt$veraen <- (mt$SoilOC.END... - mt$SoilOCBegin) * 10

mt$veraenprotC <- mt$veraen / mt$cum.input
mt$veraenprotCprocm<-mt$veraenprotC/mt$Depth

mt$jahrveraen <- mt$veraen / mt$duration..y.




############ Sites separate #########
Lethbridge015 <- subset(mt, Name == "Lethbridge CA")
Lethbridge015irrigated <- subset(mt, Name == "Lethbridge CA irr.")
Coffeeville <- subset(mt, Name == "Coffeeville USA")
Cruger <- subset(mt, Name == "Cruger USA")
Clarksville <- subset(mt, Name == "Clarksville USA")
darmstadt <- subset(mt, Name == "Darmstadt DE")
Gongzhuling <- subset(mt, Name == "Gongzhuling CN")
Iasi <- subset(mt, Name == "Iasi ROU")
Hawalbaghcattlecompost <- subset(mt, Name == "Hawal IN cattle1 ")
HawalbaghVermicompost <- subset(mt, Name == "Hawal IN vermi")
Michigangrain <- subset(mt, Name == "Michig. USA corn1")
Michigansilage <- subset(mt, Name == "Michig. USA corn2")
NIpig <- subset(mt, Name == "IRL pig slurry")
NIcattle <- subset(mt, Name == "IRL Cattle slurry")
GroßKreutz <- subset(mt, Name == "Groß Kreutz DE")
Darmstadt <- subset(mt, Name == "Darmstadt DE")
Aiello <- subset(mt, Name == "Aiello  IT")
Melle <- subset(mt, Name == "Melle BE")
Moystad <- subset(mt, Name == "Moystad SE")
BLStallmist <- subset(mt, Name == "BL FYM DE")
Dixoncattle <- subset(mt, Name == "Dixon USA cattle")
Dixonswine <- subset(mt, Name == "Dixon USA swine")
Laiyang <- subset(mt, Name == "Laiyang CN")
Melfort <- subset(mt, Name == "Melfort CA")
Moody <- subset(mt, Name == "Moody County USA")
Offer <- subset(mt, Name == "Offer SE")
Qiyang <- subset(mt, Name == "Qiyang CN")
Shenyang <- subset(mt, Name == "Shenyang CN")
Robacksdalen <- subset(mt, Name == "Robacksdalen SE")
Texasgrass <- subset(mt, Name == "Texas USA grass")
TurinSlurry <- subset(mt, Name == "Turin IT Slurry")
TurinFYM <- subset(mt, Name == "Turin IT FYM")
WoburnFYM <- subset(mt, Name == "Wob. UK FYM")
WoburnOM <- subset(mt, Name == "Wob. UK Organic")
WoburnSS <- subset(mt, Name == "Wob. UK sludge")
WoburnSC <- subset(mt, Name == "Wob. UK sl. comp.")
WoburnVC <- subset(mt, Name == "Wob. UK veg.comp.")
Zhengzhou <- subset(mt, Name == "Zhengzhou CN")
Agassiz <- subset(mt, Name == "Agassiz CA")
As <- subset(mt, Name == "As SE")
BLstatic <- subset(mt, Name == "BL static DE")
Urumqi <- subset(mt, Name == "Urumqi CN")
Changping <- subset(mt, Name == "Changping CN")
Anhui <- subset(mt, Name == "Anhui CN")
Murcia <- subset(mt, Name == "Murcia ES")
Pingyi <- subset(mt, Name == "Pingyi CN")
WashingtonWheat <- subset(mt, Name == "Washington USA")
Pamplona <- subset(mt, Name == "Pamplona ES")
GongzhulingPig <- subset(mt, Name == "Gongzhuling CN pig")
Lobau <- subset(mt, Name == "Obere Lobau AT")
HawalbaghPoultry <- subset(mt, Name == "Hawal IN poultry")
HawalbaghVermi2 <- subset(mt, Name == "Hawal IN vermi2")
HawalbaghCattle2 <- subset(mt, Name == "Hawal IN cattle2")
HawalbaghFYM <- subset(mt, Name == "Hawal IN FYM")
Virginia <- subset(mt, Name == "Virginia USA")
Hailun <- subset(mt, Name == "Hailun CN")
Keszthely <- subset(mt, Name == "Keszthely HU")
Kabete <- subset(mt, Name == "Kabete KE")
Embu <- subset(mt, Name == "Embu KE")
Faisalabad <- subset(mt, Name == "Faisalabad PAK")
UtahCompost <- subset(mt, Name == "Utah USA compost")
BonnFYM <- subset(mt, Name == "Bonn FYM DE")
BonnCompost <- subset(mt, Name == "Bonn Compost DE")
BonnSludge <- subset(mt, Name == "Bonn sludge DE")
Changins <- subset(mt, Name == "Changins CH")
UtahLiquid <- subset(mt, Name == "Utah USA liquid")
Martonvasar <- subset(mt, Name == "Martonvasar HU")

###### Figure 2 #########
mth<-subset(mt, CinputLevel==0 |CinputLevel=="h")#high input level
mtl<-subset(mt,CinputLevel==0 |CinputLevel=="l")#low input level
mtm<-subset(mt,CinputLevel==0 |CinputLevel=="m")#medium input level
#high level 
dfLin2 = mth %>% group_by(Name) %>%
  do(fit = lm(SoilOC.END...*10 ~ Cinputy, data=.))

dfLinCoef2 = tidy(dfLin2, fit)

#low level
dfLin3 = mtl %>% group_by(Name) %>%
  do(fit = lm(SoilOC.END...*10 ~ Cinputy, data=.))

dfLinCoef3 = tidy(dfLin3, fit)

#medium level

dfLin4 = mtm %>% group_by(Name) %>%
  do(fit = lm(SoilOC.END...*10 ~ Cinputy, data=.))

dfLinCoef4 = tidy(dfLin4, fit)


MeanSlopehigh<-subset(dfLinCoef2,term=="Cinputy")
MeanSlopehigh$slope<-MeanSlopehigh$estimate
MeanSlopehigh$estimate<-NULL
MeanSlopehigh$condition<-"high"

MeanSlopelow<-subset(dfLinCoef3,term=="Cinputy")
MeanSlopelow$slope<-MeanSlopelow$estimate
MeanSlopelow$estimate<-NULL
MeanSlopelow$condition<-"low"

MeanSlopemed<-subset(dfLinCoef4,term=="Cinputy")
MeanSlopemed$slope<-MeanSlopemed$estimate
MeanSlopemed$estimate<-NULL
MeanSlopemed$condition<-"medium"

slopes2<-rbind(MeanSlopelow,MeanSlopemed, MeanSlopehigh)


slopes2$condition <- factor(slopes2$condition, levels = c("low", "medium", "high"))
bp0<-ggplot(data=slopes2, aes(condition, slope))+geom_boxplot()+coord_cartesian(ylim=c(-0.3,5))+theme_classic()+
  theme(axis.text=element_text(size=12, face="bold"),axis.title=element_text(size=16,face="bold"))+
  ylab(expression('C sequestration per C input'*'  (g kg'^{'-1'}*'Mg C'^{'-1'}*')'))+xlab("Relative levels of manure C inputs")



######## Figure 3 ##########
lines_gamlinall<-ggplot(data=mt,aes(Cinputy,SoilOC.END...*10))+geom_point()+
  facet_wrap(~Name, scales="free")+theme_classic()+
  geom_smooth(method="gam", formula = y ~ s(x, k=3,bs="cr"), se=F, aes(color="red"))+
  geom_smooth(method="lm", se=F,lty="dashed")+theme(legend.position = "none") +
  labs(y="SOC at end of experiment (g/kg)",x="Manure C input (Mg C/ha year)")

###### Figure S1 ######
geo = read.delim("O:/Paper saturation/R/Daten/koordinaten.txt")
str(geo)
ggplot() +
  borders("world", colour=FALSE, fill="grey50") +
  geom_point(aes(x=E, y=N), data = geo, color="red") +
  labs(x="Longitude", y = "Latitude") +
  theme_bw(base_size = 8) +
  scale_size_continuous(range = c(.5,10)) +
  theme(panel.border = element_rect(colour="black",size = 0.2), 
        axis.ticks = element_line(colour="black",size = 0.2), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position="none")

ggsave("0_Fig_map.png", width=12, height=10, units = "cm", dpi=1000)

######### Figure S3 #########
lines_Lethbridge<-ggplot(data=Lethbridge015irrigated,aes(manureC.input..Mg..Ha.y.,SoilOC.END...*10))+
  geom_point(size=3)+theme_classic()+
  geom_segment(aes(x=0,xend=7.33,y=19,yend=36.5, color="green"))+
  geom_segment(aes(x=0,xend=14.77,y=19,yend=56.5, color="blue", linetype="dashed"))+
  geom_segment(aes(x=0,xend=22.15,y=19,yend=72.5, color="black", linetype="dotted"))+
  theme(axis.text=element_text(size=14, face="bold"),
        axis.title=element_text(size=14,face="bold"))+
  labs(y="SOC at end of experiment (g/kg)",x="Manure C input (Mg C/ha year)")

######## Figure S5 #########
#### Saturation Feng et al 2013 boundary line analysis

mthigh<-subset(mt,CinputLevel=='h')
mttreat<-subset(mt,CinputLevel!=0)
mthigh2<-subset(mthigh,!is.na(finepart))
mthigh2$SOC_Feng<-ifelse(mthigh2$Landuse=="cropland",mthigh2$SoilOC.END...*10*0.9,mthigh2$SoilOC.END...*10*0.8)

mthigh2$Feng_pred70<-ifelse(mthigh2$Landuse=="cropland", 0.45*mthigh2$finepart*0.7, 0.89*mthigh2$finepart*0.7)
mthigh2$Feng_sat70<- ifelse(mthigh2$Feng_pred70<=mthigh2$SOC_Feng,TRUE,FALSE)
mean(mthigh2$Feng_sat70)
##########Fig S5 ###### 
plotall<-ggplot(data=mthigh2, aes(finepart, SOC_Feng))+
  geom_point(aes(color=Landuse),size=2)+
  scale_color_manual(values=c("orange", "green", "blue", "red"))+
  #geom_abline(intercept=0, slope=0.78)+
  geom_abline(intercept=0, slope=0.45,color="orange")+
  geom_abline(intercept=0, slope=0.89,color="green")+
  theme_classic()+
  xlab("Mass proportion of fine soil particles (%)")+
  ylab("Organic C content (g/kg)")

plotall
######## Figure S6 ############
####### Figure S7 #######
mtcontrol<-subset(mt,CinputLevel==0)
mtcontrol<-mtcontrol[,c("Name","veraen")]
mtcontrol$VeraenControl<-mtcontrol$veraen
mtcontrol$veraen<-NULL

mttreat<-subset(mt,CinputLevel!=0)
mttreat<-merge(mttreat,mtcontrol, all.x=T)
mttreat$veraenDiffcontrol<-mttreat$veraen-mttreat$VeraenControl
mttreat$NormalizedVeraen<-mttreat$veraenDiffcontrol/mttreat$Depth
mttreat$NormalizedVeraenprotC<-mttreat$NormalizedVeraen/mttreat$cum.input
df<-mttreat
temps<-qplot(df$temp,df$NormalizedVeraenprotC)+theme_classic()+xlab("Temperature (°C)")+ylab("Normalized Annual C Sequestration")
precs<-qplot(df$Prec,df$NormalizedVeraenprotC)+theme_classic()+xlab("Precipitation (mm)")+ylab("")
clays<-qplot(df$clay,df$NormalizedVeraenprotC)+theme_classic()+xlab("Clay content(%)")+ylab("")
silts<-qplot(df$silt,df$NormalizedVeraenprotC)+theme_classic()+xlab("Silt content (%)")+ylab("")
sands<-qplot(df$sand,df$NormalizedVeraenprotC)+theme_classic()+xlab("Sand content (%)")+ylab("")
cgehalts<-qplot(df$SoilOCBegin*10,df$NormalizedVeraenprotC)+theme_classic()+xlab("Original SOC (g/kg)")+ylab("")
landuses<-qplot(df$Landuse,df$NormalizedVeraenprotC)+geom_boxplot()+theme_classic()+xlab("Land-use")+ylab("")
times<-qplot(df$duration..y.,df$NormalizedVeraenprotC)+theme_classic()+xlab("Experiment duration (y)")+ylab("")
mists<-qplot(df$Input2, df$NormalizedVeraenprotC)+theme_classic()+geom_boxplot()+xlab("Type amendendment")+ylab("")+scale_x_discrete(labels = abbreviate)
multiplot(temps,precs,clays,silts, sands, cgehalts,landuses,times,mists, cols=5)

###### Figure S8 #######
mtertrag<-subset(mt,!is.na(mt$ERTRAG))
mtertrag$CinputLevel<- factor(mtertrag$CinputLevel, levels = c("0", "l", "m", "h"))
levels(mtertrag$CinputLevel)[levels(mtertrag$CinputLevel)=="0"] <- "Control"
levels(mtertrag$CinputLevel)[levels(mtertrag$CinputLevel)=="l"] <- "Low"
levels(mtertrag$CinputLevel)[levels(mtertrag$CinputLevel)=="m"] <- "Medium"
levels(mtertrag$CinputLevel)[levels(mtertrag$CinputLevel)=="h"] <- "High"

ertrag_free_fit<-ggplot(data=mtertrag,aes(CinputLevel,ERTRAG))+geom_point()+
  facet_wrap(~Name, scales="free")+theme_classic()+
  
  labs(y="Yield (Mg/ha)",x="Treatment")

####### Linear Models ########
# NO SATURATION#

dfLin = mt %>% group_by(Name) %>%
  do(fit = lm(SoilOC.END...*10 ~ Cinputy, data=.))

dfLinCoef = tidy(dfLin, fit)

dfLinPred = augment(dfLin, fit)

dfLinSumm = glance(dfLin, fit)

MeanSlope<-subset(dfLinCoef,term=="Cinputy")


######### gam models #######
# using cubic regression splines
ma<-subset(mt,!is.na(veraen))

dfgam = ma %>% group_by(Name) %>%
  do(fit = gam(SoilOC.END...*10 ~ s(Cinputy,k=3, bs="cr"), data=.))

dfgamCoef = tidy(dfgam, fit)

dfgamPred = augment(dfgam, fit)

dfgamSumm = glance(dfgam, fit)


######### asymptotic models ###########
# Lethbridge 015 
fitasym<-nls(SoilOC.END... ~ SSasymp(Cinputy, A, lrc, c0), data = Lethbridge015)
fitasymOrig<-nls(SoilOC.END...*10 ~ SSasympOrig(Cinputy, A, lrc), data = Lethbridge015)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Lethbridge015)
resettest(fitlm, power=2, type="regressor", data=Lethbridge015)
resettest(fitlm, power=3, type="regressor", data=Lethbridge015)
plot(SoilOC.END...*10 ~ Cinputy, data=Lethbridge015)
lines(Lethbridge015$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Lethbridge015$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(Lethbridge015$Cinputy, fitted(fitasymOrig), lty = 2, col = "green")
lines(Lethbridge015$Cinputy, fitted(fitlm), lty = 2, col = "black")

AIC(fitasym,fitasymOrig, fitlm,fitasymOff)

# Lethbridge 015irrigated 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Lethbridge015irrigated)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Lethbridge015irrigated)
resettest(fitlm, power=2, type="regressor", data=Lethbridge015irrigated)
resettest(fitlm, power=3, type="regressor", data=Lethbridge015irrigated)

plot(SoilOC.END...*10 ~ Cinputy, Lethbridge015irrigated,xlim=c(0,30), ylim=c(0,90), xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Lethbridge015irrigated$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Lethbridge015irrigated$Cinputy, fitted(fitlm), lty = 2, col = "black")

AIC(fitasym, fitlm)

#Aiello 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Aiello)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Aiello)

resettest(fitlm, power=2, type="regressor", data=Aiello)
resettest(fitlm, power=3, type="regressor", data=Aiello)
plot(SoilOC.END...*10 ~ Cinputy, Aiello)
lines(Aiello$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Aiello$Cinputy, fitted(fitlm), lty = 2, col = "black")

AIC(fitasym, fitlm)

# BLStallmist 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=BLStallmist)
resettest(fitlm, power=2, type="regressor", data=BLStallmist)
resettest(fitlm, power=3, type="regressor", data=BLStallmist)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=BLStallmist)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = BLStallmist))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = BLStallmist)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = BLStallmist)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = BLStallmist)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = BLStallmist)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = BLStallmist)

plot(SoilOC.END...*10 ~ Cinputy, BLStallmist)
lines(BLStallmist$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(BLStallmist$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(BLStallmist$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(BLStallmist$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(BLStallmist$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(fitlm,powerfit, m.m, fitlogis, fitgompertz)

# Coffeeville 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Coffeeville)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = Coffeeville)
fitasymOrig<-nls(SOCEndOrigin*10 ~ SSasympOrig(Cinputy, A, lrc), data = Coffeeville)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Coffeeville)

resettest(fitlm, power=2, type="regressor", data=Coffeeville)
resettest(fitlm, power=3, type="regressor", data=Coffeeville)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Coffeeville)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Coffeeville))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Coffeeville)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Coffeeville)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Coffeeville)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = Coffeeville)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = Coffeeville)

plot(SoilOC.END...*10 ~ Cinputy, Coffeeville, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Coffeeville$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Coffeeville$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(Coffeeville$Cinputy, fitted(fitasymOrig), lty = 2, col = "green")
lines(Coffeeville$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Coffeeville$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Coffeeville$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(Coffeeville$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(Coffeeville$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)
lines(Coffeeville$Cinputy, fitted(fit2nls), lty = 2, col = "blue", lwd=4)
lines(Coffeeville$Cinputy, fitted(fit1nls), lty = 2, col = "blue", lwd=4)
AIC(fitasymOrig, fitlm,powerfit, m.m, fitlogis, fitgompertz)

#Cruger 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Cruger)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = Cruger)
fitasymOrig<-nls(SOCEndOrigin*10 ~ SSasympOrig(Cinputy, A, lrc), data = Cruger)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Cruger)

resettest(fitlm, power=2, type="regressor", data=Cruger)
resettest(fitlm, power=3, type="regressor", data=Cruger)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Cruger)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Cruger))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Cruger)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Cruger)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Cruger)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = Cruger)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = Cruger)


plot(SoilOC.END...*10 ~ Cinputy, Cruger, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Cruger$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Cruger$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(Cruger$Cinputy, fitted(fitasymOrig), lty = 2, col = "green")
lines(Cruger$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Cruger$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Cruger$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(Cruger$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(Cruger$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(fitasym,fitasymOrig, fitlm,powerfit, m.m, fitlogis, fitgompertz)

#Darmstadt 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Darmstadt)

resettest(fitlm, power=2, type="regressor", data=Darmstadt)
resettest(fitlm, power=3, type="regressor", data=Darmstadt)

print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Darmstadt))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Darmstadt)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Darmstadt)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Darmstadt)

plot(SoilOC.END...*10 ~ Cinputy, Darmstadt, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Darmstadt$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Darmstadt$Cinputy, fitted(m.m), lty = 2, col = "orange")

AIC( fitlm, m.m)

#Dixoncattle 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Dixoncattle)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = Dixoncattle)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Dixoncattle)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Dixoncattle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Dixoncattle)


powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Dixoncattle)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixoncattle))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixoncattle)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixoncattle)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Dixoncattle)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = Dixoncattle)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = Dixoncattle)


plot(SoilOC.END...*10 ~ Cinputy, Dixoncattle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Dixoncattle$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Dixoncattle$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(Dixoncattle$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Dixoncattle$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Dixoncattle$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(Dixoncattle$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(Dixoncattle$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(fitasym, fitlm,powerfit, m.m, fitlogis, fitgompertz)

# Dixonswine 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Dixonswine)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Dixonswine)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Dixonswine)

print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixonswine))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixonswine)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Dixonswine)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Dixonswine)

plot(SoilOC.END...*10 ~ Cinputy, Dixonswine, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Dixonswine$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Dixonswine$Cinputy, fitted(m.m), lty = 2, col = "orange")

AIC(fitlm, m.m)

#GroßKreutz 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=GroßKreutz)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=GroßKreutz)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=GroßKreutz)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=GroßKreutz)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = GroßKreutz))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = GroßKreutz)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = GroßKreutz)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = GroßKreutz)

plot(SoilOC.END...*10 ~ Cinputy, GroßKreutz, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(GroßKreutz$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(GroßKreutz$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(GroßKreutz$Cinputy, fitted(m.m), lty = 2, col = "orange")

AIC( fitlm,powerfit, m.m)

#Halwabaghcattlecompost 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Hawalbaghcattlecompost)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Hawalbaghcattlecompost)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Hawalbaghcattlecompost)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Hawalbaghcattlecompost)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Hawalbaghcattlecompost))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Hawalbaghcattlecompost)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Hawalbaghcattlecompost)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Hawalbaghcattlecompost)

plot(SoilOC.END...*10 ~ Cinputy, Hawalbaghcattlecompost, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Hawalbaghcattlecompost$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Hawalbaghcattlecompost$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Hawalbaghcattlecompost$Cinputy, fitted(m.m), lty = 2, col = "orange")

AIC( fitlm,powerfit, m.m)
#HalwabaghVermicompost 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = HawalbaghVermicompost)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = HawalbaghVermicompost)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=HawalbaghVermicompost)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=HawalbaghVermicompost)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=HawalbaghVermicompost)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=HawalbaghVermicompost)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = HawalbaghVermicompost))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = HawalbaghVermicompost)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = HawalbaghVermicompost)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = HawalbaghVermicompost)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = HawalbaghVermicompost)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = HawalbaghVermicompost)


plot(SoilOC.END...*10 ~ Cinputy, HawalbaghVermicompost, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(HawalbaghVermicompost$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(HawalbaghVermicompost$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(HawalbaghVermicompost$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(HawalbaghVermicompost$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(HawalbaghVermicompost$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(HawalbaghVermicompost$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(HawalbaghVermicompost$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(fitasym, fitlm,powerfit, m.m, fitlogis, fitgompertz)


# Melle 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Melle)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Melle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Melle)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Melle)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Melle))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Melle)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Melle)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Melle)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = Melle)

plot(SoilOC.END...*10 ~ Cinputy, Melle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Melle$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Melle$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Melle$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(Melle$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)

AIC( fitlm,powerfit, m.m, fitlogis)

# Michigangrain 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Michigangrain)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = Michigangrain)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Michigangrain)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Michigangrain)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Michigangrain)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Michigangrain)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigangrain))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigangrain)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigangrain)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Michigangrain)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = Michigangrain)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = Michigangrain)


plot(SoilOC.END...*10 ~ Cinputy, Michigangrain, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Michigangrain$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Michigangrain$Cinputy, fitted(fitasymOff), lty = 2, col = "blue")
lines(Michigangrain$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Michigangrain$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Michigangrain$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(Michigangrain$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(Michigangrain$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(fitasym, fitlm,powerfit, m.m, fitlogis, fitgompertz)

# Michigansilage 
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Michigansilage)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Michigansilage)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Michigansilage)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Michigansilage)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigansilage))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigansilage)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Michigansilage)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Michigansilage)

plot(SoilOC.END...*10 ~ Cinputy, Michigansilage, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Michigansilage$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Michigansilage$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Michigansilage$Cinputy, fitted(m.m), lty = 2, col = "orange")


AIC(fitasymOrig, fitlm,powerfit, m.m)

#Moystad 

fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Moystad)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Moystad)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Moystad)

powerfit <- nls(SoilOC.END...*10 ~ a + b * I(Cinputy^z), start = list(a = 1, b = 1, z = 1), data=Moystad)
print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Moystad))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Moystad)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = Moystad)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = Moystad)


plot(SoilOC.END...*10 ~ Cinputy, Moystad, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Moystad$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Moystad$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
lines(Moystad$Cinputy, fitted(m.m), lty = 2, col = "orange")


AIC( fitlm,powerfit, m.m)

# NIcattle 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = NIcattle)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = NIcattle)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=NIcattle)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=NIcattle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=NIcattle)

print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIcattle))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIcattle)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIcattle)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = NIcattle)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = NIcattle)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = NIcattle)


plot(SoilOC.END...*10 ~ Cinputy, NIcattle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(NIcattle$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(NIcattle$Cinputy, fitted(m.m), lty = 2, col = "orange")
lines(NIcattle$Cinputy, fitted(fitlogis), lty = 2, col = "pink", lwd=4)
lines(NIcattle$Cinputy, fitted(fitgompertz), lty = 2, col = "brown", lwd=4)

AIC(quad, fitlm, m.m, fitlogis, fitgompertz)

# NI pig 
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = NIpig)
fitasymOff<-nls(SoilOC.END...*10 ~ SSasympOff(Cinputy, A, lrc, c0), data = NIpig)

fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=NIpig)

resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=NIpig)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=NIpig)

print(getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIpig))
Vm <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIpig)[1]
K <- getInitial(SoilOC.END...*10 ~ SSmicmen(Cinputy, max(SoilOC.END...*10), 1), data = NIpig)[2]
m.m <- nls(SoilOC.END...*10 ~ SSmicmen(Cinputy, Vm, K), data = NIpig)
fitlogis<-nls(SoilOC.END...*10 ~ SSlogis(Cinputy, Asym, xmid, scal), data = NIpig)
fitgompertz<-nls(SoilOC.END...*10 ~ SSgompertz(Cinputy, Asym, b2, b3), data = NIpig)

AIC( fitlm, m.m, fitlogis, fitgompertz)


