

########### Data###########

mt<-read.delim("Manure treatments_red.txt", header=T)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
library(ggplot2)
library(dplyr)
library(reshape)

mt$cum.input<-mt$duration..y.*mt$manureC.input..Mg..Ha.y.
mt$veraen<-(mt$SoilOC.END...-mt$SoilOCBegin)*10
mt$veraenproC<-mt$veraen/mt$cum.input
mt$veraenproC2 = (ifelse(mt$veraenproC == "Inf"|
                           mt$veraenproC == "-Inf", (mt$veraen),
                         (mt$veraenproC)))

mt$jahrveraen<-mt$veraen/mt$duration..y.
mt$jahrveraenprotC<-mt$jahrveraen/mt$manureC.input..Mg..Ha.y.
mt$jahrveraenprotC2 = (ifelse(mt$jahrveraenprotC == "Inf"|
                                mt$jahrveraenprotC == "-Inf", (mt$jahrveraen),
                              (mt$jahrveraenprotC)))

mt$cum.input<-mt$duration..y.*mt$manureC.input..Mg..Ha.y.
mt$veraenC<-mt$SoilOC.END...-mt$SoilOCBegin
mt$veraen<-(mt$SoilOC.END...-mt$SoilOCBegin)*10
mt$jahrveraen<-mt$veraen/mt$duration..y.
mt$Cinputy<-mt$manureC.input..Mg..Ha.y.+0.01
mtx<-mt[,c(3,17,45)]
sds<-cast(mtx, SoilOC.END...~Name)
mdata <- melt(mtx, id=c("Name")) 
m2<-cast(mdata,Name~variable)


##### Dataset for each site ###########
Lethbridge015<-subset(mt, Name=="Lethbridge Canada")
Lethbridge015irrigated<-subset(mt, Name=="Lethbridge Canada irrigated")
Coffeeville<-subset(mt, Name=="Coffeeville USA")
Cruger<-subset(mt, Name=="Cruger USA")
Clarksville<-subset(mt, Name=="Clarksville USA")
darmstadt<-subset(mt, Name=="Darmstadt Germany")
Gongzhuling<-subset(mt, Name=="Gongzhuling China")
Iasi<-subset(mt, Name=="Iasi Romania")
Hawalbaghcattlecompost<-subset(mt, Name=="India cattle compost")
HawalbaghVermicompost<-subset(mt, Name=="India Vermicompost")
Michigangrain<-subset(mt, Name=="Michigan USA corn 1")
Michigansilage<-subset(mt, Name=="Michigan USA corn 2")
NIpig<-subset(mt, Name=="Ireland pig slurry")
NIcattle<-subset(mt, Name=="Ireland Cattle slurry")
GroßKreutz<-subset(mt, Name=="Groß Kreutz Germany")
Darmstadt<-subset(mt, Name=="Darmstadt Germany")
Turin<-subset(mt, Name=="Turin Italy")
Aiello<-subset(mt, Name=="Aiello del Friuli Italy")
Melle<-subset(mt, Name=="Melle Belgium")
Moystad<-subset(mt, Name=="Moystad Sweden")
BLStallmist<-subset(mt, Name=="BL Germany FYM")
Dixoncattle<-subset(mt, Name=="Dixon USA cattle")
Dixonswine<-subset(mt, Name=="Dixon USA swine")
Isfahan<-subset(mt, Name=="Isfahan Iran")
Laiyang<-subset(mt, Name=="Laiyang China")
Melfort<-subset(mt, Name=="Melfort Canada")
Moody<-subset(mt, Name=="Moody County USA")
Müncheberg<-subset(mt, Name=="Müncheberg Germany")
Offer<-subset(mt, Name=="Offer Sweden")
Qiyang<-subset(mt, Name=="Qiyang China")
Shenyang<-subset(mt, Name=="Shenyang China")
Robacksdalen<-subset(mt, Name=="Robacksdalen Sweden")
Texasgrass<-subset(mt, Name=="Texas grassland")
WoburnFYM<-subset(mt, Name=="Woburn  FYM")
WoburnOM<-subset(mt, Name=="Woburn UK Organic")
WoburnSS<-subset(mt, Name=="Woburn UK sew.sludge")
WoburnSC<-subset(mt, Name=="Woburn UK sludge comp.")
WoburnVC<-subset(mt, Name=="Woburn UK veg.comp.")
Zhengzhou<-subset(mt, Name=="Zhengzhou China")
Agassiz<-subset(mt, Name=="Agassiz Canada")
As<-subset(mt, Name=="As Sweden")
BLstatic<-subset(mt, Name=="BL Germany static")


all45<- rbind(Lethbridge015,Lethbridge015irrigated, Coffeeville, Cruger, darmstadt, Dixoncattle
              , Dixonswine,Michigangrain, Michigansilage, Melle, Moystad , NIpig,NIcattle,BLStallmist
              ,Hawalbaghcattlecompost, HawalbaghVermicompost, GroßKreutz, Turin, Aiello)
all3<-rbind(Clarksville, Gongzhuling, Iasi, BLstatic,  Laiyang
            , Melfort, Moody, Müncheberg, Offer, Qiyang, Shenyang
            , Robacksdalen, Texasgrass, WoburnFYM, WoburnOM, WoburnSC, WoburnSS, WoburnVC
            , Zhengzhou, Agassiz, As)




#######LINEAR models #### NO SATURATION ##########

library(dplyr)
library(broom)
library(lmtest)


dfLin = mt %>% group_by(Name) %>%
  do(fit = lm(SoilOC.END...*10 ~ Cinputy, data=.))
dfLinCoef = tidy(dfLin, fit)
dfLinPred = augment(dfLin, fit)
dfLinSumm = glance(dfLin, fit)


MeanSlope<-subset(dfLinCoef,term=="Cinputy")

#######GAM smoother #######
dfgam = mt %>% group_by(Name) %>%
  do(fit = gam(SoilOC.END...*10 ~ s(Cinputy, k=3), data=.))
dfgamCoef = tidy(dfgam, fit)
dfgamPred = augment(dfgam, fit)
dfgamSumm = glance(dfgam, fit,adj.r.squared)



####### Asymptotic models ## SATURATION #########


############ Lethbridge 015 #########
fitasym<-nls(SoilOC.END... ~ SSasymp(Cinputy, A, lrc, c0), data = Lethbridge015)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Lethbridge015)
resettest(fitlm, power=2, type="regressor", data=Lethbridge015)
resettest(fitlm, power=3, type="regressor", data=Lethbridge015)
plot(SoilOC.END...*10 ~ Cinputy, data=Lethbridge015)
lines(Lethbridge015$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Lethbridge015$Cinputy, fitted(fitlm), lty = 2, col = "black")

AIC(fitasym, fitlm)

################## Lethbridge 015irrigated ########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Lethbridge015irrigated)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Lethbridge015irrigated)
resettest(fitlm, power=2, type="regressor", data=Lethbridge015irrigated)
resettest(fitlm, power=3, type="regressor", data=Lethbridge015irrigated)
plot(SoilOC.END...*10 ~ Cinputy, Lethbridge015irrigated,xlim=c(0,30), ylim=c(0,90), xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Lethbridge015irrigated$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Lethbridge015irrigated$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

################ Aiello #######
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Aiello)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Aiello)
resettest(fitlm, power=2, type="regressor", data=Aiello)
resettest(fitlm, power=3, type="regressor", data=Aiello)
plot(SoilOC.END...*10 ~ Cinputy, Aiello)
lines(Aiello$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Aiello$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

########## BLStallmist #########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = BLStallmist, trace=T)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=BLStallmist)
resettest(fitlm, power=2, type="regressor", data=BLStallmist)
resettest(fitlm, power=3, type="regressor", data=BLStallmist)
plot(SoilOC.END...*10 ~ Cinputy, BLStallmist)
#lines(BLStallmist$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(BLStallmist$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitlm,fitasym)

########## Coffeeville ########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Coffeeville)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Coffeeville)
resettest(fitlm, power=2, type="regressor", data=Coffeeville)
resettest(fitlm, power=3, type="regressor", data=Coffeeville)
plot(SoilOC.END...*10 ~ Cinputy, Coffeeville, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Coffeeville$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Coffeeville$Cinputy, fitted(fitlm), lty = 2, col = "black")
lines(Coffeeville$Cinputy, fitted(powerfit), lty = 2, col = "red", lwd = 2)
AIC(fitasym, fitlm)

######## Cruger #######
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Cruger)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Cruger)
resettest(fitlm, power=2, type="regressor", data=Cruger)
resettest(fitlm, power=3, type="regressor", data=Cruger)
plot(SoilOC.END...*10 ~ Cinputy, Cruger, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Cruger$Cinputy, fitted(fitasym), lty = 2, col = "red")
ines(Cruger$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

############ Darmstadt #####
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Darmstadt)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Darmstadt)
resettest(fitlm, power=2, type="regressor", data=Darmstadt)
resettest(fitlm, power=3, type="regressor", data=Darmstadt)
plot(SoilOC.END...*10 ~ Cinputy, Darmstadt, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Darmstadt$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Darmstadt$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm, fitasym)

########## Dixoncattle ##########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Dixoncattle)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Dixoncattle)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Dixoncattle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Dixoncattle)
plot(SoilOC.END...*10 ~ Cinputy, Dixoncattle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Dixoncattle$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Dixoncattle$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)
############# Dixonswine ####
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Dixonswine)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Dixonswine)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Dixonswine)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Dixonswine)
plot(SoilOC.END...*10 ~ Cinputy, Dixonswine, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Dixonswine$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Dixonswine$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitlm, fitasym)

######### GroßKreutz ########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = GroßKreutz)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=GroßKreutz)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=GroßKreutz)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=GroßKreutz)
plot(SoilOC.END...*10 ~ Cinputy, GroßKreutz, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(GroßKreutz$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(GroßKreutz$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)

#######Halwabaghcattlecompost #########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Hawalbaghcattlecompost)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Hawalbaghcattlecompost)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Hawalbaghcattlecompost)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Hawalbaghcattlecompost)
plot(SoilOC.END...*10 ~ Cinputy, Hawalbaghcattlecompost, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Hawalbaghcattlecompost$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Hawalbaghcattlecompost$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)
############ HalwabaghVermicompost ############
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = HawalbaghVermicompost)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=HawalbaghVermicompost)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=HawalbaghVermicompost)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=HawalbaghVermicompost)
plot(SoilOC.END...*10 ~ Cinputy, HawalbaghVermicompost, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(HawalbaghVermicompost$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(HawalbaghVermicompost$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

########## Melle ###########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Melle)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Melle)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Melle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Melle)
plot(SoilOC.END...*10 ~ Cinputy, Melle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Melle$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Melle$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)

########## Michigangrain ########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Michigangrain)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Michigangrain)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Michigangrain)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Michigangrain)
plot(SoilOC.END...*10 ~ Cinputy, Michigangrain, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
lines(Michigangrain$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Michigangrain$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

########### Michigansilage #########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Michigansilage)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Michigansilage)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Michigansilage)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Michigansilage)
plot(SoilOC.END...*10 ~ Cinputy, Michigansilage, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Michigansilage$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Michigansilage$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)
############ Moysatd ############
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Moystad)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Moystad)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Moystad)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Moystad)
plot(SoilOC.END...*10 ~ Cinputy, Moystad, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Moystad$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Moystad$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)

##### NIcattle ########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = NIcattle)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=NIcattle)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=NIcattle)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=NIcattle)
plot(SoilOC.END...*10 ~ Cinputy, NIcattle, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(NIcattle$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(NIcattle$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)

####### NI pig #########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = NIpig)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=NIpig)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=NIpig)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=NIpig)
plot(SoilOC.END...*10 ~ Cinputy, NIpig, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(NIpig$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(NIpig$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm, fitasym)
######### Turin ###########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Turin)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Turin)
resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Turin)
resettest(fitlm, power=3, type=c("fitted", "regressor","princomp"), data=Turin)
plot(SoilOC.END...*10 ~ Cinputy, Turin, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Turin$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Turin$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm,fitasym)

######Agassiz ##########
#fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Agassiz)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Agassiz)
resettest(fitlm, power=2, type=c( "regressor"), data=Agassiz)
resettest(fitlm, power=3, type=c( "regressor"), data=Agassiz)
plot(SoilOC.END...*10 ~ Cinputy, Agassiz, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Agassiz$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Agassiz$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC( fitlm, fitasym)


####### As ###########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = As)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=As)
plot(SoilOC.END...*10 ~ Cinputy, As, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(As$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(As$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)
########### BL static ######
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = BLstatic)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=BLstatic)
plot(SoilOC.END...*10 ~ Cinputy, BLstatic, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(BLstatic$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(BLstatic$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

#########Clarksville ###########
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Clarksville)
fitlm<-lm(SoilOC.END...*10 ~ Cinputy,data=Clarksville)
plot(SoilOC.END...*10 ~ Cinputy, Clarksville, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Clarksville$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Clarksville$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)

####### Gongzhuling ############
fitasym<-nls(SoilOC.END...*10 ~ SSasymp(Cinputy, A, lrc, c0), data = Gongzhuling)
fitlm<-lm(SOCEndOrigin*10 ~ Cinputy,data=Gongzhuling)
plot(SOCEndOrigin*10 ~ Cinputy, Gongzhuling, xlab="Manure C input (Mg/ha y)", ylab="Resulting SOC (g/kg)")
#lines(Gongzhuling$Cinputy, fitted(fitasym), lty = 2, col = "red")
lines(Gongzhuling$Cinputy, fitted(fitlm), lty = 2, col = "black")
AIC(fitasym, fitlm)


############ Ramsey resettest  #############
dftest = mt %>% group_by(Name) %>%
  do(tt = resettest(lm(SoilOC.END...*10 ~ Cinputy, data=.), power=2, type=c("fitted", "regressor","princomp"), data=.))
#resettest(fitlm, power=2, type=c("fitted", "regressor","princomp"), data=Moystad)
#fitsmooth<-gam(SoilOC.END...*10~s(Cinputy, k=3), data=Moystad)
#dftest=mt %>% group_by(Name) %>%
#do(as.data.frame(resettest(SoilOC.END...*10 ~ Cinputy, power=2,data=.)))
# get the coefficients by group in a tidy data_frame
dftestCoef = tidy(dftest, tt)


# get the predictions by group in a tidy data_frame
dftestPred = augment(dftest, tt)


# get the summary statistics by group in a tidy data_frame
dftestSumm = glance(dftest, tt)


