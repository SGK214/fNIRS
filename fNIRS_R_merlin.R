rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load("plyr", "dplyr", "ez", "ggplot2", "lme4","R.matlab", "RColorBrewer")


# SETTING & FUNCTIONS ----------------------------------------------------------
# make  plotting theme
pl.pd <- position_dodge(0.2); pl.sc <- 4; pl.trsp = 0.6;
pal <- brewer.pal("Blues", n=9); pl.bkg <- pal[2]; pl.mid <- pal[5]; pl.foc <- pal[8]
plot.theme <- theme(
        panel.background = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        legend.key = element_rect(fill = NA),
        panel.grid.major = element_line(color = pl.bkg),
        plot.margin = margin(pl.sc*5,pl.sc*5,pl.sc*5,pl.sc*5, unit = "pt"),
        axis.ticks = element_blank(),
        axis.text = element_text(color = pl.mid),
        strip.text = element_text(color = pl.mid),
        text = element_text(color = pal[9], size = pl.sc*6),
        plot.title = element_text(margin = margin(pl.sc*10,0,pl.sc*5,0)),
        axis.title.y = element_text(colour = pl.foc, margin = margin(0,pl.sc*4,0,0, unit = "pt")),
        axis.title.x = element_text(colour = pl.foc, margin = margin(pl.sc*4,0,0,0, unit = "pt")),
        legend.title = element_blank(),
        legend.position = "top"
)

setwd("E:/Ladu/Projects/fNIRS")
Kersten <- '/Data/Rdata/Kersten/'
Richard <- '/Data/Rdata/Richard/'
Paul <- '/Data/Rdata/Paul/'
Martin <- '/Data/Rdata/Martin/'
sub <- c(Kersten, Richard, Martin, Paul)

#1-12 - vasakul pool
#13-24 paremal pool
left <- c("A","B","C","D","E","F","G","H","I","J","K","L")
right <- c("M", "N", "O","P","Q", "R", "S", "T","U","V","W","X")
# ROI's - Heschl's gyrus and the superior temporal gyrus
# Three channels on the left-hand side formed the left ROI and three channels on the right-hand side formed the right ROI
# LEFT: 8, 11, 9 (H, I, K)
# RIGHT: 21, 24, 22 (X, V, U)
leftROI <- c("H", "I", "K")
rightROI <- c("X", "V", "U")

# IMPORT ------------------------------------------------------------------
Audit_Hb0_all <- NULL
Audit_Hb0_fun_all <- NULL
Visual_Hb0_all <- NULL
Visual_Hb0_fun_all <- NULL
Audit_HbR_all <- NULL
Audit_HbR_fun_all <- NULL
Visual_HbR_all <- NULL
Visual_HbR_fun_all <- NULL
Fs <- 10 # sampling frequency = 10 Hz
for (i in 1:length(sub)){
        Audit_Hb0 <- readMat(paste(getwd(), sub[i],"fNIRS_audit_Hb0.MAT", sep=""), fixNames = FALSE) # Auditory stimuli
        Audit_Hb0 <- Audit_Hb0$delta_Hb0_auditory
        Time <- dim(Audit_Hb0)#Audit_Hb0
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Audit_Hb0<-as.data.frame.table(Audit_Hb0)
        Audit_Hb0$Time <- Time
        Audit_Hb0$sub <- i
        
        Audit_HbR <- readMat(paste(getwd(), sub[i],"fNIRS_audit_HbR.MAT",sep =""), fixNames = FALSE)
        Audit_HbR <- Audit_HbR$delta_HbR_auditory
        Time <- dim(Audit_HbR)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Audit_HbR<- as.data.frame.table(Audit_HbR)
        Audit_HbR$Time <- Time
        Audit_HbR$sub <- i
        
        Audit_Hb0_functional <- readMat(paste(getwd(), sub[i],"fNIRS_audit_Hb0_Yamada.MAT",sep =""), fixNames = FALSE) # Audit. functional components
        Audit_Hb0_functional <- Audit_Hb0_functional$funct_init_audit_Hb0
        Time <- dim(Audit_Hb0_functional)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Audit_Hb0_functional<-as.data.frame.table(Audit_Hb0_functional)
        Time <- rep(Time,24)
        Audit_Hb0_functional$Time <- Time
        Audit_Hb0_functional$sub <- i
        
        Audit_HbR_functional <- readMat(paste(getwd(), sub[i],"fNIRS_audit_HbR_Yamada.MAT",sep=""),  fixNames = FALSE)
        Audit_HbR_functional <- Audit_HbR_functional$funct_init_audit_HbR
        Time <- dim(Audit_HbR_functional)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Audit_HbR_functional<-as.data.frame.table(Audit_HbR_functional)
        Audit_HbR_functional$Time <- Time
        Audit_HbR_functional$sub <- i
        
        Visual_Hb0 <- readMat(paste(getwd(), sub[i],"fNIRS_visual_Hb0.MAT",sep=""), fixNames = FALSE) # Visual stimuli
        Visual_Hb0 <- Visual_Hb0$delta_Hb0_visual
        Time <- dim(Visual_Hb0)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Visual_Hb0<-as.data.frame.table(Visual_Hb0)
        Visual_Hb0$Time <- Time
        Visual_Hb0$sub <- i
        
        Visual_HbR <- readMat(paste(getwd(), sub[i],"fNIRS_visual_HbR.MAT",sep=""), fixNames = FALSE) 
        Visual_HbR <- Visual_HbR$delta_HbR_visual
        Time <- dim(Visual_HbR)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Visual_HbR<-as.data.frame.table(Visual_HbR)
        Visual_HbR$Time <- Time
        Visual_HbR$sub <- i
        
        Visual_Hb0_functional <- readMat(paste(getwd(), sub[i],"fNIRS_visual_Hb0_Yamada.MAT",sep =""), fixNames = FALSE) # Visual functional components
        Visual_Hb0_functional <- Visual_Hb0_functional$funct_init_visual_Hb0
        Time <- dim(Visual_Hb0_functional)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Visual_Hb0_functional<-as.data.frame.table(Visual_Hb0_functional)
        Visual_Hb0_functional$Time <- Time
        Visual_Hb0_functional$sub <- i
        
        Visual_HbR_functional <- readMat(paste(getwd(), sub[i],"fNIRS_visual_HbR_Yamada.MAT",sep =""), fixNames = FALSE)
        Visual_HbR_functional <- Visual_HbR_functional$funct_init_visual_HbR
        Time <- dim(Visual_HbR_functional)
        Time <- (1:Time[1])*(1/Fs) # calculate time values for plots
        Time <- rep(Time,24)
        Visual_HbR$Time <- Time
        Visual_HbR_functional<-as.data.frame.table(Visual_HbR_functional)
        Visual_HbR_functional$Time <- Time
        Visual_HbR_functional$sub <- i
        
        Audit_Hb0_all <- rbind(Audit_Hb0_all, Audit_Hb0)
        Audit_Hb0_fun_all <- rbind(Audit_Hb0_fun_all, Audit_Hb0_functional)
        Visual_Hb0_all <- rbind(Visual_Hb0_all, Visual_Hb0)
        Visual_Hb0_fun_all <- rbind(Visual_Hb0_fun_all, Visual_Hb0_functional)
        Audit_HbR_all <- rbind(Audit_HbR_all, Audit_HbR)
        Audit_HbR_fun_all <- rbind(Audit_HbR_fun_all, Audit_HbR_functional)
        Visual_HbR_all <- rbind(Visual_HbR_all, Visual_HbR)
        Visual_HbR_fun_all <- rbind(Visual_HbR_fun_all, Visual_HbR_functional)
}



# DATA for ANALYSIS -----------------------------------------------------------
# Block avarage across the repetitions of each stimulus - avarage haemodynamic response to each stimulus: 
# 1) condition, 
# 2) channel, and 
# 3) participant

#AUDITORY CONDITION
Audit_ROI <- Audit_Hb0_fun_all %>%
        filter(Var3 %in% leftROI | Var3 %in% rightROI)

Audit_ROI$ROI <- NA
Audit_ROI[Audit_ROI$Var3 %in% leftROI, "ROI"] <- "left"
Audit_ROI[Audit_ROI$Var3 %in% rightROI, "ROI"] <- "right"

Audit_ROI_avg <- ddply(Audit_ROI, .(ROI, sub, Var3), summarise, keskmised = mean(Freq), maximum = max(Freq))

latency_temp <- ddply(Audit_ROI,  .(ROI, sub, Var3), function(x){ #calculates the peak latency
        mean.f <- max(x$Freq)
        tulem <- x[x$Freq >= mean.f, "Time"][1]
        return(tulem)
        })

Audit_ROI_avg <- join(Audit_ROI_avg, latency_temp, by = c("ROI", "sub", "Var3"))
colnames(Audit_ROI_avg)<- c("ROI","sub","Var3","avg","maximum","latency" )
Audit_ROI_avg$cond <- "Audit" # Data with the avarages for each condition, two ROI's and subjects, peak amplitude and latency to peak

# VISUAL CONDITION
Visual_ROI <- Visual_Hb0_fun_all %>%
        filter(Var3 %in% leftROI | Var3 %in% rightROI)
Visual_ROI$ROI <- NA
Visual_ROI[Visual_ROI$Var3 %in% leftROI, "ROI"] <- "left"
Visual_ROI[Visual_ROI$Var3 %in% rightROI, "ROI"] <- "right"

Visual_ROI_avg <- ddply(Visual_ROI, .(ROI, sub, Var3), summarise, keskmised = mean(Freq), maximum = max(Freq))

lattemp <- ddply(Visual_ROI,  .(ROI, sub, Var3), function(x){
        mean.f <- max(x$Freq)
        tulem <- x[x$Freq >= mean.f, "Time"][1]
        return(tulem)
})
Visual_ROI_avg <- join(Visual_ROI_avg, lattemp, by = c("ROI", "sub", "Var3"))
colnames(Visual_ROI_avg)<- c("ROI","sub","Var3","avg","maximum","latency" )
Visual_ROI_avg$cond <- "Visual"# Data with the avarages for each condition, two ROI's and subjects, peak amplitude and latency to peak

allData <- rbind(Audit_ROI_avg, Visual_ROI_avg)
allData$avg <- allData$avg*(-1)
# #EXPLORATORY GRAPHS -----------------------------------------------------
Audit_Hb0_fun_all$cond <- "Auditory"
Visual_Hb0_fun_all$cond <- "Visual"
plottime <- rbind(Audit_Hb0_fun_all, Visual_Hb0_fun_all)
plottime <- plottime %>%
        filter(Var3 %in% leftROI | Var3 %in% rightROI)
plottime$ROI <- NA
plottime[plottime$Var3 %in% leftROI, "ROI"] <- "left"
plottime[plottime$Var3 %in% rightROI, "ROI"] <- "right"

plottime$Freq <- plottime$Freq*(-1)
ggplot(plottime, aes(x = Time, y = Freq))+
        geom_smooth(aes(group = cond, colour = cond))+
        xlab("Stimulus presentation (sec)")+
        ylab("HbO response (umol)")+
        plot.theme + theme(panel.grid.major.x = element_blank())+
        theme(legend.position = "right")+
        facet_wrap(~ROI)

ggplot(allData, (aes(cond, avg)))+
        geom_boxplot(fill = "gray96")+
        plot.theme + theme(panel.grid.major.x = element_blank())+
        xlab("Condition")+
        ylab("Avarage HbO response")+
        scale_x_discrete(breaks=c("Audit", "Visual"),
                           labels=c("Auditory", "Visual"))

ggplot(allData, (aes(cond, avg)))+
        geom_boxplot(aes(fill = factor(ROI)))+
        plot.theme + theme(panel.grid.major.x = element_blank())+
        xlab("Condition")+
        ylab("Avarage HbO response (umol)")+
        scale_x_discrete(breaks=c("Audit", "Visual"),
                         labels=c("Auditory", "Visual"))+
        theme(legend.position = "right")+
        scale_fill_manual(values=c("wheat", "thistle3"))




# MAIN EFFECTS ---------------------------------------------------------------------

#Random Effects in Classical ANOVA - http://conjugateprior.org/2013/01/formulae-in-r-anova/
aov.m1= aov(avg~cond+ROI + Error(sub/(cond+ROI)),allData)
summary(aov.m1)


# AMPLITUDE ANALYSIS ------------------------------------------------------
ddply(allData, .(cond), summarise, meanl = mean(latency), meanm = mean(maximum), sdmeanm = sd(maximum))



# GRAPHS ---------------------------------------
temp <- Audit_Hb0_fun_all
temp$side <- NA
temp[temp$Var3%in%left,"side"] <- "L"
temp[temp$Var3%in%right,"side"] <- "R"


ggplot(temp[temp$Var3%in%left,], (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(Var3)))+
        ggtitle("Auditory left")+
        ylab("My x label") +
        xlab("My x label") 
ggplot(temp[temp$Var3%in%right,], (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(Var3)))+
        ggtitle("Auditory right")
ggplot(temp, (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(side)))+
        ggtitle("Auditory left vs right")
ddply(temp, .(sub, side), summarise, keskmised = mean(Freq))



tempV <- Visual_Hb0_all
tempV$side <- NA
tempV[tempV$Var3%in%left,"side"] <- "L"
tempV[tempV$Var3%in%right,"side"] <- "R"
ggplot(tempV[tempV$Var3%in%left,], (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(Var3)))+
        ggtitle("Visual left")
ggplot(tempV[tempV$Var3%in%right,], (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(Var3)))+
        ggtitle("Visual right")

ggplot(tempV, (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(side)))+
        ggtitle("Visual left vs right")

ddply(tempV, .(sub, side), summarise, keskmised = mean(Freq))

temp$cond <- "auditory"
tempV$cond <- "visual"
tempA <- rbind(temp,tempV)
ddply(tempA, .(sub, cond), summarise, keskmised = mean(Freq))
ggplot(tempA, (aes(Time, Freq)))+
        geom_boxplot(aes(fill = factor(cond)))+
        ggtitle("Conditions")
