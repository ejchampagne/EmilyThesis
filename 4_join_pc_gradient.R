#4_join_pc_gradient
setwd("~/Desktop/Masters Analysis/analysis")

source("EmilyThesis/3_calculate_trophic_metrics.R")

library(car)
library(vegan)

pca <- read.csv("data/2018mastersanalysis.csv")
fish <- read.csv("data/2018fishwbulk.csv")
fishmeta <- read.csv("data/2018fishmetadata.csv")
# colnames(fishmeta)[1] <- "sitecode.x"
envdata <- read.csv("data/2018PCAaggrad.csv")

si <- si[!is.na(si$sampleid),]

si <- left_join(si, pca, by = "sitecode.x")
si <- left_join(si, fish, by = "sampleid")
colnames(si)[colnames(si)=="sitecode.x.x"] <- "sitecode.x"
si <- left_join(si, fishmeta, by = "sitecode.x")
si <- left_join(si, envdata, by = "sitecode.x")

si$log_p_ag <- log10(si$p_ag_watershed)
si$log_sal <- log10(si$salinity)
si$log_sin <- log10(si$sinuosity)
si$log_D <- log10(si$discharge)
si$log_grain <- log10(si$grain_size)
si$log_drainarea <- log(si$drainage_area)

terinv <- read.csv("data/ter_invert18.csv")

#Calculates diversity 
div_terinv <-terinv[c(1:28),c(4:28)]
terinv$ter_div<-diversity(div_terinv, index = "shannon")
terinv$ter_div
#evenness
terinv$ter_even <- terinv$ter_div/log(specnumber(div_terinv))
terinv$ter_even


terinv$terpred <- terinv$p_aranae + terinv$p_hymenoptera + terinv$p_opiliones + terinv$p_odonata + 
  terinv$p_acari + terinv$p_neuroptera + terinv$p_mantodea + terinv$p_mecoptera

aqinv <- read.csv("data/aq_invert18.csv")

#Calculates diversity 
div_aqinv <-aqinv[c(1:27),c(4:24)]
aqinv$aq_div<-diversity(div_aqinv, index = "shannon")
aqinv$aq_div

#evenness
aqinv$aq_even <- aqinv$aq_div/log(specnumber(div_aqinv))
aqinv$aq_even
#aqinv[c(2,9,12),3:26]  <- NA

aqinv$edibles <- aqinv$trichoptera + aqinv$amphipoda + aqinv$diptera + aqinv$ephemeroptera + 
  aqinv$hemiptera + aqinv$plecoptera + aqinv$hirudinia + aqinv$lepidoptera + aqinv$coleoptera + 
  aqinv$isopoda + aqinv$hymenoptera + aqinv$megaloptera + aqinv$odonata

aqinv$aqpred <- aqinv$oligochaeta + aqinv$hydrachnidia + aqinv$hirudinia + aqinv$megaloptera + 
  aqinv$odonata

si <- full_join(si, terinv, by = "sitecode.x")
si <- full_join(si, aqinv, by = "sitecode.x")

si[,226:250] [ si[,226:250] == 0 ] <- NA
si[,255:276] [ si[,255:276] == 0 ] <- NA
si[,56:87] [ si[,56:87] == 0 ] <- NA

si <- select(si,-c(X.x,old.sampleid.x, replicate,old.sampleid.y, sitecode.y, species.y,role.y, 
             pc1, adj_pc1, pc2, adj_pc2, sitename.x.x, date, reachdistance, volts, hertz, pass1start,
             pass1end, pass2start, pass2end, pass3start, pass3end, totaltime, X.y, mt_setdate, 
             mt_settime, mt_enddate, mt_endtime, uptrap, midtrap, downtrap, mt_notes, sitename.x.x.x, 
             sitename.y.y.y, sitecode.x.y, sitename.y))

si <- si[-c(165:170),]
