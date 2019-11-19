#9_abundances
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/7_invertdiversity.R")

library(tidyverse)
library(car)
library(Hmisc)
library(corrgram)
library ( vegan )

plot(sitedata$creek.chub~sitedata$agimpact)
plot(sitedata$creek.chub~sitedata$PC1)

sitedata$edibles <- sitedata$trichoptera + sitedata$amphipoda + sitedata$diptera + sitedata$ephemeroptera + 
  sitedata$hemiptera + sitedata$plecoptera + sitedata$hirudinia + sitedata$lepidoptera + sitedata$coleoptera + 
  sitedata$isopoda + sitedata$hymenoptera + sitedata$megaloptera + sitedata$odonata

sitedata$terpred <- sitedata$p_aranae + sitedata$p_hymenoptera + sitedata$p_opiliones + sitedata$p_odonata + 
  sitedata$p_acari + sitedata$p_neuroptera + sitedata$p_mantodea + sitedata$p_mecoptera

sitedata$tercon <- sitedata$p_hemiptera + sitedata$p_coleoptera + sitedata$p_lepidoptera + 
  sitedata$p_orthoptera + sitedata$p_diplopoda + sitedata$p_psocoptera + sitedata$pphasmatodea + 
  sitedata$p_ephemeroptera + sitedata$p_thysanoptera

sitedata$terom <- sitedata$p_p_diptera + sitedata$p_stylommatophora + sitedata$p_trichoptera + 
  sitedata$p_dermaptera + sitedata$p_collembola + sitedata$p_isopoda + sitedata$p_plecoptera

sitedata$aqpred <- sitedata$oligochaeta + sitedata$hydrachnidia + sitedata$hirudinia + sitedata$megaloptera + 
  sitedata$odonata

sitedata$aqcon <- sitedata$gastropoda + sitedata$ephemeroptera + sitedata$coleoptera + sitedata$unionidae + 
  sitedata$nematoda + sitedata$psocoptera + sitedata$lepidoptera

sitedata$aqom <- sitedata$trichoptera + sitedata$amphipoda + sitedata$diptera + sitedata$hemiptera + 
  sitedata$plecoptera + sitedata$isopoda + sitedata$hymenoptera

sitedata[,168:192] [ sitedata[,168:192] == 0 ] <- NA
sitedata[,197:217] [ sitedata[,197:217] == 0 ] <- NA
#sitedata[c(2,10,13),195:218]  <- NA
sitedata[,39:70] [ sitedata[,39:70] == 0 ] <- NA

#keep <- c("AC", "AT", "BG",  "EP1", "EP2", "EP3", "EP4", "FP", "GF", "HC", "HT", "JC", "JW", "KC", "LEF", "LEST", "MT", "PF", "PVTF", "SC", "UH", "VN", "WA", "WH", "WW")
#removed DE and BW because need to do separately
#sitedata <- sitedata[sitedata$sitecode.x %in% keep,]


fitcc <- lm(log(creek.chub) ~ july.nitrogen, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$july.nitrogen, ylab="creek chub abundance",xlab="july nitrogen")
#abline(53.251, 10.791)

fitcc <- lm(log(creek.chub) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$agimpact, ylab="log creek chub abundance",xlab="agricultural impact gradient")
#abline(3.3976, 0.2442)

fitcc <- lm(log(creek.chub) ~ log(july.phosphorus), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~log(sitedata$july.phosphorus), ylab="log creek chub abundance",xlab="log july phosphorus")
#abline(4.07836, -0.01347)

fitcc <- lm(log(creek.chub) ~ log(buf_width), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~log(sitedata$buf_width), ylab="log creek chub abundance",xlab="log buffer width(m)")
#abline(4.07836, -0.01347)

fitcc <- lm(log(creek.chub) ~ edibles, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$edibles, ylab="log creek chub abundance",xlab="edible aquatic insect abundance")
abline(2.638262, 0.007632)
#

fitcc <- lm(log(creek.chub) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(log(sitedata$creek.chub)~sitedata$agimpact, ylab="log creek chub abundance",xlab="prey fish abundance")
#abline(2.638262, 0.007632)
#

fitcc <- lm(qlogis(creek.chub/totalcount.y) ~ agimpact, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(qlogis(sitedata$creek.chub/sitedata$totalcount.y)~sitedata$agimpact, ylab="creek chub to aq invert ratio",xlab="agricultural impact gradient")
abline(-6.0030, 0.9572)

fitcc <- lm(qlogis(creek.chub/totalcount.y) ~ log(july.phosphorus), data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(qlogis(sitedata$creek.chub/sitedata$totalcount.y)~log(sitedata$july.phosphorus), ylab="creek chub to aq invert ratio",xlab="log july total phosphorus")
#abline(3.3976, 0.2442)

fitcc <- lm(qlogis(creek.chub/totalcount.y) ~ july.nitrogen, data=sitedata)
summary(fitcc)
plot(fitcc$resid)
plot(qlogis(sitedata$creek.chub/sitedata$totalcount.y)~sitedata$july.nitrogen, ylab="creek chub to aq invert ratio",xlab="july total nitrogen")
#abline(3.3976, 0.2442)

fittotalaq <- lm(log(totalcount.y)~agimpact, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$agimpact, ylab="log aquatic invertebrate abundance",xlab="agricultural impact gradient")
abline(2.78181,0.20884)

fittotalaq <- lm(log(totalcount.y)~rip_tree_cover, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$rip_tree_cover, ylab="log aquatic invertebrate abundance",xlab="percent canopy cover")
abline(4.817509, -0.019984)

fittotalaq <- lm(log(totalcount.y)~buf_width, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$buf_width, ylab="log aquatic invertebrate abundance",xlab="buffer width (m)")
abline(3.879455, -0.001547)

fittotalaq <- lm(log(totalcount.y)~july.nitrogen, data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~sitedata$july.nitrogen, ylab="log aquatic invertebrate abundance",xlab="july total nitrogen")
#abline(4.817509, -0.019984)

fittotalaq <- lm(log(totalcount.y)~log(july.phosphorus), data=sitedata)
summary(fittotalaq)
plot(fittotalaq$resid)
plot(log(sitedata$totalcount.y)~log(sitedata$july.phosphorus), ylab="log aquatic invertebrate abundance",xlab="log july total phosphorus")
abline(4.7951, 0.3365)

fit7 <- lm(qlogis((p_p_diptera/100))~agimpact, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(qlogis((sitedata$p_p_diptera/100))~sitedata$agimpact, ylab="terrestrial diptera abundance",xlab="agricultural impact gradient")

fit7 <- lm(qlogis((p_p_diptera/100))~buf_width, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(qlogis((sitedata$p_p_diptera/100))~sitedata$buf_width, ylab="terrestrial diptera abundance",xlab="buffer width")

fit7 <- lm(qlogis((p_p_diptera/100))~rip_tree_cover, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(qlogis((sitedata$p_p_diptera/100))~sitedata$rip_tree_cover, ylab="terrestrial diptera abundance",xlab="percent canopy coverage")
abline(-2.225339, 0.016738)

fit7 <- lm(log(diptera)~agimpact, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(log(sitedata$diptera)~sitedata$agimpact, ylab="aquatic diptera abundance",xlab="agricultural impact gradient")
abline(0.8206, 0.2980)
#.

fit7 <- lm(log(diptera)~buf_width, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(log(sitedata$diptera)~sitedata$buf_width, ylab="aquatic diptera abundance",xlab="buffer width")
#

fit7 <- lm(log(diptera)~july.nitrogen, data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(log(sitedata$diptera)~sitedata$july.nitrogen, ylab="log aquatic diptera abundance",xlab="July total nitrogen")
abline(1.4403, 0.1570)
#*

fit7 <- lm(log(diptera)~log(july.phosphorus), data=sitedata)
summary(fit7)
plot(fit7$resid)
plot(log(sitedata$diptera)~log(sitedata$july.phosphorus), ylab="log aquatic diptera abundance",xlab="log July total phosphorus")
#abline(1.4403, 0.1570)
#

fitn <- lm(july.nitrogen~agimpact, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(sitedata$july.nitrogen~sitedata$agimpact, ylab="July total nitrogen",xlab="agricultural impact gradient")
abline(-1.8045,1.4437)

fitn <- lm(july.nitrogen~buf_width, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(sitedata$july.nitrogen~sitedata$buf_width, ylab="July total nitrogen",xlab="agricultural impact gradient")

fitn <- lm(log(july.phosphorus)~agimpact, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(log(sitedata$july.phosphorus)~sitedata$agimpact, ylab="July total Phosphorus",xlab="agricultural impact gradient")
abline(-4.0198,0.2241)

fitn <- lm(log(july.phosphorus)~buf_width, data = sitedata)
summary(fitn)
plot(fitn$resid)
plot(log(sitedata$july.phosphorus)~sitedata$buf_width, ylab="July total Phosphorus",xlab="buffer width")
abline(-4.0198,0.2241)

fits <- lm(c.chub.tl ~ buf_width, data = sitedata)
summary(fits)
plot(fitn$resid)
plot(sitedata$july.nitrogen~sitedata$turbidity, ylab="July total nitrogen",xlab="agricultural impact gradient")

fitedible <- lm(edibles ~ log(july.phosphorus), data = sitedata)
summary(fitedible)
plot(sitedata$edibles ~ log(sitedata$july.phosphorus), xlab = "log july total phosphorus", ylab = "aquatic prey invert abundance")

fitedible <- lm(edibles ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$edibles ~ sitedata$agimpact, xlab = "agricultural intensity gradient", ylab = "aquatic prey invert abundance")

fitedible <- lm(edibles ~ july.nitrogen, data = sitedata)
summary(fitedible)
plot(sitedata$edibles ~ sitedata$july.nitrogen, xlab = "july total nitrogen", ylab = "aquatic prey invert abundance")

fitedible <- lm(edibles ~ totalcount.y, data = sitedata)
summary(fitedible)
plot(sitedata$edibles ~ sitedata$totalcount.y, ylab = "aquatic prey invert abundance")

#par(mfrow=c(2,2))
plot(log(sitedata$c.chub.tl) ~ log(sitedata$edibles))

fitedible <- lm(terom ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$terom ~ sitedata$agimpact, xlab = "agricultural intensity gradient")

fitedible <- lm(terpred ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$terpred ~ sitedata$agimpact, xlab = "agricultural intensity gradient")

fitedible <- lm((aqpred/totalcount.y) ~ agimpact, data = sitedata)
summary(fitedible)
plot((sitedata$aqpred/sitedata$totalcount.y) ~ sitedata$agimpact, xlab = "agricultural intensity gradient")

fitedible <- lm(debris_cover ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$debris_cover ~ sitedata$agimpact, xlab = "agricultural intensity gradient", ylab = "percent organic debris cover")
boxplot(agimpact ~ debris_cover, data = sitedata, xlab = "agricultural intensity gradient", ylab = "percent organic debris cover")

