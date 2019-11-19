#10_allfish analysis
setwd("~/Desktop/Masters Analysis/analysis")
source("EmilyThesis/9_abundances.R")


fish <- read.csv("data/2018fishwbulk.csv")
sitedata <- full_join(sitedata, fish, by = "sitecode.x")

keeppp <- c("creek chub")#underhill removed
#keepsc <- c("AC", "BW", "EP1", "HT", "JW", "LEST", "PF", "PVTF", "SC", "UH", "WA", "WH", "WW")
#removed DE and BW because need to do separately
sitedata <- sitedata[sitedata$species %in% keeppp,]


fitccw <- lm(forkmm ~ agimpact, data=sitedata)
summary(fitccw)
plot(fitccw$resid)
plot(sitedata$forkmm~sitedata$agimpact, ylab="creek chub fork length",xlab="agricultural gradient")
abline(91.509, -4.557)

fitccw <- lm(forkmm ~ edibles, data=sitedata)
summary(fitccw)
plot(fitccw$resid)
plot(sitedata$forkmm~sitedata$edibles, ylab="creek chub fork length",xlab="agricultural gradient")
abline(88.592, -4.043)

fitedible <- lm(stomachTP ~ agimpact, data = sitedata)
summary(fitedible)
plot(sitedata$stomachTP ~ sitedata$agimpact, xlab = "agricultural intensity gradient", ylab = "stomach content trophic position")
abline(1.82803, -0.04496)
boxplot(agimpact ~ stomachTP, data = sitedata, xlab = "agricultural intensity gradient", ylab = "stomach content trophic position")
