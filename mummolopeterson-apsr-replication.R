# 'Demand Effects in Survey Experiments: An Empirical Assessment
# Jonathan Mummolo and Erik Peterson
# 11-2-2018
# Contact information: jmummolo@princeton.edu; erik.peterson@tamu.edu

#!! Set Working Directory to Appropriate Replication Folder
setwd('/mummolopeterson-demandeffects-replicationfile/')

#######
###Load Individual Data Frames for Each Study/Experiment
#######

#Survey 1 - MTurk - Framing / News
load(file="turk_news.Rdata")
news <-d3
news <-na.omit(news[,c("response","party2","treat","profile","news_treat","respid","news_exp","treat_other")])
rm(d3)

load( file="turk.Rdata")
framing <- d
rm(d)

#Surve 2 - MTurk - News / Resume
load("turk_news2.Rdata")
news2 <-d3
rm(d3)

load("turk2.Rdata")
resume <- d
rm(d)

#Survey 3 - Mturk - Democratic Peace / Welfare
load("turk_incentive.Rdata")
incentive <- turk_incentive
rm(turk_incentive)

#Survey 4 - Qualtrics - News
load(file='qualtrics.news.guess.RData')
load(file='qualtrics.news.choice.RData')

#Survey 5 - Qualtrics - Democratic Peace / Welfare / Framing
load(file='qualtrics.other.RData')

########
###Load Combined Data Frame for Pooled Analyses
########
load('combined.frame.all.RData')
load('combined.frame.guess.RData')

########
###Packages
########
library(plm)
library(apsrtable)
library(rms)
library(xtable)
library(plyr)

#########
###Manipulation Check - Guess Correct Hypotheses by Condition
#########
#Study1
frame.guess <-lm(correct_guess_frame ~ frame_treat, data=framing)
names(frame.guess$coefficients) <- c('(Intercept)','Hint','Explanation')
frame.guess$se <- sqrt(diag(vcovHC(frame.guess, type="HC1")))

news.guess <- lm(correct_guess_news ~ news_treat, data=framing)
names(news.guess$coefficients) <- c('(Intercept)','Hint','Explanation')
news.guess$se<-sqrt(diag(vcovHC(news.guess, type="HC1")))

#Study2
resume.h1.guess <- lm(guess_white ~ resume_treat, data=resume)
names(resume.h1.guess$coefficients) <- c('(Intercept)','White Helps','Black Helps')
resume.h1.guess$se<-sqrt(diag(vcovHC(resume.h1.guess, type="HC1")))

resume.h2.guess <- lm(guess_black ~ resume_treat, data=resume)
names(resume.h2.guess$coefficients) <- c('(Intercept)','White Helps','Black Helps')
resume.h2.guess$se<-sqrt(diag(vcovHC(resume.h2.guess, type="HC1")))

news.h1.guess <- lm(guess_friendly ~ news_treat, data=resume)
names(news.h1.guess$coefficients) <- c('(Intercept)','Unfriendly Source','Friendly Source')
news.h1.guess$se <- sqrt(diag(vcovHC(news.h1.guess, type="HC1")))

news.h2.guess <- lm(guess_unfriendly ~ news_treat, data=resume)
names(news.h2.guess$coefficients) <- c('(Intercept)','Unfriendly Source','Friendly Source')
news.h2.guess$se <- sqrt(diag(vcovHC(news.h2.guess, type="HC1")))

#Study 3
peace.guess <- lm(ir.guess.correct ~ ir.treatment, data=incentive)
names(peace.guess$coefficients) <- c('(Intercept)','Explanation','Incentive')
peace.guess$se <- sqrt(diag(vcovHC(peace.guess, type="HC1")))

lazy.guess <- lm(cp.guess.correct ~ cp.treatment, data=incentive)
names(lazy.guess$coefficients) <- c('(Intercept)','Explanation','Incentive')
lazy.guess$se <- sqrt(diag(vcovHC(lazy.guess, type="HC1")))

#Study 4 - Qualtrics - News
news.guess.qualtrics <- lm(correct.guess ~ demand.condition, data=qualtrics.news.guess)
names(news.guess.qualtrics$coefficients) <- c('(Intercept)','Explanation','Incentive')
news.guess.qualtrics$se <- sqrt(diag(vcovHC(news.guess.qualtrics, type="HC1")))

#Study 4 - Qualtrics - Other Studies
peace.guess.qualtrics <- lm(peace.correct.guess ~ demand.condition.peace,data=qualtrics.other)
names(peace.guess.qualtrics$coefficients) <- c('(Intercept)','Explanation','Incentive')
peace.guess.qualtrics$se <- sqrt(diag(vcovHC(peace.guess.qualtrics, type="HC1")))

welfare.guess.qualtrics <- lm(welfare.correct.guess ~ demand.condition.welfare,data=qualtrics.other)
names(welfare.guess.qualtrics$coefficients) <- c('(Intercept)','Explanation','Incentive')
welfare.guess.qualtrics$se <- sqrt(diag(vcovHC(welfare.guess.qualtrics, type="HC1")))

frame.guess.qualtrics <- lm(frame.correct.guess ~ demand.condition.frame,data=qualtrics.other)
names(frame.guess.qualtrics$coefficients) <- c('(Intercept)','Explanation','Incentive')
frame.guess.qualtrics$se <- sqrt(diag(vcovHC(frame.guess.qualtrics, type="HC1")))

#Guess Rate
frame.base <- mean(framing$correct_guess_frame[which(framing$frame_treat=='Baseline')],na.rm=TRUE)
news1.base <- mean(framing$correct_guess_news[which(framing$news_treat=='Baseline')],na.rm=TRUE)

news2.base <- mean(resume$guess_friendly[which(resume$news_treat=='Baseline')],na.rm=TRUE)
resume.base <- mean(resume$guess_white[which(resume$resume_treat=='Baseline')],na.rm=TRUE)

ir.base <- mean(incentive$ir.guess.correct[which(incentive$ir.treatment=='Baseline')],na.rm=TRUE)
cp.base <- mean(incentive$cp.guess.correct[which(incentive$cp.treatment=='Baseline')],na.rm=TRUE)

news3.base <- mean(qualtrics.news.guess$correct.guess[which(qualtrics.news.guess$demand.condition=='baseline')],na.rm=TRUE)
frame.qualtrics.base <- mean(qualtrics.other$frame.correct.guess[which(qualtrics.other$demand.condition.frame=='Baseline')],na.rm=TRUE)
ir.qualtrics.base <- mean(qualtrics.other$peace.correct.guess[which(qualtrics.other$demand.condition.peace=='Baseline')],na.rm=TRUE)
cp.qualtrics.base <- mean(qualtrics.other$welfare.correct.guess[which(qualtrics.other$demand.condition.welfare=='Baseline')],na.rm=TRUE)

#########
###Correlation between Guessing Correctly on Different Items (Footnote 8)
#########

#Survey 1
frame.news.1 <- cor(framing$correct_guess_frame[which(framing$frame_treat=='Baseline' & framing$news_treat=='Baseline')],framing$correct_guess_news[which(framing$frame_treat=='Baseline' & framing$news_treat=='Baseline')], use='complete.obs')

table(framing$correct_guess_frame[which(framing$frame_treat=='Baseline' & framing$news_treat=='Baseline')],framing$correct_guess_news[which(framing$frame_treat=='Baseline' & framing$news_treat=='Baseline')])
summary(lm(correct_guess_frame ~ correct_guess_news, data=framing, subset=which(framing$frame_treat=='Baseline' & framing$news_treat=='Baseline')))

#Survey 2
resume.news.2 <- cor(resume$guess_white[which(resume$resume_treat=='Baseline' & resume$news_treat=='Baseline')], resume$guess_friendly[which(resume$resume_treat=='Baseline' & resume$news_treat=='Baseline')], use='complete.obs')
summary(lm(guess_white ~ guess_friendly, data=resume, subset=which(resume$resume_treat=='Baseline' & resume$news_treat=='Baseline')))

#Survey 3
ir.cp.3 <- cor(incentive$ir.guess.correct[which(incentive$ir.treatment=='Baseline' & incentive$cp.treatment=='Baseline')], incentive$cp.guess.correct[which(incentive$ir.treatment=='Baseline' & incentive$cp.treatment=='Baseline')], use='complete.obs')
summary(lm(ir.guess.correct ~ cp.guess.correct, data=incentive, subset=which(incentive$ir.treatment=='Baseline' & incentive$cp.treatment=='Baseline')))

#Survey 5
peace.welfare.5 <- cor(qualtrics.other$peace.correct.guess[which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')], qualtrics.other$welfare.correct.guess[which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')], use='complete.obs')
summary(lm(peace.correct.guess ~ welfare.correct.guess, data=qualtrics.other, subset=which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')))

peace.frame.5 <- cor(qualtrics.other$peace.correct.guess[which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.frame=='Baseline')], qualtrics.other$frame.correct.guess[which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.frame=='Baseline')], use='complete.obs')
summary(lm(peace.correct.guess ~ frame.correct.guess, data=qualtrics.other, subset=which(qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$demand.condition.frame=='Baseline')))

welfare.frame.5 <- cor(qualtrics.other$welfare.correct.guess[which(qualtrics.other$demand.condition.frame=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')], qualtrics.other$frame.correct.guess[which(qualtrics.other$demand.condition.frame=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')], use='complete.obs')
summary(lm(welfare.correct.guess ~ frame.correct.guess, data=qualtrics.other, subset=which(qualtrics.other$demand.condition.frame=='Baseline' & qualtrics.other$demand.condition.welfare=='Baseline')))

guess.correlations <- rbind.data.frame(frame.news.1,resume.news.2,ir.cp.3,peace.welfare.5,peace.frame.5,welfare.frame.5)
colnames(guess.correlations) <- c('Correlation')
rownames(guess.correlations) <- c('Study 1 (Frame/News)','Study 2 (Resume/News)','Study 3 (Peace/Welfare)','Study 5 (Peace/Welfare)','Study 5 (Peace/Frame)','Study 5 (Welfare/Frame)')
xtable(guess.correlations,caption='Cross-Experiment Correlation of Correctly Guessing Experimenter Intent')


########
########
###FIGURE B1 HERE
########
########
pdf(file='guessrate-combined.pdf',height=5,width=5)
par(mar=c(4.1,6.25,1,1))
barplot(rev(c(frame.base,news1.base,resume.base,news2.base,ir.base,cp.base,news3.base,frame.qualtrics.base,ir.qualtrics.base,cp.qualtrics.base)),horiz=TRUE,xlim=c(0,.85),names.arg=rev(c('Frame 1','News 1','Resume','News 2','Dem Peace 1','Welfare 1','News 3','Frame 2','Dem Peace 2','Welfare 2')),las=1,xlab='% Correctly Guessing Experimenter Intent',col=rev(c('black','black','black','black','black','black','gray50','gray50','gray50','gray50')),xaxt='n')
#axis(side=2,at=1.3,label='TRIAL',las=1,line=-1,tick=FALSE)
legend("topright",legend=c('MTurk','Qualtrics'),col=c('black','gray50'),pch=c(15,15),pt.cex=c(2,2))
axis(side=1,at=c(0,.2,.4,.6,.8),labels=c('0%','20%','40%','60%','80%'))
dev.off()

guess.organization <- c('Study 1','Study 1','Study 1','Study 1','Study 2','Study 2','Study 2','Study 2','Study 2','Study 2','Study 2','Study 2','Study 3','Study 3','Study 3','Study 3','Study 4','Study 4','Study 5','Study 5','Study 5','Study 5','Study 5','Study 5')
guess.topic <- c('Framing','Framing','News','News','Resume','Resume','Resume','Resume','News','News','News','News','Dem Peace','Dem Peace','Welfare','Welfare','News','News','Framing','Framing','Dem Peace','Dem Peace','Welfare','Welfare')
guess.correct.outcome <- c(1,1,1,1,1,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1)
guess.coefficients <- c(frame.guess$coefficients[2:3],news.guess$coefficients[2:3],resume.h1.guess$coefficients[2:3],resume.h2.guess$coefficients[2:3],news.h2.guess$coefficients[2:3],news.h1.guess$coefficients[2:3],peace.guess$coefficients[2:3],lazy.guess$coefficients[2:3],news.guess.qualtrics$coefficients[2:3],frame.guess.qualtrics$coefficients[2:3],peace.guess.qualtrics$coefficients[2:3],welfare.guess.qualtrics$coefficients[2:3])
guess.ses <- c(frame.guess$se[2:3],news.guess$se[2:3],resume.h1.guess$se[2:3],resume.h2.guess$se[2:3],news.h2.guess$se[2:3],news.h1.guess$se[2:3],peace.guess$se[2:3],lazy.guess$se[2:3],news.guess.qualtrics$se[2:3],frame.guess.qualtrics$se[2:3],peace.guess.qualtrics$se[2:3],welfare.guess.qualtrics$se[2:3])

guess.output <- cbind.data.frame(guess.organization,guess.topic,guess.correct.outcome,names(guess.coefficients),guess.coefficients,guess.ses)
names(guess.output) <- c('study','topic','correct.outcome','treatment','coefficient','se')
guess.output$upper <- guess.output$coefficient+2*guess.output$se
guess.output$lower <- guess.output$coefficient-2*guess.output$se
guess.plot <- subset(guess.output,guess.output$correct.outcome==1)
guess.plot$plot.names <- c('Hint','Explicit','Hint','Explicit','Negative Effect','Positive Effect','Negative Effect','Positive Effect','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive')
guess.plot$incentive.experiments <- ifelse(guess.plot$study %in% c('Study 3','Study 4','Study 5'),1,0)

guess.plot.initial <- subset(guess.plot,guess.plot$incentive.experiments==0)
guess.plot.incentive <- subset(guess.plot,guess.plot$incentive.experiments==1)

########
########
###FIGURE 1 HERE
########
########
pdf("Figure1.pdf",height=5, width=6.5)
par(mar=c(4.1,6.75,1,6))
plot(y=dim(guess.plot.initial)[1]:1,x=guess.plot.initial$coefficient,pch=16,xlim=c(-.05,.3),yaxt='n',ylab='',xlab="Change in Pr(Correctly Guess Experiment's Purpose)",cex=2)
abline(v=0)
segments(x0=guess.plot.initial$upper,x1=guess.plot.initial$lower,y1=dim(guess.plot.initial)[1]:1,y0=dim(guess.plot.initial)[1]:1,lwd=4)
axis(side=2,at=dim(guess.plot.initial)[1]:1,labels=guess.plot.initial$plot.names,las=1)
#axis(side=4,at=c(10.5,6.5,2.5),labels=c('Study 1','Study 2','Study 3'),las=1)
axis(side=4,at=c(7.5,5.5,3.5,1.5),labels=c('Framing
(Survey 1)','Partisan News
(Survey 1)','Resum\uE9
(Survey 2)','Partisan News
(Survey 2)'),las=1,tick=FALSE,line=-.75)
abline(h=c(6.5,4.5,2.5),lty=2)
dev.off()

########
########
###FIGURE 3 HERE
########
########
pdf("Figure3.pdf",height=5, width=6.5)
par(mar=c(4.1,5,1,6))
plot(y=dim(guess.plot.incentive)[1]:1,x=guess.plot.incentive$coefficient,pch=16,xlim=c(-.075,.3),yaxt='n',ylab='',xlab="Change in Pr(Correctly Guess Experiment's Purpose)",cex=2,col=c('black','black','black','black','gray50','gray50','gray50','gray50','gray50','gray50','gray50','gray50'))
abline(v=0)
segments(x0=guess.plot.incentive$upper,x1=guess.plot.incentive$lower,y1=dim(guess.plot.incentive)[1]:1,y0=dim(guess.plot.incentive)[1]:1,lwd=4,col=c('black','black','black','black','gray50','gray50','gray50','gray50','gray50','gray50','gray50','gray50'))
axis(side=2,at=dim(guess.plot.incentive)[1]:1,labels=guess.plot.incentive$plot.names,las=1)
#axis(side=4,at=c(10.5,6.5,2.5),labels=c('Study 1','Study 2','Study 3'),las=1)
axis(side=4,at=c(11.5,9.5,7.5,5.5,3.5,1.5),labels=c('Dem Peace
(Survey 3)','Welfare
(Survey 3)','Partisan News
(Survey 4)','Framing
(Survey 5)','Dem Peace
(Survey 5)','Welfare
(Survey 5)'),las=1,tick=FALSE,line=-.75)
abline(h=c(10.5,8.5,6.5,4.5,2.5),lty=2)
legend("bottomright",legend=c('MTurk','Qualtrics'),col=c('black','gray50'),pch=c(16,16),pt.cex=c(2,2))
dev.off()

#######
###Treatment Effects by Condition - Relative to baseline condition
#######
#Survey 2
frame.outcome <- lm(rally_dv ~ frame*frame_treat, data=framing)

news1.outcome <- lm(response ~ treat*news_treat, data=news)
m<-ols(response ~ treat*news_treat, data=news, x=T, y=T)
m.news.clust<-robcov(m, cluster=news$respid)
news1.outcome$se <-sqrt(diag(vcov(m.news.clust)))

#Survey 2
resume.outcome <- lm(res_outcome ~ name_treat*resume_treat, data=resume)
resume.outcome$se <- sqrt(diag(vcovHC(resume.outcome, type="HC1")))

news2.outcome<- lm(response ~ treat*news_treat, data=news2)
m2 <-ols(response ~ treat*news_treat, data=news2, x=T, y=T)
m2.news.clust <- robcov(m2, cluster=news2$respid)
news2.outcome$se <- sqrt(diag(vcov(m2.news.clust)))

#Survey 3
peace.outcome <- lm(ir.outcome ~ I(dem.treatment==0)*ir.treatment,data=incentive)
welfare.outcome <- lm(cp.outcome ~ lazy.treatment*cp.treatment,data=incentive)

#Survey 4
news3.outcome <- lm(selected ~ copartisan.source*demand.condition,data=qualtrics.news.choice)
m3 <- ols(selected ~ copartisan.source*demand.condition, data=qualtrics.news.choice,x=T,y=T)
m3.news.clust <- robcov(m3,cluster=qualtrics.news.choice$rnid)
news3.outcome$se <- sqrt(diag(vcov(m3.news.clust)))

#Survey 5
peace.outcome.qualtrics <- lm(peace.outcome ~ dem.treatment*demand.condition.peace,data=qualtrics.other)
welfare.outcome.qualtrics <- lm(welfare.outcome ~ welfare.treatment*demand.condition.welfare,data=qualtrics.other)
frame.outcome.qualtrics <- lm(framing.outcome ~ speech.treatment*demand.condition.frame,data=qualtrics.other)

#Extract Treatment Effects in Each Category
me.extract <- function(model,vcov.mat){
	baseline.treat <- model$coefficients[2]
	group1.treat <- model$coefficients[2] + model$coefficients[5]
	group2.treat <- model$coefficients[2] + model$coefficients[6]
	
	baseline.se <- sqrt(vcov.mat[2,2])
	group1.se <- sqrt(vcov.mat[2,2] + vcov.mat[5,5] + 2*vcov.mat[2,5])
	group2.se <- sqrt(vcov.mat[2,2] + vcov.mat[6,6] + 2*vcov.mat[2,6])
	
	coefs <- c(baseline.treat,group1.treat,group2.treat)
	ses <- c(baseline.se,group1.se,group2.se)
	output <- cbind.data.frame(coefs,ses)
	names(output) <- c('treatment.effect','se')
	return(output)
}

me.diff.extract <- function(model,vcov.mat){
	diff1 <- model$coefficients[5]
	diff2 <- model$coefficients[6]
	
	diff1.se <- sqrt(vcov.mat[5,5])
	diff2.se <- sqrt(vcov.mat[6,6])
	
	coefs <- c(diff1,diff2)
	ses <- c(diff1.se,diff2.se)
	output <- cbind.data.frame(coefs,ses)
	names(output) <- c('treatment.effect','se')
	return(output)
}

#Marginal Effect Plot
frame.mes <- me.extract(frame.outcome,vcovHC(frame.outcome, type="HC1"))
news1.mes <- me.extract(news1.outcome,vcov(m.news.clust))
resume.mes <- me.extract(resume.outcome,vcovHC(resume.outcome, type="HC1"))
news2.mes <- me.extract(news2.outcome,vcov(m2.news.clust))
peace.mes <- me.extract(peace.outcome,vcovHC(peace.outcome, type="HC1"))
welfare.mes <- me.extract(welfare.outcome,vcovHC(welfare.outcome, type="HC1"))

frame.qualtrics.mes <- me.extract(frame.outcome.qualtrics,vcovHC(frame.outcome.qualtrics, type="HC1"))
news3.mes <- me.extract(news3.outcome,vcov(m3.news.clust))
peace.qualtrics.mes <- me.extract(peace.outcome.qualtrics,vcovHC(peace.outcome.qualtrics, type="HC1"))
welfare.qualtrics.mes <- me.extract(welfare.outcome.qualtrics,vcovHC(welfare.outcome.qualtrics, type="HC1"))

me.combined.plot <- rbind(frame.mes,news1.mes,resume.mes,news2.mes,peace.mes,welfare.mes,news3.mes,frame.qualtrics.mes,peace.qualtrics.mes,welfare.qualtrics.mes)

me.study <- c('Survey 1','Survey 1','Survey 1','Survey 1','Survey 1','Survey 1','Survey 2','Survey 2','Survey 2','Survey 2','Survey 2','Survey 2','Survey 3','Survey 3','Survey 3','Survey 3','Survey 3','Survey 3','Survey 4','Survey 4','Survey 4','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5')
me.name <- c('Framing 1','Framing 1','Framing 1','News 1','News 1','News 1','Resume','Resume','Resume','News 2','News 2','News 2','Dem Peace','Dem Peace','Dem Peace','Welfare','Welfare','Welfare','News 3','News 3','News 3','Framing 2','Framing 2','Framing 2','Dem Peace 2','Dem Peace 2','Dem Peace 2','Welfare 2','Welfare 2','Welfare 2')
me.treatment <- c('Replication','Replication+Hint','Replication+Explicit','Replication','Replication+Hint','Replication+Explicit','Replication','Replication+Negative Effect','Replication+Positive Effect','Replication','Replication+Negative Effect','Replication+Positive Effect','Replication','Replication+Explicit','Replication+Incentive','Replication','Replication+Explicit','Replication+Incentive','Replication','Replication+Explicit','Replication+Incentive','Replication','Replication+Explicit','Replication+Incentive','Replication','Replication+Explicit','Replication+Incentive','Replication','Replication+Explicit','Replication+Incentive')

me.plot.frame <- cbind.data.frame(me.study,me.name,me.treatment,me.combined.plot)
me.plot.frame$upper <- me.plot.frame$treatment.effect + 2*me.plot.frame$se
me.plot.frame$lower <- me.plot.frame$treatment.effect - 2*me.plot.frame$se

########
########
###TABLE B2 HERE
########
########
xtable(me.plot.frame[,c('me.study','me.name','me.treatment','treatment.effect','se','lower','upper')])

#Difference From Baseline Treatment Effect Plot
frame.mes.diff <- me.diff.extract(frame.outcome,vcovHC(frame.outcome, type="HC1"))
news1.mes.diff <- me.diff.extract(news1.outcome,vcov(m.news.clust))
resume.mes.diff <- me.diff.extract(resume.outcome,vcovHC(resume.outcome, type="HC1"))
news2.mes.diff <- me.diff.extract(news2.outcome,vcov(m2.news.clust))
peace.mes.diff <- me.diff.extract(peace.outcome,vcovHC(peace.outcome, type="HC1"))
welfare.mes.diff <- me.diff.extract(welfare.outcome,vcovHC(welfare.outcome, type="HC1"))

frame.qualtrics.mes.diff <- me.diff.extract(frame.outcome.qualtrics,vcovHC(frame.outcome.qualtrics, type="HC1"))
news3.mes.diff<- me.diff.extract(news3.outcome,vcov(m3.news.clust))
peace.qualtrics.mes.diff <- me.diff.extract(peace.outcome.qualtrics,vcovHC(peace.outcome.qualtrics, type="HC1"))
welfare.qualtrics.mes.diff<- me.diff.extract(welfare.outcome.qualtrics,vcovHC(welfare.outcome.qualtrics, type="HC1"))

me.combined.plot.diff <- rbind(frame.mes.diff,news1.mes.diff,resume.mes.diff,news2.mes.diff,peace.mes.diff,welfare.mes.diff,news3.mes.diff,frame.qualtrics.mes.diff,peace.qualtrics.mes.diff,welfare.qualtrics.mes.diff)

me.study.diff <- c('Survey 1','Survey 1','Survey 1','Survey 1','Survey 2','Survey 2','Survey 2','Survey 2','Survey 3','Survey 3','Survey 3','Survey 3','Survey 4','Survey 4','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5','Survey 5')
me.name.diff <- c('Frame','Frame','News 1','News 1','Resume','Resume','News 2','News 2','Dem Peace','Dem Peace','Welfare','Welfare','News 3','News 3','Frame 2','Frame 2','Dem Peace 2','Dem Peace 2','Welfare 2','Welfare 2')
me.treatment.diff <- c('Hint','Explicit','Hint','Explicit','Negative Effect','Positive Effect','Negative Effect','Positive Effect','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive','Explicit','Incentive')

me.diff.plot.frame <- cbind.data.frame(me.study.diff,me.name.diff,me.treatment.diff,me.combined.plot.diff)
me.diff.plot.frame$upper <- me.diff.plot.frame$treatment.effect + 2*me.diff.plot.frame$se
me.diff.plot.frame$lower <- me.diff.plot.frame$treatment.effect - 2*me.diff.plot.frame$se

me.diff.plot.frame.initial <- subset(me.diff.plot.frame,me.diff.plot.frame$me.study.diff %in% c('Survey 1','Survey 2'))
me.diff.plot.frame.incentive <- subset(me.diff.plot.frame,me.diff.plot.frame$me.study.diff %in% c('Survey 3','Survey 4','Survey 5'))

########
########
###FIGURE 2 HERE
########
########
pdf("Figure2.pdf",height=5, width=6.5)
par(mar=c(4,6.75,1,5.75))
plot(y=dim(me.diff.plot.frame.initial)[1]:1,x=me.diff.plot.frame.initial$treatment.effect,pch=16,xlim=c(-.3,.3),yaxt='n',ylab='',xlab="Difference in Treatment Effects (Demand Condition - Replication)",cex=2,xaxt='n')
abline(v=0)
segments(x0=me.diff.plot.frame.initial$upper,x1=me.diff.plot.frame.initial$lower,y1=dim(me.diff.plot.frame.initial)[1]:1,y0=dim(me.diff.plot.frame.initial)[1]:1,lwd=4)
axis(side=2,at=dim(me.diff.plot.frame.initial)[1]:1,labels=me.diff.plot.frame.initial$me.treatment.diff,las=1)
#axis(side=4,at=c(10.5,6.5,2.5),labels=c('Study 1','Study 2','Study 3'),las=1)
axis(side=4,at=c(7.5,5.5,3.5,1.5),labels=c('Framing
(Survey 1)','Partisan News
(Survey 1)','Resum\uE9
(Survey 2)','Partisan News
(Survey 2)'),las=1,tick=FALSE,line=-.75)
axis(side=1,at=c(-.3,-.2,-.1,0,.1,.2,.3))
abline(h=c(6.5,4.5,2.5),lty=2)
dev.off()

########
########
###FIGURE 4 HERE
########
########
pdf("Figure4.pdf",height=5, width=6.5)
par(mar=c(4,5.25,1,5.75))
plot(y=dim(me.diff.plot.frame.incentive)[1]:1,x=me.diff.plot.frame.incentive$treatment.effect,pch=16,xlim=c(-.25,.25),yaxt='n',ylab='',xlab="Difference in Treatment Effects (Demand Condition - Replication)",cex=2,xaxt='n',type='n')
abline(v=0)
points(y=dim(me.diff.plot.frame.incentive)[1]:1,x=me.diff.plot.frame.incentive$treatment.effect,col=c('black','black','black','black','gray50','gray50','gray50','gray50','gray50','gray50','gray50','gray50'),pch=16,cex=2)
segments(x0=me.diff.plot.frame.incentive$upper,x1=me.diff.plot.frame.incentive$lower,y1=dim(me.diff.plot.frame.incentive)[1]:1,y0=dim(me.diff.plot.frame.incentive)[1]:1,lwd=4,col=c('black','black','black','black','gray50','gray50','gray50','gray50','gray50','gray50','gray50','gray50'))
axis(side=2,at=dim(me.diff.plot.frame.incentive)[1]:1,labels=me.diff.plot.frame.incentive$me.treatment.diff,las=1)
#axis(side=4,at=c(10.5,6.5,2.5),labels=c('Study 1','Study 2','Study 3'),las=1)
axis(side=4,at=c(11.5,9.5,7.5,5.5,3.5,1.5),labels=c('Dem Peace
(Survey 3)','Welfare
(Survey 3)','Partisan News
(Survey 4)','Framing
(Survey 5)','Dem Peace
(Survey 5)','Welfare
(Survey 5)'),las=1,tick=FALSE,line=-.75)
abline(h=c(10.5,8.5,6.5,4.5,2.5),lty=2)
#axis(side=4,at=c(7.5,5.5,3.5,1.5),labels=c('Frame 1','News 1','Resume','News 2'),las=1,tick=FALSE,line=-.75)
axis(side=1,at=c(-.2,-.1,0,.1,.2))
#abline(h=c(6.5,4.5,2.5),lty=2)
legend("bottomright",legend=c('MTurk','Qualtrics'),col=c('black','gray50'),pch=c(16,16),pt.cex=c(2,2))
dev.off()

#######
###Benchmark Treatments in Replication Conditions Back to the Original Experiments
#######
#Mummolo Partisan News Experiment | Estimated from Replication Data after subsetting to comparable tasks
	#Turk Treatment Effect - 0.084 / SE 0.029
	#SSI Treatment Effect - 0.118 / SE 0.035

news.comparison.coefs <- c(0.084,0.118,news1.mes$treatment.effect[1],news2.mes$treatment.effect[1],news3.mes$treatment.effect[1])
news.comparison.ses <- c(0.029,0.035,news1.mes$se[1],news2.mes$se[1],news3.mes$se[1])
news.comparison.frame <- cbind.data.frame(news.comparison.coefs,news.comparison.ses)
news.comparison.frame$upper <- news.comparison.frame[,1] + 2*news.comparison.frame[,2]
news.comparison.frame$lower <- news.comparison.frame[,1] - 2*news.comparison.frame[,2]

########
########
###FIGURE B4 HERE
########
########
pdf("news-effect-comparison.pdf",height=5, width=6.5)
par(mar=c(4,7.5,4,1))
plot(y=5:1,x=news.comparison.frame[,1],pch=16,cex=2,xlim=c(0,.3),yaxt='n',ylab='',xlab='Effect of Co-Partisan Source',main='Partisan Selective Exposure Study')
segments(y0=5:1,x0=news.comparison.frame$lower,x1=news.comparison.frame$upper,lwd=4)
abline(h=3.5,lty=2)
axis(side=2,at=5:1,labels=c('Mummolo 2016
(Mturk)','Mummolo 2016
(SSI)','This Study
(Mturk-1)','This Study
(Mturk-2)','This Study
(Qualtrics)'),las=1)
dev.off()

#Mullinix et al. Replication of Issue Framing Experiment
	#Page 130 - Appendix 1 / Mturk Sample: Effect = 0.20, SE=0.02
	#Page 130 - Appendix 1 / GfK Sample: Effect = 0.16, SE=0.03
framing.comparison.coefs <- c(0.20,0.16,frame.mes$treatment.effect[1],frame.qualtrics.mes$treatment.effect[1])
framing.comparison.ses <- c(0.02,0.03,frame.mes$se[1],frame.qualtrics.mes$se[1])

framing.comparison.frame <- cbind.data.frame(framing.comparison.coefs,framing.comparison.ses)
framing.comparison.frame$upper <- framing.comparison.frame[,1] + 2*framing.comparison.frame[,2]
framing.comparison.frame$lower <- framing.comparison.frame[,1] - 2*framing.comparison.frame[,2]

########
########
###FIGURE B3 HERE
########
########
pdf("framing-effect-comparison.pdf",height=5, width=6.5)
par(mar=c(4,8.25,4,1))
plot(y=4:1,x=framing.comparison.frame[,1],pch=16,cex=2,xlim=c(0,.3),yaxt='n',ylab='',xlab='Effect of Free Speech Frame',main='Framing Study')
segments(y0=4:1,x0=framing.comparison.frame$lower,x1=framing.comparison.frame$upper,lwd=4)
abline(h=2.5,lty=2)
axis(side=2,at=4:1,labels=c('Mullinix et al. 2015
(Mturk)','Mullinix et al. 2015
(GfK)','This Study
(Mturk)','This Study
(Qualtrics)'),las=1)
dev.off()

#Aaroe & Petersen Welfare Experiment
	#Page 692 - Table 2 / United States YouGov Sample: Relative to No Cue Condition .21(.02) - -.21(.02)
welfare.comparison.coefs <- c(.42,welfare.mes$treatment.effect[1],welfare.qualtrics.mes$treatment.effect[1])
welfare.comparison.ses <- c(.028,welfare.mes$se[1],welfare.qualtrics.mes$se[1])
welfare.comparison.frame <- cbind.data.frame(welfare.comparison.coefs,welfare.comparison.ses)
welfare.comparison.frame$upper <- welfare.comparison.frame[,1] + 2*welfare.comparison.frame[,2]
welfare.comparison.frame$lower <- welfare.comparison.frame[,1] - 2*welfare.comparison.frame[,2]

########
########
###FIGURE B6 HERE
########
########
pdf("welfare-effect-comparison.pdf",height=5, width=6.5)
par(mar=c(4,10.5,4,1))
plot(y=3:1,x=welfare.comparison.frame[,1],pch=16,cex=2,xlim=c(0,.8),yaxt='n',ylab='',xlab='Effect of Lazy Recipient Cue',main='Welfare Support Study')
segments(y0=3:1,x0=welfare.comparison.frame$lower,x1=welfare.comparison.frame$upper,lwd=4)
abline(h=2.5,lty=2)
axis(side=2,at=3:1,labels=c('Aaroe and Petersen 2014
(YouGov)','This Study
(Mturk)','This Study
(Qualtrics)'),las=1)
dev.off()
	
#Tomz and Weeks Democratic Peace Experiment
	#Page 854 - Table  1 / United States Between YouGov Sample: 11.4 (5.9 to 17.0)
	#Page 855 - Text / United States Between MTurk Sample: 11.7 (NO CI Reported)
	#Need to recode our variables to binary to make direct comparison, this is done below
peace.outcome.qualtrics.comparison <- lm(I(peace.outcome>.5)~ dem.treatment,data=qualtrics.other,subset=which(demand.condition.peace=='Baseline'))
peace.outcome.qualtrics.comparison$se <- sqrt(diag(vcovHC(peace.outcome.qualtrics.comparison, type="HC1")))

peace.outcome.comparison <- lm(I(ir.outcome>.5) ~ I(dem.treatment==0),data=incentive,subset=which(ir.treatment=='Baseline'))
peace.outcome.comparison$se <- sqrt(diag(vcovHC(peace.outcome.comparison, type="HC1")))

dempeace.comparison.coefs <- c(0.114,0.117,peace.outcome.comparison$coefficients[2],peace.outcome.qualtrics.comparison$coefficients[2])
dempeace.comparison.ses <- c(0,0,peace.outcome.comparison$se[2],peace.outcome.qualtrics.comparison$se[2])

dempeace.comparison.frame <- cbind.data.frame(dempeace.comparison.coefs,dempeace.comparison.ses)
dempeace.comparison.frame$upper <- dempeace.comparison.frame[,1] + 2*dempeace.comparison.frame[,2]
dempeace.comparison.frame$lower <- dempeace.comparison.frame[,1] - 2*dempeace.comparison.frame[,2]
dempeace.comparison.frame[1,3] <- 0.059
dempeace.comparison.frame[1,4] <- 0.17

########
########
###FIGURE B5 HERE
########
########
pdf("dempeace-effect-comparison.pdf",height=5, width=6.5)
par(mar=c(4,10.5,4,1))
plot(y=4:1,x=dempeace.comparison.frame[,1],pch=16,cex=2,xlim=c(0,.2),yaxt='n',ylab='',xlab='Effect of Non-Democracy',main='Democratic Peace Study')
segments(y0=4:1,x0=dempeace.comparison.frame$lower,x1=dempeace.comparison.frame$upper,lwd=4)
abline(h=2.5,lty=2)
axis(side=2,at=4:1,labels=c('Tomz and Weeks 2013
(YouGov)','Tomz and Weeks 2013
(Mturk, No SE Reported)','This Study
(Mturk)','This Study
(Qualtrics)'),las=1)
dev.off()

#Experiment 2
resume.comparison <- lm(I(res_outcome > .5) ~ name_treat, data=resume, subset=which(resume_treat=='Baseline'))
resume.comparison$se <- sqrt(diag(vcovHC(resume.comparison, type="HC1")))
resume.comparison.coefs <- c(-0.0320, resume.comparison$coefficients[2])
resume.comparison.ses <- c(0,resume.comparison$se[2])

resume.comparison.frame <- cbind.data.frame(resume.comparison.coefs,resume.comparison.ses)
resume.comparison.frame$upper <- resume.comparison.frame[,1] + 2*resume.comparison.frame[,2]
resume.comparison.frame$lower <- resume.comparison.frame[,1] - 2*resume.comparison.frame[,2]

########
########
###FIGURE B7 HERE
########
########
pdf("resume-effect-comparison.pdf",height=5, width=6.5)
par(mar=c(4,13,4,1))
plot(y=2:1,x=resume.comparison.frame[,1],pch=16,cex=2,xlim=c(-.15,.15),yaxt='n',ylab='',xlab='Effect of African-American Name',main='Resum\uE9 Study')
segments(y0=2:1,x0=resume.comparison.frame$lower,x1=resume.comparison.frame$upper,lwd=4)
abline(h=1.5,lty=2)
#abline(v=0,lty=2)
axis(side=2,at=2:1,labels=c('Bertrand and Mullainathan 2004
(Field Exp, No SE Reported)','This Study
(Mturk)'),las=1)
dev.off()

#######
###Effects Among Guessers
#######
#Study 1
framing$guess_first_exp <- NA
framing$guess_first_exp[framing$news_exp==1 & framing$correct_guess_news==FALSE & framing$news_treat=="Baseline"] <- 0
framing$guess_first_exp[framing$news_exp==1 & framing$correct_guess_news==TRUE & framing$news_treat=="Baseline"] <- 1
framing$guess_first_exp[framing$news_exp==0 & framing$correct_guess_frame==FALSE & framing$frame_treat=="Baseline"] <- 0
framing$guess_first_exp[framing$news_exp==0 & framing$correct_guess_frame==TRUE & framing$frame_treat=="Baseline"] <- 1
framing.second <- subset(framing,framing$news_exp==1 & !is.na(framing$guess_first_exp))

study1.guess <- framing[,c('respid','guess_first_exp')]
news <- merge(news,study1.guess,by=c('respid'),all.x=TRUE)
news.second <- subset(news,news$news_exp==0 & !is.na(news$guess_first_exp))

#Study 2
resume$guess_first_exp <- NA
resume$guess_first_exp[resume$news_exp==1 & resume$correct_guess_news==FALSE & resume$news_treat=="Baseline"] <- 0
resume$guess_first_exp[resume$news_exp==1 & resume$correct_guess_news==TRUE & resume$news_treat=="Baseline"] <- 1
resume$guess_first_exp[resume$news_exp==0 & resume$correct_guess_resume==FALSE & resume$resume_treat=="Baseline"] <- 0
resume$guess_first_exp[resume$news_exp==0 & resume$correct_guess_resume==TRUE & resume$resume_treat=="Baseline"] <- 1
resume.second <- subset(resume,resume$news_exp==1 & !is.na(resume$guess_first_exp))

study2.guess <- resume[,c('respid','guess_first_exp')]
news2 <- merge(news2,study2.guess,by=c('respid'),all.x=TRUE)
news2.second <- subset(news2,news2$news_exp==0 & !is.na(news2$guess_first_exp))

#Study 3
peace.second <- subset(incentive,incentive$first_exp=='welfare' & incentive$cp.treatment=='Baseline')
welfare.second <- subset(incentive,incentive$first_exp=='dem_peace' & incentive$ir.treatment=='Baseline')

#Study 4b (Since Partisan News Was Alone Unable To Do Guessing There )
qualtrics.other$guess_first_exp <- NA
#Peace First
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='dem_peace' & qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$peace.correct.guess==1] <- 1
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='dem_peace' & qualtrics.other$demand.condition.peace=='Baseline' & qualtrics.other$peace.correct.guess==0] <- 0

#Welfare First
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='welfare' & qualtrics.other$demand.condition.welfare=='Baseline' & qualtrics.other$welfare.correct.guess==1] <- 1
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='welfare' & qualtrics.other$demand.condition.welfare=='Baseline' & qualtrics.other$welfare.correct.guess==0] <- 0

#Framing First
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='framing' & qualtrics.other$demand.condition.frame=='Baseline' & qualtrics.other$frame.correct.guess==1] <- 1
qualtrics.other$guess_first_exp[qualtrics.other$first_exp=='framing' & qualtrics.other$demand.condition.frame=='Baseline' & qualtrics.other$frame.correct.guess==0] <- 0

peace.second.qualtrics <- subset(qualtrics.other,qualtrics.other$second_exp=='dem_peace' & !is.na(qualtrics.other$guess_first_exp))
welfare.second.qualtrics  <- subset(qualtrics.other,qualtrics.other$second_exp=='welfare' & !is.na(qualtrics.other$guess_first_exp))
framing.second.qualtrics  <- subset(qualtrics.other,qualtrics.other$second_exp=='framing' & !is.na(qualtrics.other$guess_first_exp))

#Estimating
#Framing
frame.guess.all <- lm(rally_dv ~ frame*guess_first_exp,data=framing.second)
names(frame.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')
frame.guess.baseline <- lm(rally_dv ~ frame*guess_first_exp,data=framing.second,subset=which(frame_treat=='Baseline'))
names(frame.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#First News
news1.guess.all <- lm(response ~ treat*guess_first_exp,data=news.second)
m.guess<-ols(response ~ treat*guess_first_exp, data=news.second, x=T, y=T)
m.news.clust.guess <-robcov(m.guess, cluster=news.second$respid)
news1.guess.all$se <-sqrt(diag(vcov(m.news.clust.guess)))
names(news1.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

news1.guess.baseline <- lm(response ~ treat*guess_first_exp,data=news.second,subset=which(news_treat=='Baseline'))
m.guess.baseline <- ols(response ~ treat*guess_first_exp, data=news.second,subset=which(news_treat=='Baseline'), x=T, y=T)
m.news.clust.guess.baseline <- robcov(m.guess.baseline, cluster=news.second$respid[which(news.second$news_treat=='Baseline')])
news1.guess.baseline$se <- sqrt(diag(vcov(m.news.clust.guess.baseline)))
names(news1.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Resume
resume.guess.all <- lm(res_outcome ~ name_treat*guess_first_exp,data=resume.second)
resume.guess.all$se <- sqrt(diag(vcovHC(resume.guess.all, type="HC1")))
names(resume.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

resume.guess.baseline <- lm(res_outcome ~ name_treat*guess_first_exp,data=resume.second,subset=which(resume_treat=='Baseline'))
resume.guess.baseline$se <- sqrt(diag(vcovHC(resume.guess.baseline, type="HC1")))
names(resume.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Second News
news2.guess.all <- lm(response ~ treat*guess_first_exp,data=news2.second)
m.guess.2<-ols(response ~ treat*guess_first_exp, data=news2.second, x=T, y=T)
m.news2.clust.guess <-robcov(m.guess.2, cluster=news2.second$respid)
news2.guess.all$se <-sqrt(diag(vcov(m.news2.clust.guess)))
names(news2.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

news2.guess.baseline <- lm(response ~ treat*guess_first_exp,data=news2.second,subset=which(news_treat=='Baseline'))
m.guess.2.baseline <- ols(response ~ treat*guess_first_exp, data=news2.second,subset=which(news_treat=='Baseline'), x=T, y=T)
m.news2.clust.guess.baseline <- robcov(m.guess.2.baseline, cluster=news2.second$respid[which(news2.second$news_treat=='Baseline')])
news2.guess.baseline$se <- sqrt(diag(vcov(m.news2.clust.guess.baseline)))
names(news2.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Democratic Peace
peace.guess.all <- lm(ir.outcome ~ I(dem.treatment==0)*guesser,data=peace.second)
names(peace.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

peace.guess.baseline <- lm(ir.outcome ~ I(dem.treatment==0)*guesser,data=peace.second,subset=which(ir.treatment=='Baseline'))
names(peace.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Welfare
welfare.guess.all <- lm(cp.outcome ~ lazy.treatment*guesser,data=welfare.second)
names(welfare.guess.all$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

welfare.guess.baseline <- lm(cp.outcome ~ lazy.treatment*guesser,data=welfare.second,subset=which(cp.treatment=='Baseline'))
names(welfare.guess.baseline$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Democratic Peace 2
peace.guess.all.qualtrics <- lm(peace.outcome ~ dem.treatment*guess_first_exp,data=peace.second.qualtrics)
names(peace.guess.all.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

peace.guess.baseline.qualtrics <- lm(peace.outcome ~ dem.treatment*guess_first_exp,data=peace.second.qualtrics,subset=which(demand.condition.peace=='Baseline'))
names(peace.guess.baseline.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Welfare Experiment 2
welfare.guess.all.qualtrics <- lm(welfare.outcome ~ welfare.treatment*guess_first_exp,data=welfare.second.qualtrics)
names(welfare.guess.all.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

welfare.guess.baseline.qualtrics <- lm(welfare.outcome ~ welfare.treatment*guess_first_exp,data=welfare.second.qualtrics,subset=which(demand.condition.welfare=='Baseline'))
names(welfare.guess.baseline.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

#Framing Experiment 2
framing.guess.all.qualtrics <- lm(framing.outcome ~ speech.treatment*guess_first_exp,data=framing.second.qualtrics)
names(framing.guess.all.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

framing.guess.baseline.qualtrics <- lm(framing.outcome ~ speech.treatment*guess_first_exp,data=framing.second.qualtrics,subset=which(demand.condition.frame=='Baseline'))
names(framing.guess.baseline.qualtrics$coefficients) <- c('(Intercept)','Treatment','Guess First','Treatment*Guess First')

########
########
###TABLE B3 HERE
########
########
apsrtable(frame.guess.all,news1.guess.all,resume.guess.all,news2.guess.all,peace.guess.all,welfare.guess.all,peace.guess.all.qualtrics,welfare.guess.all.qualtrics,framing.guess.all.qualtrics,model.names=c('Framing 1','News 1','Resume','News 2','Dem Peace 1','Welfare 1','Dem Peace 2','Welfare 2','Framing 2'),caption="Treatment Effects Conditional on Correct Guess - All")

########
########
###TABLE B4 HERE
########
########
apsrtable(frame.guess.baseline,news1.guess.baseline,resume.guess.baseline,news2.guess.baseline,peace.guess.baseline,welfare.guess.baseline,peace.guess.baseline.qualtrics,welfare.guess.baseline.qualtrics,framing.guess.baseline.qualtrics,model.names=c('Framing 1','News 1','Resume','News 2','Dem Peace 1','Welfare 1','Dem Peace 2','Welfare 2','Framing 2'),caption='Treatment Effects Conditional on Correct Guess - Baseline')

########
###POOL RESULTS - All Respondents
########
#Function for Cluster Robust SEs
fit.w.robust <- function(model, cluster.var, dta){	
	robust.se <- function(model, cluster){
		require(sandwich)
		require(lmtest)
		M <- length(unique(cluster))
		N <- length(cluster)
		K <- model$rank
		dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
		uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
		rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
		rcse.se <- coeftest(model, rcse.cov)
		return(list(rcse.cov, rcse.se))
	}
	
	out <- model$model
	clustervar <- mapply(paste,cluster.var,dta[!(1:dim(dta)[1] %in% model$na.action),cluster.var],sep="")
	vcov <- robust.se(model, clustervar)
	model$se <- vcov[[1]]
	model$coeftest <- vcov[[2]]
	return(model)
}

#All Demand Type Conditions Versus Baseline
pooled.model.all <- lm(outcome ~ experimental.treatment*all.demand.conditions + factor(study),data=combined.frame.all)
pooled.model.all <- fit.w.robust(pooled.model.all,'study',combined.frame.all)

pooled.model.all.mturk <- lm(outcome ~ experimental.treatment*all.demand.conditions + factor(study),data=combined.frame.all[which(combined.frame.all$sample.type=='mturk'),])
pooled.model.all.mturk <- fit.w.robust(pooled.model.all.mturk,'study',combined.frame.all[which(combined.frame.all$sample.type=='mturk'),])

pooled.model.all.qualtrics <- lm(outcome ~ experimental.treatment*all.demand.conditions + factor(study),data=combined.frame.all[which(combined.frame.all$sample.type=='qualtrics'),])
pooled.model.all.qualtrics <- fit.w.robust(pooled.model.all.qualtrics,'study',combined.frame.all[which(combined.frame.all$sample.type=='qualtrics'),])

#Separate Out Baseline/Information/Incentive+Information Conditions
pooled.model.treatmenttype <- lm(outcome ~ experimental.treatment*demand.by.type + factor(study),data=combined.frame.all[which(combined.frame.all$study %in% c('Study3-Peace','Study3-Welfare','Study4-Peace','Study4-Welfare','Study4-News','Study4-Framing')),])
pooled.model.treatmenttype <- fit.w.robust(pooled.model.treatmenttype,'study',combined.frame.all[which(combined.frame.all$study %in% c('Study3-Peace','Study3-Welfare','Study4-Peace','Study4-Welfare','Study4-News','Study4-Framing')),])

pooled.model.treatmenttype.mturk <- lm(outcome ~ experimental.treatment*demand.by.type + factor(study),data=combined.frame.all[which(combined.frame.all$study %in% c('Study3-Peace','Study3-Welfare')),])
pooled.model.treatmenttype.mturk <- fit.w.robust(pooled.model.treatmenttype.mturk,'study',combined.frame.all[which(combined.frame.all$study %in% c('Study3-Peace','Study3-Welfare')),])

pooled.model.treatmenttype.qualtrics <- lm(outcome ~ experimental.treatment*demand.by.type + factor(study),data=combined.frame.all[which(combined.frame.all$study %in% c('Study4-Peace','Study4-Welfare','Study4-News','Study4-Framing')),])
pooled.model.treatmenttype.qualtrics <- fit.w.robust(pooled.model.treatmenttype.qualtrics,'study',combined.frame.all[which(combined.frame.all$study %in% c('Study4-Peace','Study4-Welfare','Study4-News','Study4-Framing')),])

########
########
###TABLE B5 HERE
########
########
apsrtable(pooled.model.all,pooled.model.all.mturk,pooled.model.all.qualtrics,model.names=c('All','Mturk Studies','Qualtrics Studies'),caption=c('Pooled Estimates of Treatment Effect Variation by Demand Condition'),omitcoef=expression(grep(pattern="study",coefnames)),coef.names=c('(Intercept)','Treatment','Demand Condition','Treatment*Demand Condition'))

########
########
###TABLE B6 HERE
########
########
apsrtable(pooled.model.treatmenttype,pooled.model.treatmenttype.mturk,pooled.model.treatmenttype.qualtrics,model.names=c('All','Mturk Studies','Qualtrics Studies'),caption=c('Pooled Estimates of Treatment Effect Variation by Demand Condition'),omitcoef=expression(grep(pattern="study",coefnames)),coef.names=c('(Intercept)','Treatment','Demand Condition-Information','Demand Condition-Incentive','Treatment*Demand Condition-Information','Treatment*Demand Condition-Incentive'))

########
###POOL RESULTS - Guessing Analysis
########
#All Demand Type Conditions Versus Basline
pooled.model.guess <- lm(outcome ~ experimental.treatment*correct.guess + factor(study),data=combined.frame.guess)
pooled.model.guess <- fit.w.robust(pooled.model.guess,'study',combined.frame.guess)

#Separate Out Baseline/Information/Incentive+Information Conditions
pooled.model.guess.control <- lm(outcome ~ experimental.treatment*correct.guess + factor(study),data=combined.frame.guess[which(combined.frame.guess$demand.treatment=='Baseline'),])
pooled.model.guess.control <- fit.w.robust(pooled.model.guess.control,'study',combined.frame.guess[which(combined.frame.guess$demand.treatment=='Baseline'),])

########
########
###TABLE 4 HERE
########
########
apsrtable(pooled.model.guess.control,pooled.model.guess,model.names=c('No Demand Information','All Conditions'),caption=c('Pooled Estimates of Treatment Effect Variation by Correct Guess of Prior Exp. Intent'),omitcoef=expression(grep(pattern="study",coefnames)),coef.names=c('(Intercept)','Treatment','Correct Guess','Treatment*Correct Guess'))

########
###Balance Checks - Relative to baseline condition
########
frame1.cells <-expand.grid(unique(na.omit(framing$frame)), unique(na.omit(framing$frame_treat)))
colnames(frame1.cells) <- c("exp.treatment","demand.treatment")
frame1.cells$exp<-"frame1"

news1.cells <- expand.grid(unique(na.omit(framing$source1)), unique(na.omit(framing$news_treat)))
colnames(news1.cells) <- c("exp.treatment","demand.treatment")
news1.cells$exp<-"news1"

news2.cells <- expand.grid(unique(na.omit(resume$source1)), unique(na.omit(resume$news_treat)))
colnames(news2.cells) <- c("exp.treatment","demand.treatment")
news2.cells$exp<-"news2"

resume.cells <- expand.grid(unique(na.omit(resume$name_treat)), unique(na.omit(resume$resume_treat)))
colnames(resume.cells) <- c("exp.treatment","demand.treatment")
resume.cells$exp<-"resume"

dempeace1.cells <- expand.grid(unique(na.omit(incentive$dem.treatment)), unique(na.omit(incentive$ir.treatment)))
colnames(dempeace1.cells) <- c("exp.treatment","demand.treatment")
dempeace1.cells$exp <- 'dempeace1'

welfare1.cells <- expand.grid(unique(na.omit(incentive$lazy.treatment)), unique(na.omit(incentive$cp.treatment)))
colnames(welfare1.cells) <- c("exp.treatment","demand.treatment")
welfare1.cells$exp <- 'welfare1'

news3.cells <- expand.grid(unique(na.omit(qualtrics.news.guess$source1)), unique(na.omit(qualtrics.news.guess$demand.condition)))
colnames(news3.cells) <- c("exp.treatment","demand.treatment")
news3.cells$exp <- 'news3'

framing2.cells <- expand.grid(unique(na.omit(qualtrics.other$speech.treatment)),unique(na.omit(qualtrics.other$demand.condition.frame)))
colnames(framing2.cells) <- c("exp.treatment","demand.treatment")
framing2.cells$exp <- 'framing2'

dempeace2.cells <- expand.grid(unique(na.omit(qualtrics.other$dem.treatment)),unique(na.omit(qualtrics.other$demand.condition.peace)))
colnames(dempeace2.cells) <- c("exp.treatment","demand.treatment")
dempeace2.cells$exp <- 'dempeace2'

welfare2.cells <- expand.grid(unique(na.omit(qualtrics.other$welfare.treatment)),unique(na.omit(qualtrics.other$demand.condition.welfare)))
colnames(welfare2.cells) <- c("exp.treatment","demand.treatment")
welfare2.cells$exp <- 'welfare2'


#Study 1 Framing Loop
#Exclude gender because not collected for this study
frame1.cells$fstat <- NA
frame1.cells$pvalue <- NA
for(k in 1:dim(frame1.cells)[1]){
	current.treatment <- frame1.cells$exp.treatment[k]
	current.demand.treatment <- frame1.cells$demand.treatment[k]
	current.model <- lm( I(frame==current.treatment & frame_treat==current.demand.treatment) ~ non.white + college + income + age + democrat + republican, data=framing)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	frame1.cells$fstat[k] <- current.fstat
	frame1.cells$pvalue[k] <- current.pvalue
}

#Study 1 Partisan News Loop
news1.cells$fstat <- NA
news1.cells$pvalue <- NA
for(k in 1:dim(news1.cells)[1]){
	current.treatment <- news1.cells$exp.treatment[k]
	current.demand.treatment <- news1.cells$demand.treatment[k]
	current.model <- lm( I(source1==current.treatment & news_treat==current.demand.treatment) ~ non.white + college + income + age + democrat + republican, data=framing)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	news1.cells$fstat[k] <- current.fstat
	news1.cells$pvalue[k] <- current.pvalue
}

#Study 2
#Exclude gender because not collected for this study
news2.cells$fstat <- NA
news2.cells$pvalue <- NA
for(k in 1:dim(news2.cells)[1]){
	current.treatment <- news2.cells$exp.treatment[k]
	current.demand.treatment <- news2.cells$demand.treatment[k]
	current.model <- lm( I(source1==current.treatment & news_treat==current.demand.treatment) ~ non.white + college + income + age + democrat + republican, data=resume)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	news2.cells$fstat[k] <- current.fstat
	news2.cells$pvalue[k] <- current.pvalue
}

resume.cells$fstat <- NA
resume.cells$pvalue <- NA
for(k in 1:dim(resume.cells)[1]){
	current.treatment <- resume.cells$exp.treatment[k]
	current.demand.treatment <- resume.cells$demand.treatment[k]
	current.model <- lm( I(name_treat==current.treatment & resume_treat==current.demand.treatment) ~ non.white + college + income + age + democrat + republican, data=resume)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	resume.cells$fstat[k] <- current.fstat
	resume.cells$pvalue[k] <- current.pvalue
}

#Study 3
dempeace1.cells$fstat <- NA
dempeace1.cells$pvalue <- NA
for(k in 1:dim(dempeace1.cells)[1]){
	current.treatment <- dempeace1.cells$exp.treatment[k]
	current.demand.treatment <- dempeace1.cells$demand.treatment[k]
	current.model <- lm( I(dem.treatment==current.treatment & ir.treatment==current.demand.treatment) ~ non.white + college + income + age + democrat + republican + male, data=incentive)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	dempeace1.cells$fstat[k] <- current.fstat
	dempeace1.cells$pvalue[k] <- current.pvalue
}

welfare1.cells$fstat <- NA
welfare1.cells$pvalue <- NA
for(k in 1:dim(welfare1.cells)[1]){
	current.treatment <- welfare1.cells$exp.treatment[k]
	current.demand.treatment <- welfare1.cells$demand.treatment[k]
	current.model <- lm( I(lazy.treatment==current.treatment & cp.treatment==current.demand.treatment) ~ non.white + college + income + age + democrat + republican + male, data=incentive)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	welfare1.cells$fstat[k] <- current.fstat
	welfare1.cells$pvalue[k] <- current.pvalue
}

#Study 4
#Exclude one partisanship category because no independents in the study
news3.cells$fstat <- NA
news3.cells$pvalue <- NA
for(k in 1:dim(news3.cells)[1]){
	current.treatment <- news3.cells$exp.treatment[k]
	current.demand.treatment <- news3.cells$demand.treatment[k]
	current.model <- lm( I(source1==current.treatment & demand.condition==current.demand.treatment) ~ non.white + college + income + age + democrat + male, data=qualtrics.news.guess)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	news3.cells$fstat[k] <- current.fstat
	news3.cells$pvalue[k] <- current.pvalue
}

#Study 5
framing2.cells$fstat <- NA
framing2.cells$pvalue <- NA
for(k in 1:dim(framing2.cells)[1]){
	current.treatment <- framing2.cells$exp.treatment[k]
	current.demand.treatment <- framing2.cells$demand.treatment[k]
	current.model <- lm( I(speech.treatment==current.treatment & demand.condition.frame==current.demand.treatment) ~ non.white + college + income + age + democrat + republican + male, data=qualtrics.other)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	framing2.cells$fstat[k] <- current.fstat
	framing2.cells$pvalue[k] <- current.pvalue
}

dempeace2.cells$fstat <- NA
dempeace2.cells$pvalue <- NA
for(k in 1:dim(dempeace2.cells)[1]){
	current.treatment <- dempeace2.cells$exp.treatment[k]
	current.demand.treatment <- dempeace2.cells$demand.treatment[k]
	current.model <- lm( I(dem.treatment==current.treatment & demand.condition.peace==current.demand.treatment) ~ non.white + college + income + age + democrat + republican + male, data=qualtrics.other)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	dempeace2.cells$fstat[k] <- current.fstat
	dempeace2.cells$pvalue[k] <- current.pvalue
}

welfare2.cells$fstat <- NA
welfare2.cells$pvalue <- NA
for(k in 1:dim(welfare2.cells)[1]){
	current.treatment <- welfare2.cells$exp.treatment[k]
	current.demand.treatment <- welfare2.cells$demand.treatment[k]
	current.model <- lm( I(welfare.treatment==current.treatment & demand.condition.welfare==current.demand.treatment) ~ non.white + college + income + age + democrat + republican + male, data=qualtrics.other)
	current.fstat <- summary(current.model)$fstatistic[1]
	current.pvalue <- pf(summary(current.model)$fstatistic[1],summary(current.model)$fstatistic[2],summary(current.model)$fstatistic[2],lower.tail=F)
	welfare2.cells$fstat[k] <- current.fstat
	welfare2.cells$pvalue[k] <- current.pvalue
}

#Combine Frames and Plot
combinedftest <- rbind(news1.cells,frame1.cells,news2.cells,resume.cells,dempeace1.cells,welfare1.cells,news3.cells,framing2.cells,dempeace2.cells,welfare2.cells)

########
########
###FIGURE B2 HERE
########
########
pdf("balance_hist.pdf",height=6, width=6)
hist(combinedftest$pvalue, breaks=25, freq=T, axes=F, xlab="p-value", col=c("grey"), main="Distribution of p-values from Balance Tests", xlim=c(0,1))
axis(1, at=seq(0,1,.05), cex.axis=.7)
axis(2, at=seq(0,10,1), las=2)
dev.off()

####################
####Demographic Tables by Study
####################
study1.demos <- framing[,c('black','white','hispanic','other.race','college','not.college','female','male','age','income','democrat','republican','independent')]
study1.demos$study <- 'study1'

study2.demos <- resume[,c('black','white','hispanic','other.race','college','not.college','female','male','age','income','democrat','republican','independent')]
study2.demos$study <- 'study2'

study3.demos <- incentive[,c('black','white','hispanic','other.race','college','not.college','female','male','age','income','democrat','republican','independent')]
study3.demos$study <- 'study3'

study4.demos <- qualtrics.news.guess[,c('black','white','hispanic','other.race','college','not.college','female','male','age','income','democrat','republican','independent')]
study4.demos$study <- 'study4'

study5.demos <- qualtrics.other[,c('black','white','hispanic','other.race','college','not.college','female','male','age','income','democrat','republican','independent')]
study5.demos$study <- 'study5'

combined.demos <- rbind(study1.demos,study2.demos,study3.demos,study4.demos,study5.demos)

demo.frame <- ddply(combined.demos,.(study),summarise,black=mean(black,na.rm=TRUE),hispanic=mean(hispanic,na.rm=TRUE),white=mean(white,na.rm=TRUE),other.race=mean(other.race,na.rm=TRUE),college=mean(college,na.rm=TRUE),no.college=mean(not.college,na.rm=TRUE),female=mean(female,na.rm=TRUE),male=mean(male,na.rm=TRUE),age=mean(age,na.rm=TRUE),income=mean(income,na.rm=TRUE),democrat=mean(democrat,na.rm=TRUE),republican=mean(republican,na.rm=TRUE),independent=mean(independent,na.rm=TRUE),sample.size=length(study))
demo.frame <- as.data.frame(t(demo.frame))

########
########
###TABLE B1 HERE
########
########
xtable(demo.frame,digits=2)


####################
####Implications of Guessing for TESS Studies
####################

load(file="tess_data.Rdata")

# plot results
addest <- function(x, n, which = c(2,5), pos = c(0.3, -0.3), col = "gray", pch = 23) {
    e <- x[[n]][which,"Effect"]
     if(e[1] < 0) {
         e[1] <- (-1 * e[1])
     }
     if(e[2] < 0) {
         e[2] <- (-1 * e[2])
     }
     e<-e/6
    se <- x[[n]][which,"SE"]/6
    col<-x[[n]][which,"color"]
    n <- n*2
    n <- n + pos
    points(e*100, n, col = col, bg = col, pch = pch, cex = 0.75)
   # segments(e*100 - se*100, n, e*100 + se*100, n, lwd = 2, col = col)
    segments(e*100 - 2 * se*100, n, e*100 + 2 * se*100, n, lwd = 1, col = col)
    print(e)
}

# Study 2, Treatment Group 1 (TESS Weighted, MTurk Unweighted)
s2t1 <- abs(sapply(out1, `[`, 2, "Effect"))
s2t1order <- order(s2t1)
#png(filename = "ResultsTreatmentGroup1.png", height = 6, width = 7.5, units = "in", res = 300)



out1_new<-out1
for(i in 1:length(out1_new)){
	
	out1[[i]]<-as.data.frame(out1[[i]])
	out1_new[[i]]<-as.data.frame(out1_new[[i]])
	
	for(j in 1:nrow(out1_new[[i]])){
		
	if(out1[[i]][j,"Effect"]>0){
	out1_new[[i]][j,"Effect"]<-out1[[i]][j,"Effect"]-.03
	}
	
		if(out1[[i]][j,"Effect"]<0){
	out1_new[[i]][j,"Effect"]<-out1[[i]][j,"Effect"]+.03
	}
		
	}
	
	out1[[i]][,"lb"]<-NA
	out1[[i]][,"ub"]<-NA
	out1[[i]][,"sig_change"]<-NA
	out1[[i]][,"pos_change"]<-NA
	out1[[i]][,"lb"]<-out1[[i]][,"Effect"]-2*out1[[i]][,"SE"]
	out1[[i]][,"ub"]<-out1[[i]][,"Effect"]+2*out1[[i]][,"SE"]

	out1_new[[i]][,"lb"]<-NA
	out1_new[[i]][,"ub"]<-NA
    out1_new[[i]][,"sig_pre"]<-I( (out1[[i]][,"lb"]*out1[[i]][,"ub"])>0 )
    out1_new[[i]][,"pos_pre"]<-I( out1[[i]][,"Effect"]>0)
  
    out1_new[[i]][,"sig_post"]<-NA
    out1_new[[i]][,"sig_change"]<-NA
    out1_new[[i]][,"pos_change"]<-NA

	out1_new[[i]][,"lb"]<-out1_new[[i]][,"Effect"]-2*out1_new[[i]][,"SE"]
	out1_new[[i]][,"ub"]<-out1_new[[i]][,"Effect"]+2*out1_new[[i]][,"SE"]
    out1_new[[i]][,"sig_post"]<-I( (out1_new[[i]][,"lb"]*out1_new[[i]][,"ub"])>0 )
    out1_new[[i]][,"pos_post"]<-I( out1_new[[i]][,"Effect"]>0)


    out1[[i]][,"sig_change"]<-I( out1_new[[i]][,"sig_post"]!=out1_new[[i]][,"sig_pre"] )
    out1[[i]][,"pos_change"]<-I( out1_new[[i]][,"pos_post"]!=out1_new[[i]][,"pos_pre"]   )	
   
    out1[[i]][,"color"]<-"gray"
    out1[[i]][out1[[i]]$pos_change==T,"color"]<-"red"
    out1[[i]][out1[[i]]$sig_change==T,"color"]<-"black"
    
    out1_new[[i]][,"sig_change"]<-I( out1_new[[i]][,"sig_post"]!=out1_new[[i]][,"sig_pre"] )
    out1_new[[i]][,"pos_change"]<-I( out1_new[[i]][,"pos_post"]!=out1_new[[i]][,"pos_pre"]   )	
    
    out1_new[[i]][,"color"]<-"gray"
    out1_new[[i]][out1_new[[i]]$pos_change==T,"color"]<-"red"
    out1_new[[i]][out1_new[[i]]$sig_change==T,"color"]<-"black"
	
}

########
########
###FIGURE B8 HERE
########
########
pdf(file="tess.pdf")
plot(NA, xlim = c(-.05, .25)*100, ylim = c(0, (2*length(s2t1order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect\n(Percentage Points, Absolute Value)", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = c("change in stat. significance", "sign change"), col = c("black", "red"), bty = "n", pch=18, lty=1)
axis(2, seq_along(s2t1order)*2, s2t1order, las = 2)
invisible(sapply(seq_along(s2t1order), addest, x = out1_new[s2t1order], which = c(2,3)))
dev.off()


