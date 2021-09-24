# Script to perform analyses comparing NLP and tabular BP data

# Depends
library(data.table)
library(plyr)
library(sktools)
library(ggplot2)

# Load
closest <- fread(file='closest_pair.tsv')
same <- fread(file='same_pair.tsv')

# Format dates
for (j in c('age','start_fu','tab_age','start_fu_before','tab_diff')){
  set(closest,j=j,value=as.numeric(str_extract(closest[[j]],'\\d+')))
  set(same,j=j,value=as.numeric(str_extract(same[[j]],'\\d+')))
}

# Apply standard filters for both
closest <- closest[systolic >= 50 & systolic <= 300]
same <- same[systolic >= 50 & systolic <= 300]

closest <- closest[diastolic >= 20 & diastolic <= 200]
same <- same[diastolic >= 20 & diastolic <= 200]

################ PART 1: DISTRIBUTIONS
################ CLOSEST
##### SBP
x <- list(v1=closest$systolic,v2=closest$nlp_systolic)
data <- melt(x)

# Binwidth using sturge's rule
width <- round((max(closest$systolic) - min(closest$systolic))/(1 + 3.322*log(nrow(closest))),0)

# Plot distribution
ggplot() + geom_histogram(data=data,aes(x=value,fill=L1),position='identity',color='black',alpha=0.55,binwidth = width) +
  scale_x_continuous(breaks=seq(75,200,25),expand=c(0.02,0),limits=c(75,200)) +
  scale_y_continuous(breaks=seq(0,45000,5000),expand=c(0,0),limits=c(0,45000)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Tabular','NLP')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.90,0.90),
        axis.text=element_text(size=30,color='black'),plot.margin=unit(c(1,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=30,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=30),legend.text=element_text(size=25)) +
  labs(x='Systolic blood pressure (mmHg)',y='Frequency')
ggsave(filename='tab_nlp_closest_sbp.pdf',height=2,width=2.5,
       scale=4,device='pdf')

##### DBP
x <- list(v1=closest$diastolic,v2=closest$nlp_diastolic)
data <- melt(x)

# Binwidth using sturge's rule
width <- round((max(closest$diastolic) - min(closest$diastolic))/(1 + 3.322*log(nrow(closest))),0)

# Plot distribution
ggplot() + geom_histogram(data=data,aes(x=value,fill=L1),position='identity',color='black',alpha=0.55,binwidth = width) +
  scale_x_continuous(breaks=seq(40,120,10),expand=c(0.02,0),limits=c(40,120)) +
  scale_y_continuous(breaks=seq(0,45000,5000),expand=c(0,0),limits=c(0,45000)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Tabular','NLP')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.90,0.90),
        axis.text=element_text(size=30,color='black'),plot.margin=unit(c(1,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=30,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=30),legend.text=element_text(size=25)) +
  labs(x='Diastolic blood pressure (mmHg)',y='Frequency')
ggsave(filename='ab_nlp_closest_dbp.pdf',height=2,width=2.5,
       scale=4,device='pdf')

################ SAME
### SBP
x <- list(v1=same$systolic,v2=same$nlp_systolic)
data <- melt(x)

# Binwidth using sturge's rule
width <- round((max(same$systolic) - min(same$systolic))/(1 + 3.322*log(nrow(same))),0)

# Plot distribution
ggplot() + geom_histogram(data=data,aes(x=value,fill=L1),position='identity',color='black',alpha=0.55,binwidth = width) +
  scale_x_continuous(breaks=seq(75,200,25),expand=c(0.02,0),limits=c(75,200)) +
  scale_y_continuous(breaks=seq(0,25000,5000),expand=c(0,0),limits=c(0,25000)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Tabular','NLP')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.90,0.90),
        axis.text=element_text(size=30,color='black'),plot.margin=unit(c(1,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=30,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=30),legend.text=element_text(size=25)) +
  labs(x='Systolic blood pressure (mmHg)',y='Frequency')
ggsave(filename='tab_nlp_same_sbp.pdf',height=2,width=2.5,
       scale=4,device='pdf')

##### DBP
x <- list(v1=same$diastolic,v2=same$nlp_diastolic)
data <- melt(x)

# Binwidth using sturge's rule
width <- round((max(same$diastolic) - min(same$diastolic))/(1 + 3.322*log(nrow(same))),0)

# Plot distribution
ggplot() + geom_histogram(data=data,aes(x=value,fill=L1),position='identity',color='black',alpha=0.55,binwidth = width) +
  scale_x_continuous(breaks=seq(40,120,10),expand=c(0.02,0),limits=c(40,120)) +
  scale_y_continuous(breaks=seq(0,30000,5000),expand=c(0,0),limits=c(0,30000)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Tabular','NLP')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.90,0.90),
        axis.text=element_text(size=30,color='black'),plot.margin=unit(c(1,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=30,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=30),legend.text=element_text(size=25)) +
  labs(x='Diastolic blood pressure (mmHg)',y='Frequency')
ggsave(filename='tab_nlp_same_dbp.pdf',height=2,width=2.5,
       scale=4,device='pdf')

################ PART 2: CORRELATIONS
################ CLOSEST
### SBP
raw <- cor.test(closest$systolic,y=closest$nlp_systolic)

png('corr_closest_sbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1),xpd=TRUE)
col <- "#2b8cbe40"

# Plot
plot(x=closest$systolic,y=closest$nlp_systolic,pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(25,300),ylim=c(25,300),cex=0.5)

# Identity line
segments(25,25,301,301,lty=5,lwd=1,col='black')

# Print correlation
text(270,60,paste0('Pearson r = ',round(raw$estimate,2)),cex=1.4)
text(270,45,paste0('p',ifelse(raw$p.value<0.01,' < 0.01',paste(' = ',round(raw$p.value,2)))),cex=1.4)

# Axes
axis(1,cex.axis=1.5,pos=25)
axis(2,cex.axis=1.5,las=2,pos=25)
mtext(side=1,'Tabular Systolic BP (mmHg)',cex=1.8,line=1.8)
mtext(side=2,'NLP Systolic BP (mmHg)',cex=1.8,line=2.8)

dev.off()

### DBP
raw <- cor.test(closest$diastolic,y=closest$nlp_diastolic)

png('corr_closest_dbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1),xpd=TRUE)
col <- "#f03b2040"

# Plot
plot(x=closest$diastolic,y=closest$nlp_diastolic,pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(0,200),ylim=c(0,200),cex=0.5)

# Identity line
segments(-1,-1,201,201,lty=5,lwd=1,col='black')

# Print correlation
text(170,30,paste0('Pearson r = ',round(raw$estimate,2)),cex=1.4)
text(170,20,paste0('p',ifelse(raw$p.value<0.01,' < 0.01',paste(' = ',round(raw$p.value,2)))),cex=1.4)

# Axes
axis(1,cex.axis=1.5,pos=0)
axis(2,cex.axis=1.5,las=2,pos=0)
mtext(side=1,'Tabular Diastolic BP (mmHg)',cex=1.8,line=1.8)
mtext(side=2,'NLP Diastolic BP (mmHg)',cex=1.8,line=2.8)

dev.off()

################ SAME
### SBP
raw <- cor.test(same$systolic,y=same$nlp_systolic)

png('corr_same_sbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1),xpd=TRUE)
col <- "#2b8cbe40"

# Plot
plot(x=same$systolic,y=same$nlp_systolic,pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(25,300),ylim=c(25,300),cex=0.5)

# Identity line
segments(25,25,301,301,lty=5,lwd=1,col='black')

# Print correlation
text(270,60,paste0('Pearson r = ',round(raw$estimate,2)),cex=1.4)
text(270,45,paste0('p',ifelse(raw$p.value<0.01,' < 0.01',paste(' = ',round(raw$p.value,2)))),cex=1.4)

# Axes
axis(1,cex.axis=1.5,pos=25)
axis(2,cex.axis=1.5,las=2,pos=25)
mtext(side=1,'Tabular Systolic BP (mmHg)',cex=1.8,line=1.8)
mtext(side=2,'NLP Systolic BP (mmHg)',cex=1.8,line=2.8)

dev.off()

### DBP
raw <- cor.test(same$diastolic,y=same$nlp_diastolic)

png('corr_same_dbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1),xpd=TRUE)
col <- "#f03b2040"

# Plot
plot(x=same$diastolic,y=same$nlp_diastolic,pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(0,200),ylim=c(0,200),cex=0.5)

# Identity line
segments(-1,-1,201,201,lty=5,lwd=1,col='black')

# Print correlation
text(170,30,paste0('Pearson r = ',round(raw$estimate,2)),cex=1.4)
text(170,20,paste0('p',ifelse(raw$p.value<0.01,' < 0.01',paste(' = ',round(raw$p.value,2)))),cex=1.4)

# Axes
axis(1,cex.axis=1.5,pos=0)
axis(2,cex.axis=1.5,las=2,pos=0)
mtext(side=1,'Tabular Diastolic BP (mmHg)',cex=1.8,line=1.8)
mtext(side=2,'NLP Diastolic BP (mmHg)',cex=1.8,line=2.8)

dev.off()

################ PART 3: AGREEMENT
################ CLOSEST
### SBP
closest[,mean_pair := apply(.SD,FUN=mean,MARGIN=1),.SDcols=c('systolic','nlp_systolic')]
closest[,diff_pair := systolic - nlp_systolic]
setkey(closest,mean_pair)

png('bland_altman_closest_sbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1))
col <- "#2b8cbe40"

# Plot
plot(x=closest$mean_pair,y=closest$diff_pair,
     pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(75,250),ylim=c(-200,200),cex=0.5)

# CI Lines
mean_diff <- mean(closest$diff_pair)
upper <- mean(closest$diff_pair)+1.96*sd(closest$diff_pair)
lower <- mean(closest$diff_pair)-1.96*sd(closest$diff_pair)
segments(70,upper,250,upper,lty=5,lwd=1,col='black')
segments(70,lower,250,lower,lty=5,lwd=1,col='black')
segments(70,mean_diff,250,mean_diff,lty=1,lwd=1,col='#f03b20')

# Axes
axis(1,cex.axis=1.5,pos=-210,at=seq(75,250,25))
axis(2,cex.axis=1.5,las=2,pos=70)
mtext(side=1,"Mean of Systolic BPs (mmHg)",cex=1.8,line=2.6)
mtext(side=2,"Difference in Systolic BPs (mmHg)",cex=1.8,line=3.5)

# Agreement
par(xpd=TRUE)
agree <- nrow(closest[c((diff_pair >= lower) & (diff_pair <= upper))])/nrow(closest)
text(x=130,y=185,labels=paste0("Limits of Agreement: ",round(lower,2),'-',round(upper,2)),cex=1.2)
text(x=130,y=172,labels=paste0("% Within Limits: ",round(agree*100,1)),cex=1.2)

dev.off()

### DBP
closest[,mean_pair := apply(.SD,FUN=mean,MARGIN=1),.SDcols=c('diastolic','nlp_diastolic')]
closest[,diff_pair := diastolic - nlp_diastolic]
setkey(closest,mean_pair)

png('bland_altman_closest_dbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1))
col <- "#f03b2040"

# Plot
plot(x=closest$mean_pair,y=closest$diff_pair,
     pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(20,160),ylim=c(-140,120),cex=0.5)

# CI Lines
mean_diff <- mean(closest$diff_pair)
upper <- mean(closest$diff_pair)+1.96*sd(closest$diff_pair)
lower <- mean(closest$diff_pair)-1.96*sd(closest$diff_pair)
segments(17,upper,160,upper,lty=5,lwd=1,col='black')
segments(17,lower,160,lower,lty=5,lwd=1,col='black')
segments(17,mean_diff,160,mean_diff,lty=1,lwd=1,col='#2b8cbe')

# Axes
axis(1,cex.axis=1.5,pos=-155,at=seq(20,160,20))
axis(2,cex.axis=1.5,las=2,at=seq(-140,120,20),pos=17)
mtext(side=1,"Mean of Diastolic BPs (mmHg)",cex=1.8,line=3.5)
mtext(side=2,"Difference in Diastolic BPs (mmHg)",cex=1.8,line=3.5)

# Agreement
par(xpd=TRUE)
agree <- nrow(closest[c((diff_pair >= lower) & (diff_pair <= upper))])/nrow(closest)
text(x=65,y=-120,labels=paste0("Limits of Agreement: ",round(lower,2),'-',round(upper,2)),cex=1.2)
text(x=65,y=-128,labels=paste0("% Within Limits: ",round(agree*100,1)),cex=1.2)

dev.off()

################ SAME
### SBP
same[,mean_pair := apply(.SD,FUN=mean,MARGIN=1),.SDcols=c('systolic','nlp_systolic')]
same[,diff_pair := systolic - nlp_systolic]
setkey(same,mean_pair)

png('bland_altman_same_sbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1))
col <- "#2b8cbe40"

# Plot
plot(x=same$mean_pair,y=same$diff_pair,
     pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(75,250),ylim=c(-150,125),cex=0.5)

# CI Lines
mean_diff <- mean(same$diff_pair)
upper <- mean(same$diff_pair)+1.96*sd(same$diff_pair)
lower <- mean(same$diff_pair)-1.96*sd(same$diff_pair)
segments(70,upper,250,upper,lty=5,lwd=1,col='black')
segments(70,lower,250,lower,lty=5,lwd=1,col='black')
segments(70,mean_diff,250,mean_diff,lty=1,lwd=1,col='#f03b20')

# Axes
axis(1,cex.axis=1.5,pos=-160,at=seq(75,250,25))
axis(2,cex.axis=1.5,las=2,pos=70,at=seq(-150,125,25))
mtext(side=1,"Mean of Systolic BPs (mmHg)",cex=1.8,line=2.6)
mtext(side=2,"Difference in Systolic BPs (mmHg)",cex=1.8,line=3.5)

# Agreement
par(xpd=TRUE)
agree <- nrow(same[c((diff_pair >= lower) & (diff_pair <= upper))])/nrow(same)
text(x=120,y=115,labels=paste0("Limits of Agreement: ",round(lower,2),'-',round(upper,2)),cex=1.2)
text(x=120,y=105,labels=paste0("% Within Limits: ",round(agree*100,1)),cex=1.2)

dev.off()

### DBP
same[,mean_pair := apply(.SD,FUN=mean,MARGIN=1),.SDcols=c('diastolic','nlp_diastolic')]
same[,diff_pair := diastolic - nlp_diastolic]
setkey(same,mean_pair)

png('bland_altman_same_dbp.png',pointsize=6,res=300,
    height=1200,width=1200)
par(oma=c(1,1,1,1))
par(mar=c(5,5,2,1))
col <- "#f03b2040"

# Plot
plot(x=same$mean_pair,y=same$diff_pair,
     pch=19,col=col,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,
     xlim=c(20,160),ylim=c(-100,125),cex=0.5)

# CI Lines
mean_diff <- mean(same$diff_pair)
upper <- mean(same$diff_pair)+1.96*sd(same$diff_pair)
lower <- mean(same$diff_pair)-1.96*sd(same$diff_pair)
segments(17,upper,160,upper,lty=5,lwd=1,col='black')
segments(17,lower,160,lower,lty=5,lwd=1,col='black')
segments(17,mean_diff,160,mean_diff,lty=1,lwd=1,col='#2b8cbe')

# Axes
axis(1,cex.axis=1.5,pos=-110,at=seq(20,160,20))
axis(2,cex.axis=1.5,las=2,pos=17,at=seq(-100,125,25))
mtext(side=1,"Mean of Diastolic BPs (mmHg)",cex=1.8,line=3.5)
mtext(side=2,"Difference in Diastolic BPs (mmHg)",cex=1.8,line=3.5)

# Agreement
par(xpd=TRUE)
agree <- nrow(same[c((diff_pair >= lower) & (diff_pair <= upper))])/nrow(same)
text(x=60,y=-85,labels=paste0("Limits of Agreement: ",round(lower,2),'-',round(upper,2)),cex=1.2)
text(x=60,y=-92,labels=paste0("% Within Limits: ",round(agree*100,1)),cex=1.2)

dev.off()

     
