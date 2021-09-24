# Script to assess agreement between paired NLP and tabular values by year

# Depends
library(data.table)

# Define function
agree_year <- function(dataset){
  n <- 1; out <- list()
  for (year_iter in unique(dataset$year)){
    dt <- as.data.table(dataset[year==year_iter])
    dt <- dt[,.SD[which.max(date)],by='id']
    if (nrow(dt) < 10){next}
    pearson <- cor.test(x=dt$tabular,y=dt$nlp)
    corr <- pearson$estimate[1]
    corr_lower <- pearson$conf.int[1]
    corr_upper <- pearson$conf.int[2]
    mean <- mean(dt$tabular - dt$nlp)
    lower <- mean-1.96*sd(dt$tabular - dt$nlp)
    upper <- mean+1.96*sd(dt$tabular - dt$nlp)
    out[[n]] <- data.table(year=year_iter,corr=corr,corr_lower=corr_lower,corr_upper=corr_upper,
                           mean_diff=mean,lower_diff=lower,upper_diff=upper)
    n <- n + 1
  }
  output <- data.table(do.call(rbind,out))
  names(output) <- c('year','corr','corr_lower','corr_upper','mean_diff','lower_diff','upper_diff')
  setkey(output,year)
  return(output)
}

# Load datasets containing paired values
nlp_tab_dbp <- fread(file='diastolic.tsv')
nlp_tab_sbp <- fread(file='systolic.tsv')
nlp_tab_ht <- fread(file='height.tsv')
nlp_tab_wt <- fread(file='weight.tsv')

out_dbp <- agree_year(nlp_tab_dbp)
out_sbp <- agree_year(nlp_tab_sbp)
out_ht <- agree_year(nlp_tab_ht)
out_wt <- agree_year(nlp_tab_wt)

# DBP PLOT
## CORR
pdf(file='dbp_time_corr.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_dbp$year)
y <- out_dbp$corr

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#8856a7',
     xaxt='n',xlim=c(0.5,length(out_dbp$year)+0.5),ylim=c(0,1),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_dbp$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,1,0.1),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Pearson Correlation",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 0.03,
     labels = paste0(unique(out_dbp$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_dbp$corr_lower,x,out_dbp$corr_upper,col='#8856a7',lwd=1)

dev.off()

## AGREE
pdf(file='dbp_time_diff.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_dbp$year)
y <- out_dbp$mean_diff

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#8856a7',
     xaxt='n',xlim=c(0.5,length(out_dbp$year)+0.5),ylim=c(-20,20),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_dbp$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(-20,20,5),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Mean Difference (mmHg)",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 1.2,
     labels = paste0(unique(out_dbp$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_dbp$lower_diff,x,out_dbp$upper_diff,col='#8856a7',lwd=1)
segments(0,0,length(out_dbp$year)+0.5,0,lty=5)

dev.off()

# SBP PLOT
## CORR
pdf(file='sbp_time_corr.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_sbp$year)
y <- out_sbp$corr

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2ca25f',
     xaxt='n',xlim=c(0.5,length(out_sbp$year)+0.5),ylim=c(0,1),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_sbp$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,1,0.1),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Pearson Correlation",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 0.03,
     labels = paste0(unique(out_sbp$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_sbp$corr_lower,x,out_sbp$corr_upper,col='#2ca25f',lwd=1)

dev.off()

## AGREE
pdf(file='sbp_time_diff.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_sbp$year)
y <- out_sbp$mean_diff

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2ca25f',
     xaxt='n',xlim=c(0.5,length(out_sbp$year)+0.5),ylim=c(-20,20),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_sbp$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(-20,20,5),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Mean Difference (mmHg)",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 1.2,
     labels = paste0(unique(out_sbp$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_sbp$lower_diff,x,out_sbp$upper_diff,col='#2ca25f',lwd=1)
segments(0,0,length(out_sbp$year)+0.5,0,lty=5)

dev.off()

# HT PLOT
## CORR
pdf(file='ht_time_corr.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_ht$year)
y <- out_ht$corr

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2b8cbe',
     xaxt='n',xlim=c(0.5,length(out_ht$year)+0.5),ylim=c(0,1),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_ht$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,1,0.1),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Pearson Correlation",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 0.03,
     labels = paste0(unique(out_ht$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_ht$corr_lower,x,out_ht$corr_upper,col='#2b8cbe',lwd=1)

dev.off()

## AGREE
pdf(file='ht_time_diff.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_ht$year)
y <- out_ht$mean_diff

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2b8cbe',
     xaxt='n',xlim=c(0.5,length(out_ht$year)+0.5),ylim=c(-20,20),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_ht$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(-20,20,5),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Mean Difference (cm)",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 1.2,
     labels = paste0(unique(out_ht$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_ht$lower_diff,x,out_ht$upper_diff,col='#2b8cbe',lwd=1)
segments(0,0,length(out_ht$year)+0.5,0,lty=5)

dev.off()

# WT PLOT
## CORR
pdf(file='wt_time_corr.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_wt$year)
y <- out_wt$corr

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#f03b20',
     xaxt='n',xlim=c(0.5,length(out_wt$year)+0.5),ylim=c(0,1),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_wt$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,1,0.1),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Pearson Correlation",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 0.03,
     labels = paste0(unique(out_wt$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_wt$corr_lower,x,out_wt$corr_upper,col='#f03b20',lwd=1)

dev.off()

## AGREE
pdf(file='wt_time_diff.pdf',height=3,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- 1:length(out_wt$year)
y <- out_wt$mean_diff

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#f03b20',
     xaxt='n',xlim=c(0.5,length(out_wt$year)+0.5),ylim=c(-20,20),pch=19,cex=1.2,bty='n')

axis(1,at=1:length(out_wt$year),labels=rep(NA,times=length(x)),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(-20,20,5),las=1)

mtext("Year of Extraction",side=1,cex=1.8,line=3.5)
mtext("Mean Difference (kg)",side=2,cex=1.8,line=4.5)

text(x = x,
     y = par("usr")[3] - 1.2,
     labels = paste0(unique(out_wt$year)),
     xpd = NA,
     srt = -45,
     adj = 0,
     cex = 1.3)

segments(x,out_wt$lower_diff,x,out_wt$upper_diff,col='#f03b20',lwd=1)
segments(0,0,length(out_wt$year)+0.5,0,lty=5)

dev.off()




