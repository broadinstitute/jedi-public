#################
library(survival)
library(rms)
library(pec)
library(polspline)

# Source helper functions
source(file='~/functions/functions.R')

# Load datasets
load(file='charge.RData') # C3PO CHARGE set
load(file='pce.RData') # C3PO PCE set
load(file='cc_pce.RData') # Convenience PCE set
load(file='cc_charge.RData') # Convenience CHARGE set

###################################### Step 1: Calculate predicted probabilities
### A. C3PO AF
charge_45$charge_decile <- classifier(risk=charge_45$charge_pred5,ncuts=10)

setDF(charge_45)
charge_obv <- survivor(data=charge_45,risk_data="charge_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45)

charge_pred <- charge_45[,mean(charge_pred5),by="charge_decile"][order(charge_decile)]

### B. C3PO CC
cc_charge_45$charge_decile <- classifier(risk=cc_charge_45$charge_pred5,ncuts=10)

setDF(cc_charge_45)
cc_charge_obv <- survivor(data=cc_charge_45,risk_data="charge_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(cc_charge_45)

cc_charge_pred <- cc_charge_45[,mean(charge_pred5),by="charge_decile"][order(charge_decile)]

### C. C3PO MI
pce_wf <- pce_40[c(Dem.Gender.no_filter=='Female' & race_black==0)]
pce_bf <- pce_40[c(Dem.Gender.no_filter=='Female' & race_black==1)]
pce_wm <- pce_40[c(Dem.Gender.no_filter=='Male' & race_black==0)]
pce_bm <- pce_40[c(Dem.Gender.no_filter=='Male' & race_black==1)]

pce_wf$pce_decile <- classifier(risk=pce_wf$pce_risk,ncuts=10)
pce_bf$pce_decile <- classifier(risk=pce_bf$pce_risk,ncuts=10)
pce_wm$pce_decile <- classifier(risk=pce_wm$pce_risk,ncuts=10)
pce_bm$pce_decile <- classifier(risk=pce_bm$pce_risk,ncuts=10)

setDF(pce_wf); setDF(pce_bf); setDF(pce_wm); setDF(pce_bm)
pce_wf_obv <- survivor(data=pce_wf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bf_obv <- survivor(data=pce_bf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_wm_obv <- survivor(data=pce_wm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bm_obv <- survivor(data=pce_bm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(pce_wf); setDT(pce_bf); setDT(pce_wm); setDT(pce_bm)

pce_wf_pred <- pce_wf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_bf_pred <- pce_bf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_wm_pred <- pce_wm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_bm_pred <- pce_bm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]

### D. CC MI
cc_pce_wf <- cc_pce_40[c(sex=='F' & race_black==0)]
cc_pce_bf <- cc_pce_40[c(sex=='F' & race_black==1)]
cc_pce_wm <- cc_pce_40[c(sex=='M' & race_black==0)]
cc_pce_bm <- cc_pce_40[c(sex=='M' & race_black==1)]

cc_pce_wf$pce_decile <- classifier(risk=cc_pce_wf$pce_risk,ncuts=10)
cc_pce_bf$pce_decile <- classifier(risk=cc_pce_bf$pce_risk,ncuts=10)
cc_pce_wm$pce_decile <- classifier(risk=cc_pce_wm$pce_risk,ncuts=10)
cc_pce_bm$pce_decile <- classifier(risk=cc_pce_bm$pce_risk,ncuts=10)

setDF(cc_pce_wf); setDF(cc_pce_bf); setDF(cc_pce_wm); setDF(cc_pce_bm)
cc_pce_wf_obv <- survivor(data=cc_pce_wf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
cc_pce_bf_obv <- survivor(data=cc_pce_bf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
cc_pce_wm_obv <- survivor(data=cc_pce_wm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
cc_pce_bm_obv <- survivor(data=cc_pce_bm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(cc_pce_wf); setDT(cc_pce_bf); setDT(cc_pce_wm); setDT(cc_pce_bm)

cc_pce_wf_pred <- cc_pce_wf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
cc_pce_bf_pred <- cc_pce_bf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
cc_pce_wm_pred <- cc_pce_wm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
cc_pce_bm_pred <- cc_pce_bm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]

###################################### Step 2: Measures and plot
### A. C3PO AF
## Adaptive regression stuff
charge_45$cox.5yr.cll <- log(-log(1-charge_45$charge_pred5/100))

calibrate.cox <- hare(data=charge_45$af_5y.t,delta=charge_45$incd_af_5y,
                      cov=as.matrix(charge_45$cox.5yr.cll))
predict.grid.cox <- seq(quantile(charge_45$charge_pred5/100,probs=0.01),
                        quantile(charge_45$charge_pred5/100,probs=0.99),length=100)
predict.grid.cox.cll <- log(-log(1-predict.grid.cox))
predict.calibrate.cox <- phare(5,predict.grid.cox.cll,calibrate.cox)

# Plots for visualization
pdf(file='cal_charge.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- charge_pred$V1
y <- do.call(rbind,charge_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2b83ba',
     xaxt='n',xlim=c(0,40),ylim=c(0,40),pch=19,cex=1.5,bty='n')

axis(1,at=seq(0,40,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,40,10),las=1)

segments(-1,-1,41,41,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox*100,predict.calibrate.cox*100,type="l",lty=1,col="#2b83ba",
     xlim=c(0,40),ylim=c(0,40),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Quant measures
predict.calibrate.cox <- phare(5,charge_45$cox.5yr.cll,calibrate.cox)

# Predicted probability of death within 1 year for all subjects in
# validation sample
ICI.5yr.cox <- mean(abs(charge_45$charge_pred5/100 - predict.calibrate.cox))
E50.5yr.cox <- median(abs(charge_45$charge_pred5/100 - predict.calibrate.cox))
E90.5yr.cox <- quantile(abs(charge_45$charge_pred5/100 - predict.calibrate.cox),probs=0.9)

################################################################################ CC AF
## Adaptive regression stuff
cc_charge_45$cox.5yr.cll <- log(-log(1-cc_charge_45$charge_pred5/100))

calibrate.cox_cc <- hare(data=cc_charge_45$af_5y.t,delta=cc_charge_45$incd_af_5y,
                         cov=as.matrix(cc_charge_45$cox.5yr.cll))
predict.grid.cox_cc <- seq(quantile(cc_charge_45$charge_pred5/100,probs=0.01),
                           quantile(cc_charge_45$charge_pred5/100,probs=0.99),length=100)
predict.grid.cox.cll_cc <- log(-log(1-predict.grid.cox_cc))
predict.calibrate.cox_cc <- phare(5,predict.grid.cox.cll_cc,calibrate.cox_cc)

# Plots for visualization
pdf(file='cc_cal_charge.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_charge_pred$V1
y <- do.call(rbind,cc_charge_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#bd0026',
     xaxt='n',xlim=c(0,40),ylim=c(0,40),pch=19,cex=1.5,bty='n')

axis(1,at=seq(0,40,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,40,10),las=1)

segments(-1,-1,41,41,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_cc*100,predict.calibrate.cox_cc*100,type="l",lty=1,col="#bd0026",
     xlim=c(0,40),ylim=c(0,40),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Quant measures
predict.calibrate.cox_cc <- phare(5,cc_charge_45$cox.5yr.cll,calibrate.cox_cc)

# Predicted probability of death within 1 year for all subjects in
# validation sample
ICI.5yr.cox <- mean(abs(cc_charge_45$charge_pred5/100 - predict.calibrate.cox_cc))
E50.5yr.cox <- median(abs(cc_charge_45$charge_pred5/100 - predict.calibrate.cox_cc))
E90.5yr.cox <- quantile(abs(cc_charge_45$charge_pred5/100 - predict.calibrate.cox_cc),probs=0.9)

################################################################################ C3PO MI
# WF
# Fit Cox PH model to model hazard of death. Use all baseline covariates.
pce_wf$cox.10yr.cll <- log(-log(1-pce_wf$pce_risk/100))
calibrate.cox_wf <- hare(data=pce_wf$mi_stroke_10y.t,delta=pce_wf$incd_mi_stroke_10y,
                         cov=as.matrix(pce_wf$cox.10yr.cll))
predict.grid.cox_wf <- seq(quantile(pce_wf$pce_risk/100,probs=0.01),
                           quantile(pce_wf$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_wf <- log(-log(1-predict.grid.cox_wf))
predict.calibrate.cox_wf <- phare(10,predict.grid.cox.cll_wf,calibrate.cox_wf)

# Plots for visualization
pdf(file='cal_wf.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_wf_pred$V1
y <- do.call(rbind,pce_wf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,50),ylim=c(0,50),pch=19,cex=1.5,bty='n',col='#9e9ac8')

axis(1,at=seq(0,50,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,50,10),las=1)

segments(-1,-1,51,51,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_wf*100,predict.calibrate.cox_wf*100,type="l",lty=1,col="#9e9ac8",
     xlim=c(0,50),ylim=c(0,50),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_wf <- phare(10,pce_wf$cox.10yr.cll,calibrate.cox_wf)
ICI.10yr.cox_wf <- mean(abs(pce_wf$pce_risk/100 - predict.calibrate.cox_wf))
E50.10yr.cox_wf <- median(abs(pce_wf$pce_risk/100 - predict.calibrate.cox_wf))
E90.10yr.cox_wf <- quantile(abs(pce_wf$pce_risk/100 - predict.calibrate.cox_wf),probs=0.9)

# BF
pce_bf$cox.10yr.cll <- log(-log(1-pce_bf$pce_risk/100))
calibrate.cox_bf <- hare(data=pce_bf$mi_stroke_10y.t,delta=pce_bf$incd_mi_stroke_10y,
                         cov=as.matrix(pce_bf$cox.10yr.cll))
predict.grid.cox_bf <- seq(quantile(pce_bf$pce_risk/100,probs=0.01),
                           quantile(pce_bf$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_bf <- log(-log(1-predict.grid.cox_bf))
predict.calibrate.cox_bf <- phare(10,predict.grid.cox.cll_bf,calibrate.cox_bf)

# Plots for visualization
pdf(file='cal_bf.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_bf_pred$V1
y <- do.call(rbind,pce_bf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,60),ylim=c(0,60),pch=19,cex=1.5,bty='n',col='#3f007d')

axis(1,at=seq(0,60,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,60,10),las=1)

segments(-1,-1,61,61,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_bf*100,predict.calibrate.cox_bf*100,type="l",lty=1,col="#3f007d",
     xlim=c(0,60),ylim=c(0,60),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_bf <- phare(10,pce_bf$cox.10yr.cll,calibrate.cox_bf)
ICI.10yr.cox_bf <- mean(abs(pce_bf$pce_risk/100 - predict.calibrate.cox_bf))
E50.10yr.cox_bf <- median(abs(pce_bf$pce_risk/100 - predict.calibrate.cox_bf))
E90.10yr.cox_bf <- quantile(abs(pce_bf$pce_risk/100 - predict.calibrate.cox_bf),probs=0.9)

# WM
pce_wm$cox.10yr.cll <- log(-log(1-pce_wm$pce_risk/100))
calibrate.cox_wm <- hare(data=pce_wm$mi_stroke_10y.t,delta=pce_wm$incd_mi_stroke_10y,
                         cov=as.matrix(pce_wm$cox.10yr.cll))
predict.grid.cox_wm <- seq(quantile(pce_wm$pce_risk/100,probs=0.01),
                           quantile(pce_wm$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_wm <- log(-log(1-predict.grid.cox_wm))
predict.calibrate.cox_wm <- phare(10,predict.grid.cox.cll_wm,calibrate.cox_wm)

# Plots for visualization
pdf(file='cal_wm.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_wm_pred$V1
y <- do.call(rbind,pce_wm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,50),ylim=c(0,50),pch=19,cex=1.5,bty='n',col='#74c476')

axis(1,at=seq(0,50,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,50,10),las=1)

segments(-1,-1,51,51,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_wm*100,predict.calibrate.cox_wm*100,type="l",lty=1,col="#74c476",
     xlim=c(0,50),ylim=c(0,50),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_wm <- phare(10,pce_wm$cox.10yr.cll,calibrate.cox_wm)
ICI.10yr.cox_wm <- mean(abs(pce_wm$pce_risk/100 - predict.calibrate.cox_wm))
E50.10yr.cox_wm <- median(abs(pce_wm$pce_risk/100 - predict.calibrate.cox_wm))
E90.10yr.cox_wm <- quantile(abs(pce_wm$pce_risk/100 - predict.calibrate.cox_wm),probs=0.9)

# BM
pce_bm$cox.10yr.cll <- log(-log(1-pce_bm$pce_risk/100))
calibrate.cox_bm <- hare(data=pce_bm$mi_stroke_10y.t,delta=pce_bm$incd_mi_stroke_10y,
                         cov=as.matrix(pce_bm$cox.10yr.cll))
predict.grid.cox_bm <- seq(quantile(pce_bm$pce_risk/100,probs=0.01),
                           quantile(pce_bm$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_bm <- log(-log(1-predict.grid.cox_bm))
predict.calibrate.cox_bm <- phare(10,predict.grid.cox.cll_bm,calibrate.cox_bm)

# Plots for visualization
pdf(file='cal_bm.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_bm_pred$V1
y <- do.call(rbind,pce_bm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,40),ylim=c(0,40),pch=19,cex=1.5,bty='n',col='#00441b')

axis(1,at=seq(0,40,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,40,10),las=1)

segments(-1,-1,41,41,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_bm*100,predict.calibrate.cox_bm*100,type="l",lty=1,col="#00441b",
     xlim=c(0,40),ylim=c(0,40),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_bm <- phare(10,pce_bm$cox.10yr.cll,calibrate.cox_bm)
ICI.10yr.cox_bm <- mean(abs(pce_bm$pce_risk/100 - predict.calibrate.cox_bm))
E50.10yr.cox_bm <- median(abs(pce_bm$pce_risk/100 - predict.calibrate.cox_bm))
E90.10yr.cox_bm <- quantile(abs(pce_bm$pce_risk/100 - predict.calibrate.cox_bm),probs=0.9)

################################################################################ CC MI
# WF
cc_pce_wf$cox.10yr.cll <- log(-log(1-cc_pce_wf$pce_risk/100))
calibrate.cox_wf_cc <- hare(data=cc_pce_wf$mi_stroke_10y.t,delta=cc_pce_wf$incd_mi_stroke_10y,
                            cov=as.matrix(cc_pce_wf$cox.10yr.cll))
predict.grid.cox_wf_cc <- seq(quantile(cc_pce_wf$pce_risk/100,probs=0.01),
                              quantile(cc_pce_wf$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_wf_cc <- log(-log(1-predict.grid.cox_wf_cc))
predict.calibrate.cox_wf_cc <- phare(10,predict.grid.cox.cll_wf_cc,calibrate.cox_wf_cc)

# Plots for visualization
pdf(file='cal_wf_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_pce_wf_pred$V1
y <- do.call(rbind,cc_pce_wf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,60),ylim=c(0,60),pch=19,cex=1.5,bty='n',col='#fed976')

axis(1,at=seq(0,60,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,60,10),las=1)

segments(-1,-1,61,61,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_wf_cc*100,predict.calibrate.cox_wf_cc*100,type="l",lty=1,col="#fed976",
     xlim=c(0,60),ylim=c(0,60),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_wf_cc <- phare(10,cc_pce_wf$cox.10yr.cll,calibrate.cox_wf_cc)
ICI.10yr.cox_wf_cc <- mean(abs(cc_pce_wf$pce_risk/100 - predict.calibrate.cox_wf_cc))
E50.10yr.cox_wf_cc <- median(abs(cc_pce_wf$pce_risk/100 - predict.calibrate.cox_wf_cc))
E90.10yr.cox_wf_cc <- quantile(abs(cc_pce_wf$pce_risk/100 - predict.calibrate.cox_wf_cc),probs=0.9)

# BF
cc_pce_bf$cox.10yr.cll <- log(-log(1-cc_pce_bf$pce_risk/100))
calibrate.cox_bf_cc <- hare(data=cc_pce_bf$mi_stroke_10y.t,delta=cc_pce_bf$incd_mi_stroke_10y,
                            cov=as.matrix(cc_pce_bf$cox.10yr.cll))
predict.grid.cox_bf_cc <- seq(quantile(cc_pce_bf$pce_risk/100,probs=0.01),
                              quantile(cc_pce_bf$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_bf_cc <- log(-log(1-predict.grid.cox_bf_cc))
predict.calibrate.cox_bf_cc <- phare(10,predict.grid.cox.cll_bf_cc,calibrate.cox_bf_cc)

# Plots for visualization
pdf(file='cal_bf_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_pce_bf_pred$V1
y <- do.call(rbind,cc_pce_bf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,60),ylim=c(0,60),pch=19,cex=1.5,bty='n',col='#fd8d3c')

axis(1,at=seq(0,60,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,60,10),las=1)

segments(-1,-1,61,61,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_bf_cc*100,predict.calibrate.cox_bf_cc*100,type="l",lty=1,col="#fd8d3c",
     xlim=c(0,60),ylim=c(0,60),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_bf_cc <- phare(10,cc_pce_bf$cox.10yr.cll,calibrate.cox_bf_cc)
ICI.10yr.cox_bf_cc <- mean(abs(cc_pce_bf$pce_risk/100 - predict.calibrate.cox_bf_cc))
E50.10yr.cox_bf_cc <- median(abs(cc_pce_bf$pce_risk/100 - predict.calibrate.cox_bf_cc))
E90.10yr.cox_bf_cc <- quantile(abs(cc_pce_bf$pce_risk/100 - predict.calibrate.cox_bf_cc),probs=0.9)

# WM
cc_pce_wm$cox.10yr.cll <- log(-log(1-cc_pce_wm$pce_risk/100))
calibrate.cox_wm_cc <- hare(data=cc_pce_wm$mi_stroke_10y.t,delta=cc_pce_wm$incd_mi_stroke_10y,
                            cov=as.matrix(cc_pce_wm$cox.10yr.cll))
predict.grid.cox_wm_cc <- seq(quantile(cc_pce_wm$pce_risk/100,probs=0.01),
                              quantile(cc_pce_wm$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_wm_cc <- log(-log(1-predict.grid.cox_wm_cc))
predict.calibrate.cox_wm_cc <- phare(10,predict.grid.cox.cll_wm_cc,calibrate.cox_wm_cc)

# Plots for visualization
pdf(file='cal_wm_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_pce_wm_pred$V1
y <- do.call(rbind,cc_pce_wm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,60),ylim=c(0,60),pch=19,cex=1.5,bty='n',col='#80cdc1')

axis(1,at=seq(0,60,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,60,10),las=1)

segments(-1,-1,61,61,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_wm_cc*100,predict.calibrate.cox_wm_cc*100,type="l",lty=1,col="#80cdc1",
     xlim=c(0,60),ylim=c(0,60),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_wm_cc <- phare(10,cc_pce_wm$cox.10yr.cll,calibrate.cox_wm_cc)
ICI.10yr.cox_wm_cc <- mean(abs(cc_pce_wm$pce_risk/100 - predict.calibrate.cox_wm_cc))
E50.10yr.cox_wm_cc <- median(abs(cc_pce_wm$pce_risk/100 - predict.calibrate.cox_wm_cc))
E90.10yr.cox_wm_cc <- quantile(abs(cc_pce_wm$pce_risk/100 - predict.calibrate.cox_wm_cc),probs=0.9)

# BM
cc_pce_bm$cox.10yr.cll <- log(-log(1-cc_pce_bm$pce_risk/100))
calibrate.cox_bm_cc <- hare(data=cc_pce_bm$mi_stroke_10y.t,delta=cc_pce_bm$incd_mi_stroke_10y,
                            cov=as.matrix(cc_pce_bm$cox.10yr.cll))
predict.grid.cox_bm_cc <- seq(quantile(cc_pce_bm$pce_risk/100,probs=0.01),
                              quantile(cc_pce_bm$pce_risk/100,probs=0.99),length=100)
predict.grid.cox.cll_bm_cc <- log(-log(1-predict.grid.cox_bm_cc))
predict.calibrate.cox_bm_cc <- phare(10,predict.grid.cox.cll_bm_cc,calibrate.cox_bm_cc)

# Plots for visualization
pdf(file='cal_bm_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_pce_bm_pred$V1
y <- do.call(rbind,cc_pce_bm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,50),ylim=c(0,50),pch=19,cex=1.5,bty='n',col='#01665e')

axis(1,at=seq(0,50,10),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,50,10),las=1)

segments(-1,-1,51,51,lwd=1.2,lty=2)

par(new=TRUE)
plot(predict.grid.cox_bm_cc*100,predict.calibrate.cox_bm_cc*100,type="l",lty=1,col="#01665e",
     xlim=c(0,50),ylim=c(0,50),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

predict.calibrate.cox_bm_cc <- phare(10,cc_pce_bm$cox.10yr.cll,calibrate.cox_bm_cc)
ICI.10yr.cox_bm_cc <- mean(abs(cc_pce_bm$pce_risk/100 - predict.calibrate.cox_bm_cc))
E50.10yr.cox_bm_cc <- median(abs(cc_pce_bm$pce_risk/100 - predict.calibrate.cox_bm_cc))
E90.10yr.cox_bm_cc <- quantile(abs(cc_pce_bm$pce_risk/100 - predict.calibrate.cox_bm_cc),probs=0.9)

