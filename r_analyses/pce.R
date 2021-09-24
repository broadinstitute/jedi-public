# Script to merge CAD definitions with wide file

# Depends
library(data.table)
library(stringr)
library(prodlim)
library(Cairo)
library(plyr)
library(survival)
library(timeROC)

# Source helper functions
source(file='~/functions/functions.R')

# Load latest wide file
c3po_wide <- fread(file='wide.csv')

# Cast ages
numerics <- c('start_fu','last_encounter','Dem.Date_Of_Death_Age.no_filter','mi_age')
for (j in numerics){set(c3po_wide,j=j,value=as.numeric(str_extract(c3po_wide[[j]],'\\d+')))}

# Cast continuous variables
numerics <- c('start_fu_Systolic','start_fu_Diastolic','start_fu_total_cholesterol','start_fu_hdl','mi_age')
for (j in numerics){set(c3po_wide,j=j,value=as.numeric(c3po_wide[[j]]))}

## PCE completion variable
c3po_wide[,pce_complete := ifelse(!is.na(race_white) & !is.na(start_fu) & (Dem.Gender.no_filter != '')
                                  & !is.na(start_fu_total_cholesterol) & !is.na(start_fu_hdl)
                                  & !is.na(sbp),1,0)]

# Remove missing last encounter
c3po_wide <- c3po_wide[!is.na(last_encounter)]

## Time/outcome variables
c3po_wide[,':='(mi_stroke = ifelse(!is.na(mi_age) | !is.na(ischemic_stroke_age),1,0),
                mi = ifelse(!is.na(mi_age),1,0),
                stroke = ifelse(!is.na(ischemic_stroke_age),1,0))]
c3po_wide[,':='(prev_mi = ifelse(c(mi==1 & (mi_age <= start_fu)),1,0),
                prev_stroke = ifelse(c(stroke==1 & (ischemic_stroke_age <= start_fu)),1,0))]
c3po_wide[,':='(prev_mi_stroke = ifelse(prev_mi == 1 | prev_stroke == 1,1,0))]
c3po_wide[,':='(incd_mi = ifelse(mi==1 & (mi_age > start_fu) & !is.na(global_censor_age) & (mi_age <= global_censor_age),1,0),
                incd_stroke = ifelse(stroke==1 & (ischemic_stroke_age > start_fu) & !is.na(global_censor_age) & (ischemic_stroke_age <= global_censor_age),1,0))]
c3po_wide[,':='(incd_mi_stroke = ifelse(incd_mi == 1 | incd_stroke == 1,1,0))]
c3po_wide[,':='(time_to_mi_stroke = ifelse(mi_stroke == 1,pmin(mi_age - start_fu,global_censor_age - start_fu,ischemic_stroke_age-start_fu,na.rm=T)/365.25,
                                           pmin((last_encounter - start_fu),(global_censor_age-start_fu),(Dem.Date_Of_Death_Age.no_filter-start_fu),na.rm=T)/365.25),
                time_to_mi = ifelse(mi == 1,pmin(mi_age - start_fu,global_censor_age - start_fu)/365.25,
                                    pmin((last_encounter - start_fu),(global_censor_age - start_fu),
                                         (Dem.Date_Of_Death_Age.no_filter-start_fu),na.rm=T)/365.25),
                time_to_stroke = ifelse(stroke == 1,pmin(ischemic_stroke_age-start_fu,global_censor_age-start_fu)/365.25,
                                        pmin((last_encounter - start_fu),(global_censor_age - start_fu),
                                             (Dem.Date_Of_Death_Age.no_filter-start_fu),na.rm=T)/365.25))]
c3po_wide[,':='(incd_mi_stroke_10y = ifelse(incd_mi_stroke==1 & time_to_mi_stroke <= 10,1,0),
                incd_stroke_10y = ifelse(incd_stroke==1 & time_to_stroke <= 10,1,0),
                incd_mi_10y = ifelse(incd_mi==1 & time_to_mi <= 10,1,0),
                mi_stroke_10y.t = ifelse(time_to_mi_stroke > 10,10,time_to_mi_stroke))]

# Exclusions
complete_case <- c3po_wide[pce_complete==1] 
no_mi <- complete_case[prev_mi_stroke==0] 
pce_set <- no_mi[mi_stroke_10y.t > 0]
pce_40 <- pce_set[(start_fu/365.25) >= 40 & (start_fu/365.25) < 80]

# Calculate transformed variables for PCE
pce_40[,start_fu_yrs := start_fu/365.25]
## Log variables
pce_40[,':='(ln_age = log(start_fu_yrs),ln_age2 = (log(start_fu_yrs))**2,ln_chol = log(start_fu_total_cholesterol),
             ln_hdl = log(start_fu_hdl))]
## Treatment-specific BP variables
pce_40[,':='(ln_sbp_treated = ifelse(start_fu_hypertension_med==1,log(sbp),0),
             ln_sbp_untreated = ifelse(start_fu_hypertension_med==0,log(sbp),0))]
## Interaction variables
pce_40[,':='(ln_age_chol = ln_age*ln_chol,ln_age_hdl = ln_age*ln_hdl,ln_age_sbp_treated = ln_age*ln_sbp_treated,
             ln_age_sbp_untreated = ln_age*ln_sbp_untreated,ln_age_smoker = ln_age * current_smoker_startfu)]

# Calculate scores
pce_40[c(Dem.Gender.no_filter=='Female' & race_black==0),
       pce := -29.799*ln_age + 4.884*ln_age2 + 13.540*ln_chol + (-3.114)*ln_age_chol + (-13.578)*ln_hdl +
         3.149*ln_age_hdl + 2.019*ln_sbp_treated + 1.957*ln_sbp_untreated + 7.574*current_smoker_startfu + 
         (-1.665)*ln_age_smoker + 0.661*start_fu_dm]

pce_40[c(Dem.Gender.no_filter=='Female' & race_black==1),
       pce := 17.114*ln_age + 0.940*ln_chol + (-18.920)*ln_hdl +
         4.475*ln_age_hdl + 29.291*ln_sbp_treated + (-6.432)*ln_age_sbp_treated +
         27.820*ln_sbp_untreated + (-6.087)*ln_age_sbp_untreated + 0.691*current_smoker_startfu + 0.874*start_fu_dm]

pce_40[c(Dem.Gender.no_filter=='Male' & race_black==0),
       pce := 12.344*ln_age + 11.853*ln_chol + (-2.664)*ln_age_chol +
         (-7.990)*ln_hdl + 1.769*ln_age_hdl + 1.797*ln_sbp_treated +
         1.764*ln_sbp_untreated + 7.837*current_smoker_startfu + (-1.795)*ln_age_smoker + 0.658*start_fu_dm]

pce_40[c(Dem.Gender.no_filter=='Male' & race_black==1),
       pce := 2.469*ln_age + 0.302*ln_chol + (-0.307)*ln_hdl + 1.916*ln_sbp_treated +
         1.809*ln_sbp_untreated + 0.549*current_smoker_startfu + 0.645*start_fu_dm]

# Calculate predicted risks
pce_40[c(Dem.Gender.no_filter=='Female' & race_black==0),
       pce_risk := (1-0.9665^exp(pce-(-29.18)))*100]
pce_40[c(Dem.Gender.no_filter=='Female' & race_black==1),
       pce_risk := (1-0.9533^exp(pce-86.61))*100]
pce_40[c(Dem.Gender.no_filter=='Male' & race_black==0),
       pce_risk := (1-0.9144^exp(pce-61.18))*100]
pce_40[c(Dem.Gender.no_filter=='Male' & race_black==1),
       pce_risk := (1-0.8954^exp(pce-19.54))*100]

# Save out PCE set
save(pce_40,file='pce.RData')

# Overall event rates
pce_40[,dummy := 1]
setDF(pce_40)
pce_40_obv1 <- survivor(data=pce_40,risk_data="dummy",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(pce_40)

ir_pce_1 <- ir(data=pce_40,strata='dummy',time='mi_stroke_10y.t',status='incd_mi_stroke_10y')

# Subset data for convenience
pce_wf <- pce_40[c(Dem.Gender.no_filter=='Female' & race_black==0)]
pce_bf <- pce_40[c(Dem.Gender.no_filter=='Female' & race_black==1)]
pce_wm <- pce_40[c(Dem.Gender.no_filter=='Male' & race_black==0)]
pce_bm <- pce_40[c(Dem.Gender.no_filter=='Male' & race_black==1)]

# Standardized scores
pce_wf[,pce_std := (pce - mean(pce))/sd(pce)]
pce_bf[,pce_std := (pce - mean(pce))/sd(pce)]
pce_wm[,pce_std := (pce - mean(pce))/sd(pce)]
pce_bm[,pce_std := (pce - mean(pce))/sd(pce)]

# Censor variable for GND
pce_wf[,censored := ifelse(incd_mi_stroke_10y==1,0,1)]
pce_bf[,censored := ifelse(incd_mi_stroke_10y==1,0,1)]
pce_wm[,censored := ifelse(incd_mi_stroke_10y==1,0,1)]
pce_bm[,censored := ifelse(incd_mi_stroke_10y==1,0,1)]

# HRs and discrimination
white_f <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce_std,data=pce_wf)
summary(white_f)
black_f <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce_std,data=pce_bf)
summary(black_f)
white_m <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce_std,data=pce_wm)
summary(white_m)
black_m <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce_std,data=pce_bm)
summary(black_m)

# ROC curves
roc_wf <- timeROC(T=pce_wf$mi_stroke_10y.t, delta=pce_wf$incd_mi_stroke_10y,
                  marker=pce_wf$pce,cause=1,times=(9.999))
roc_bf <- timeROC(T=pce_bf$mi_stroke_10y.t, delta=pce_bf$incd_mi_stroke_10y,
                  marker=pce_bf$pce,cause=1,times=(9.999))
roc_wm <- timeROC(T=pce_wm$mi_stroke_10y.t, delta=pce_wm$incd_mi_stroke_10y,
                  marker=pce_wm$pce,cause=1,times=(9.999))
roc_bm <- timeROC(T=pce_bm$mi_stroke_10y.t, delta=pce_bm$incd_mi_stroke_10y,
                  marker=pce_bm$pce,cause=1,times=(9.999))

# AUCs with CIs
# WF
boot_wf <- auc_boot(status='incd_mi_stroke_10y',time='mi_stroke_10y.t',response='pce',
                        data=pce_wf,times=9.999,runs=200)

auc_wf <- timeROC(T=pce_wf$mi_stroke_10y.t, delta=pce_wf$incd_mi_stroke_10y,
                  marker=pce_wf$pce,cause=1,times=c(0,9.999))
cstat_wf <- c(auc_wf$AUC[length(auc_wf$AUC)],
              auc_wf$AUC[length(auc_wf$AUC)] - 1.96*sd(boot_wf),
              auc_wf$AUC[length(auc_wf$AUC)] + 1.96*sd(boot_wf))
print(cstat_wf)

# BF
boot_bf <- auc_boot(status='incd_mi_stroke_10y',time='mi_stroke_10y.t',response='pce',
                    data=pce_bf,times=9.999,runs=200)

auc_bf <- timeROC(T=pce_bf$mi_stroke_10y.t, delta=pce_bf$incd_mi_stroke_10y,
                  marker=pce_bf$pce,cause=1,times=c(0,9.999))
cstat_bf <- c(auc_bf$AUC[length(auc_bf$AUC)],
              auc_bf$AUC[length(auc_bf$AUC)] - 1.96*sd(boot_bf),
              auc_bf$AUC[length(auc_bf$AUC)] + 1.96*sd(boot_bf))
print(cstat_bf)

# WM
boot_wm <- auc_boot(status='incd_mi_stroke_10y',time='mi_stroke_10y.t',response='pce',
                    data=pce_wm,times=9.999,runs=200)

auc_wm <- timeROC(T=pce_wm$mi_stroke_10y.t, delta=pce_wm$incd_mi_stroke_10y,
                  marker=pce_wm$pce,cause=1,times=c(0,9.999))
cstat_wm <- c(auc_wm$AUC[length(auc_wm$AUC)],
              auc_wm$AUC[length(auc_wm$AUC)] - 1.96*sd(boot_wm),
              auc_wm$AUC[length(auc_wm$AUC)] + 1.96*sd(boot_wm))
print(cstat_wm)

# BM
boot_bm <- auc_boot(status='incd_mi_stroke_10y',time='mi_stroke_10y.t',response='pce',
                    data=pce_bm,times=9.999,runs=200)

auc_bm <- timeROC(T=pce_bm$mi_stroke_10y.t, delta=pce_bm$incd_mi_stroke_10y,
                  marker=pce_bm$pce,cause=1,times=c(0,9.999))
cstat_bm <- c(auc_bm$AUC[length(auc_bm$AUC)],
              auc_bm$AUC[length(auc_bm$AUC)] - 1.96*sd(boot_bm),
              auc_bm$AUC[length(auc_bm$AUC)] + 1.96*sd(boot_bm))
print(cstat_bm)

## Plotting
pdf(file='roc_pce_timeroc.pdf',height=4,width=4,
    pointsize=3)

# Plot settings
par(oma=c(1,1,1,1))
par(mar=c(4,4.8,1,1))

plot.new() #?

# Specifics
## Plot 1
plot(roc_wf,9.999,add=T,col='#9e9ac8',lwd=1.2)
par(new=TRUE)
plot(roc_bf,9.999,add=T,col='#3f007d',lwd=1.2)
par(new=TRUE)
plot(roc_wm,9.999,add=T,col='#74c476',lwd=1.2)
par(new=TRUE)
plot(roc_bm,9.999,add=T,col='#00441b',lwd=1.2)

## Axes
axis(1,at=seq(1,0,-0.2),cex.axis=1.6,
     labels=c('1.0','0.8','0.6','0.4','0.2','0.0'))
axis(2,at=seq(0,1,0.2),cex.axis=1.6,las=2)

## Labels
title(xlab='1-Specificity',line=2.5,cex.lab=1.8)
title(ylab='Sensitivity',line=3.2,cex.lab=1.8)

## Legend
legend(0.4,0.25,legend=c('White women (0.770)','Black women (0.724)',
                        'White men (0.738)','Black men (0.725)'),
       col=c('#9e9ac8','#3f007d','#74c476','#00441b'),
       lty=1,lwd=1,pch=1,bty='n',cex=1.5)

## Stop
dev.off()

## Calibration

# Use classifier to classify scores into quantiles (size 10)
pce_wf$pce_decile <- classifier(risk=pce_wf$pce_risk,ncuts=10)
pce_bf$pce_decile <- classifier(risk=pce_bf$pce_risk,ncuts=10)
pce_wm$pce_decile <- classifier(risk=pce_wm$pce_risk,ncuts=10)
pce_bm$pce_decile <- classifier(risk=pce_bm$pce_risk,ncuts=10)

pce_wf$pce_quintile <- classifier(risk=pce_wf$pce_risk,ncuts=5)
pce_bf$pce_quintile <- classifier(risk=pce_bf$pce_risk,ncuts=5)
pce_wm$pce_quintile <- classifier(risk=pce_wm$pce_risk,ncuts=5)
pce_bm$pce_quintile <- classifier(risk=pce_bm$pce_risk,ncuts=5)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(pce_wf); setDF(pce_bf); setDF(pce_wm); setDF(pce_bm)
pce_wf_obv <- survivor(data=pce_wf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bf_obv <- survivor(data=pce_bf,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_wm_obv <- survivor(data=pce_wm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bm_obv <- survivor(data=pce_bm,risk_data="pce_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(pce_wf); setDT(pce_bf); setDT(pce_wm); setDT(pce_bm)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
pce_wf_pred <- pce_wf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_bf_pred <- pce_bf[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_wm_pred <- pce_wm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]
pce_bm_pred <- pce_bm[,mean(pce_risk),by="pce_decile"][order(pce_decile)]

# Plots for visualization
pdf(file='cal_pce_wf.pdf',height=4,width=4,
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

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='cal_pce_bf.pdf',height=4,width=4,
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

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='cal_pce_wm.pdf',height=4,width=4,
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

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='cal_pce_bm.pdf',height=4,width=4,
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

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

calib_wf <- GND.calib(pred=pce_wf$pce_risk/100,tvar=pce_wf$mi_stroke_10y.t,
                        out=pce_wf$incd_mi_stroke_10y,groups=pce_wf$pce_decile,
                        cens.t=pce_wf$censored,adm.cens=10)
print(calib_wf)
calib_bf <- GND.calib(pred=pce_bf$pce_risk/100,tvar=pce_bf$mi_stroke_10y.t,
                      out=pce_bf$incd_mi_stroke_10y,groups=pce_bf$pce_decile,
                      cens.t=pce_bf$censored,adm.cens=10)
print(calib_bf)

calib_wm <- GND.calib(pred=pce_wm$pce_risk/100,tvar=pce_wm$mi_stroke_10y.t,
                      out=pce_wm$incd_mi_stroke_10y,groups=pce_wm$pce_decile,
                      cens.t=pce_wm$censored,adm.cens=10)
print(calib_wm)

calib_bm <- GND.calib(pred=pce_bm$pce_risk/100,tvar=pce_bm$mi_stroke_10y.t,
                      out=pce_bm$incd_mi_stroke_10y,groups=pce_bm$pce_decile,
                      cens.t=pce_bm$censored,adm.cens=10)
print(calib_bm)

####################### Re-calibration
## WF
pce_wf[,avgbeta := mean(pce)]
res <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce, data=pce_wf)
km <- survfit(res, data=data.frame(x1=mean(pce)),type="kaplan-meier")
s0 <- summary(km, times=c(10))$surv
pce_wf[,pce_risk_cal := (1-(s0)^exp(pce - (avgbeta)))*100]

## BF
pce_bf[,avgbeta := mean(pce)]
res <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce, data=pce_bf)
km <- survfit(res, data=data.frame(x1=mean(pce)),type="kaplan-meier")
s0 <- summary(km, times=c(10))$surv
pce_bf[,pce_risk_cal := (1-(s0)^exp(pce - (avgbeta)))*100]

## WM
pce_wm[,avgbeta := mean(pce)]
res <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce, data=pce_wm)
km <- survfit(res, data=data.frame(x1=mean(pce)),type="kaplan-meier")
s0 <- summary(km, times=c(10))$surv
pce_wm[,pce_risk_cal := (1-(s0)^exp(pce - (avgbeta)))*100]

## BM
pce_bm[,avgbeta := mean(pce)]
res <- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce, data=pce_bm)
km <- survfit(res, data=data.frame(x1=mean(pce)),type="kaplan-meier")
s0 <- summary(km, times=c(10))$surv
pce_bm[,pce_risk_cal := (1-(s0)^exp(pce - (avgbeta)))*100]

### Now repeat calibration measures

# Use classifier to classify scores into quantiles (size 10)
pce_wf$pce_cal_decile <- classifier(risk=pce_wf$pce_risk_cal,ncuts=10)
pce_bf$pce_cal_decile <- classifier(risk=pce_bf$pce_risk_cal,ncuts=10)
pce_wm$pce_cal_decile <- classifier(risk=pce_wm$pce_risk_cal,ncuts=10)
pce_bm$pce_cal_decile <- classifier(risk=pce_bm$pce_risk_cal,ncuts=10)

pce_wf$pce_cal_quintile <- classifier(risk=pce_wf$pce_risk_cal,ncuts=5)
pce_bf$pce_cal_quintile <- classifier(risk=pce_bf$pce_risk_cal,ncuts=5)
pce_wm$pce_cal_quintile <- classifier(risk=pce_wm$pce_risk_cal,ncuts=5)
pce_bm$pce_cal_quintile <- classifier(risk=pce_bm$pce_risk_cal,ncuts=5)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(pce_wf); setDF(pce_bf); setDF(pce_wm); setDF(pce_bm)
pce_wf_obv <- survivor(data=pce_wf,risk_data="pce_cal_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bf_obv <- survivor(data=pce_bf,risk_data="pce_cal_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_wm_obv <- survivor(data=pce_wm,risk_data="pce_cal_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
pce_bm_obv <- survivor(data=pce_bm,risk_data="pce_cal_decile",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(pce_wf); setDT(pce_bf); setDT(pce_wm); setDT(pce_bm)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
pce_wf_pred <- pce_wf[,mean(pce_risk_cal),by="pce_cal_decile"][order(pce_cal_decile)]
pce_bf_pred <- pce_bf[,mean(pce_risk_cal),by="pce_cal_decile"][order(pce_cal_decile)]
pce_wm_pred <- pce_wm[,mean(pce_risk_cal),by="pce_cal_decile"][order(pce_cal_decile)]
pce_bm_pred <- pce_bm[,mean(pce_risk_cal),by="pce_cal_decile"][order(pce_cal_decile)]

# Plots for visualization
pdf(file='pce_recal_wf.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_wf_pred$V1
y <- do.call(rbind,pce_wf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#9e9ac8',
     xaxt='n',xlim=c(0,50),ylim=c(0,50),pch=19,cex=1.5)

axis(1,at=seq(0,50,10),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,50,10),las=1)

segments(-1,-1,51,51,lwd=1.2,lty=2)

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='pce_recal_bf.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_bf_pred$V1
y <- do.call(rbind,pce_bf_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#3f007d',
     xaxt='n',xlim=c(0,60),ylim=c(0,60),pch=19,cex=1.5)

axis(1,at=seq(0,60,10),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,60,10),las=1)

segments(-1,-1,61,61,lwd=1.2,lty=2)

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='pce_recal_wm.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_wm_pred$V1
y <- do.call(rbind,pce_wm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#74c476',
     xaxt='n',xlim=c(0,50),ylim=c(0,50),pch=19,cex=1.5)

axis(1,at=seq(0,50,10),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,50,10),las=1)

segments(-1,-1,51,51,lwd=1.2,lty=2)

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='pce_recal_bm.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- pce_bm_pred$V1
y <- do.call(rbind,pce_bm_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#00441b',
     xaxt='n',xlim=c(0,40),ylim=c(0,40),pch=19,cex=1.5)

axis(1,at=seq(0,40,10),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,40,10),las=1)

segments(-1,-1,41,41,lwd=1.2,lty=2)

mtext("Predicted risk of MI or stroke at 10 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of MI or stroke at 10 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# GND and calibration slope
calib_wf <- GND.calib(pred=pce_wf$pce_risk_cal/100,tvar=pce_wf$mi_stroke_10y.t,
                      out=pce_wf$incd_mi_stroke_10y,groups=pce_wf$pce_cal_decile,
                      cens.t=pce_wf$censored,adm.cens=10)
print(calib_wf)
calib_bf <- GND.calib(pred=pce_bf$pce_risk_cal/100,tvar=pce_bf$mi_stroke_10y.t,
                      out=pce_bf$incd_mi_stroke_10y,groups=pce_bf$pce_cal_decile,
                      cens.t=pce_bf$censored,adm.cens=10)
print(calib_bf)
calib_wm <- GND.calib(pred=pce_wm$pce_risk_cal/100,tvar=pce_wm$mi_stroke_10y.t,
                      out=pce_wm$incd_mi_stroke_10y,groups=pce_wm$pce_cal_decile,
                      cens.t=pce_wm$censored,adm.cens=10)
print(calib_wm)
calib_bm <- GND.calib(pred=pce_bm$pce_risk_cal/100,tvar=pce_bm$mi_stroke_10y.t,
                      out=pce_bm$incd_mi_stroke_10y,groups=pce_bm$pce_cal_decile,
                      cens.t=pce_bm$censored,adm.cens=10)
print(calib_bm)

wf<- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce,data=pce_wf)
slope_wf <- c(wf$coefficients[1],confint(wf)[1],confint(wf)[2])
print(slope_wf)

bf<- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce,data=pce_bf)
slope_bf <- c(bf$coefficients[1],confint(bf)[1],confint(bf)[2])
print(slope_bf)

wm<- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce,data=pce_wm)
slope_wm <- c(wm$coefficients[1],confint(wm)[1],confint(wm)[2])
print(slope_wm)

bm<- coxph(Surv(mi_stroke_10y.t,incd_mi_stroke_10y) ~ pce,data=pce_bm)
slope_bm <- c(bm$coefficients[1],confint(bm)[1],confint(bm)[2])
print(slope_bm)

################################# Cumulative incidence of AF across strata of PCE risk
# Create strata variable
pce_all <- rbind(pce_wf,pce_bf,pce_wm,pce_bm)

pce_all[,pce_risk_strat := ifelse(pce_risk >= 7.5,'Elevated','Not elevated')]

# Model
mod <- prodlim(Hist(mi_stroke_10y.t,incd_mi_stroke_10y)~pce_risk_strat,data=pce_all)

# Plot
CairoPDF(file='km_pce_strat.pdf',height=3,width=3.7,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(4,2,1,1))
plot(mod,"cuminc",ylim=c(0,0.20),xlim=c(0,10), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.20,0.05),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,10,2),axis1.labels=as.character(seq(0,10,2)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c("#3f007d",'#bcbddc'), # color of curves
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.20, # legend parameters
     legend.cex=1.8,legend.legend=c("Elevated (\u22657.5%)","Not elevated (<7.5%)"), # more legend parameters
     atrisk.title='',atrisk.pos=-1,atrisk.line=c(2,3.5), # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4, # more N at risk parameters
     atrisk.times=c(0,5,10), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
mtext("Cumulative risk of MI or stroke (%)",side=2,line=-2,at=0.10,cex=2) # y-axis label
mtext("Years",side=1, line=-0.5,cex=2) # x-axis label
mtext('Stratum',side=1, line=0.5,cex=1.6,at=-2.1) # descriptor for N at risk
dev.off()

# Get overall CI and IR of MI/stroke
pce_all[,dummy := 1]
setDF(pce_all)
ci_af <- survivor(data=pce_all,risk_data="dummy",time='mi_stroke_10y.t',status='incd_mi_stroke_10y',eval.t=10)
setDT(pce_all)
ir_af <- ir(data=pce_all,strata="dummy",time='mi_stroke_10y.t',status='incd_mi_stroke_10y')

################################# Distribution plots
# Probability density plot
m <- pce_all[Dem.Gender.no_filter=='Male']
f <- pce_all[Dem.Gender.no_filter=='Female']

x <- list(v1=m$pce_risk,v2=f$pce_risk)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(0,60,10),expand=c(0,0),limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0,0.25,0.05),expand=c(0,0),limits=c(0,0.25)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Male','Female')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='Predicted Probability of MI or Stroke at 10 Years (%)',y='Density') 
ggsave('pce_pred_density.pdf',
       height=2,width=2.5,units='in',scale=4)

###### Calibrated
x <- list(v1=m$pce_risk_cal,v2=f$pce_risk_cal)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(0,60,10),expand=c(0,0),limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0,0.25,0.05),expand=c(0,0),limits=c(0,0.25)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Male','Female')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='Predicted Probability of MI or Stroke at 10 Years (%)',y='Density') 
ggsave('pce_pred_density_cal.pdf',
       height=2,width=2.5,units='in',scale=4)
