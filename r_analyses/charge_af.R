# Script to generate CHARGE-AF analysis sets using C3PO data

# Dependencies
library(data.table)
library(plyr)
library(stringr)
library(rms)
library(survival)
library(ggplot2)
library(reshape2)
library(timeROC)
library(prodlim)
library(Cairo)

# Source helper functions
source(file='~/functions/functions.R')

# Load latest wide file
af_wide <- fread(file='wide_with_af.csv')

# Cast ages
numerics <- c('start_fu','last_encounter','Dem.Date_Of_Death_Age.no_filter',
              'inpatient_hf_age')
for (j in numerics){set(af_wide,j=j,value=as.numeric(str_extract(af_wide[[j]],'\\d+')))}

# Cast continuous variables
numerics <- c('start_fu_Height','start_fu_Weight','start_fu_Systolic',
              'start_fu_Diastolic')
for (j in numerics){set(af_wide,j=j,value=as.numeric(af_wide[[j]]))}

# Define AF outcomes
## 5Y
af_wide[,':='(af = ifelse(!is.na(af_age),1,0),
              incd_af = ifelse(c(!is.na(af_age) & !is.na(start_fu) & (af_age > start_fu) & (af_age <= global_censor_age)),1,0),
              incd_af_5y = ifelse(c(!is.na(af_age) & !is.na(start_fu) & (af_age > start_fu) 
                                    & ((af_age - start_fu) <= 1826.25) & (af_age <= global_censor_age)),1,0),
              prev_af = ifelse(c(!is.na(af_age) & !is.na(start_fu) & (af_age <= start_fu)),1,0))]

af_wide[,':='(af_5y.t = (ifelse(incd_af_5y==1,(af_age - start_fu),
                               pmin((last_encounter - start_fu),(global_censor_age - start_fu),
                                    (Dem.Date_Of_Death_Age.no_filter-start_fu),1826.25,na.rm=T)))/365.25)]

## Prevalent HF
af_wide[,prev_hf := ifelse(c(!is.na(inpatient_hf_age) & !is.na(start_fu) & (inpatient_hf_age <= start_fu)),1,0)]

###################################################### A. CHARGE-AF
# Calculate necessary CHARGE-AF variables
af_wide[,':='(age5 = (start_fu/(365.25*5)), 
              ht_10 = ht_cm/10, wt_15 = wt_kg/15,
              sbp20 = sbp/20, dbp10 = dbp/10)]

# Calculate CHARGE-AF
af_wide[,charge_startfu := age5*0.508 + (race_white==1)*0.465 + ht_10*0.248 + wt_15*0.115 + sbp20*0.197
            +dbp10*(-0.101)+ (!is.na(current_smoker_startfu))*0.359 + start_fu_hypertension_med*0.349
            +(start_fu_dm==1)*0.237 + (prev_hf==1)*0.701 + (prev_mi==1)*0.496]

# Tally exclusions
complete_case <- af_wide[!is.na(charge_startfu)] 
no_af <- complete_case[prev_af==0]
charge_set <- no_af[af_5y.t > 0]
charge_45 <- charge_set[(start_fu/365.25) >= 46] 

# Standardize the score
charge_45[,":="(charge_std = (charge_startfu - mean(charge_startfu))/sd(charge_startfu))]

# Convert to probability of AF at 5 years (see Alonso et al. 2013 for formula)
charge_45[,":="(charge_pred5 = (1-0.9718412736^exp(charge_startfu-12.5815600))*100)]

## Distribution plots
# Probability density plot
m <- charge_45[Dem.Gender.no_filter=='Male']
f <- charge_45[Dem.Gender.no_filter=='Female']

x <- list(v1=m$charge_pred5,v2=f$charge_pred5)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(0,20,2),expand=c(0,0),limits=c(0,20)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1),expand=c(0,0),limits=c(0,0.7)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Male','Female')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='Predicted Probability of AF at 5 Years (%)',y='Density') 
ggsave('charge_pred_density.pdf',
       height=2,width=2.5,units='in',scale=4)

## Distribution plots
# Score density plot
x <- list(v1=m$charge_startfu,v2=f$charge_startfu)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(8,18,2),expand=c(0,0),limits=c(8,18)) +
  scale_y_continuous(breaks=seq(0,0.4,0.1),expand=c(0,0),limits=c(0,0.4)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Male','Female')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='CHARGE-AF Score',y='Density') 
ggsave('charge_density.pdf',
       height=2,width=2.5,units='in',scale=4)

################################# Discrimination
# Bootstrap function
boot <- function(time,status,response,data,runs,size=nrow(data)){
  out <- cbind(rep(NA,times=runs),rep(NA,times=runs))
  for (i in 1:runs){
    sample <- data[sample(1:nrow(data),size=size,replace=TRUE),]
    out[i,1] <- cph(Surv(sample[,time],sample[,status]) ~ sample[,response],data=sample)$stats['R2']
    out[i,2] <- concordance(coxph(Surv(sample[,time],sample[,status]) ~ sample[,response],data=sample))$concordance
    print(paste0('run ',i,' complete'))
  }
  return(out)
}

af <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_std,data=charge_45)
HR_af <- c(exp(af$coefficients[1]),exp(confint(af)[1]),exp(confint(af)[2]))
print(HR_af)
cstat_af <- c(summary(af)$concordance[1],summary(af)$concordance[1]-1.96*summary(af)$concordance[2],
                 summary(af)$concordance[1]+1.96*summary(af)$concordance[2])
print(cstat_af)

# Use classifier to classify scores into quantiles (size 10)
charge_45$charge_decile <- classifier(risk=charge_45$charge_pred5,ncuts=10)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(charge_45)
charge_obv <- survivor(data=charge_45,risk_data="charge_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
charge_pred <- charge_45[,mean(charge_pred5),by="charge_decile"][order(charge_decile)]

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

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

charge_45[,censored := ifelse(incd_af_5y==1,0,1)]

# GND and calibration slope
calib_af <- GND.calib(pred=charge_45$charge_pred5/100,tvar=charge_45$af_5y.t,
                         out=charge_45$incd_af_5y,groups=charge_45$charge_decile,
                         cens.t=charge_45$censored,adm.cens=5)
print(calib_af)

af <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_startfu,data=charge_45)
slope_af <- c(af$coefficients[1],confint(af)[1],confint(af)[2])
print(slope_af)

####################### Re-calibration
### Calculate predicted risk
# Average beta
charge_45[,avgbeta := mean(charge_startfu)]

# Fit survival model
res <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_startfu, data=charge_45)

### derive s0
# set km as a survival function using the average level of each factor
km <- survfit(res, data=data.frame(x1=mean(charge_startfu)),type="kaplan-meier")
# Set s0 as the survival coefficient of km
af_s0 <- summary(km, times=c(5))$surv
# Pred risk
charge_45[,charge_pred5_cal := (1-(af_s0)^exp(charge_startfu - (avgbeta)))*100]

### Now repeat calibration measures

## Distribution plots
# Probability density plot
m <- charge_45[Dem.Gender.no_filter=='Male']
f <- charge_45[Dem.Gender.no_filter=='Female']

x <- list(v1=m$charge_pred5_cal,v2=f$charge_pred5_cal)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(0,40,5),expand=c(0,0),limits=c(0,40)) +
  scale_y_continuous(breaks=seq(0,0.25,0.05),expand=c(0,0),limits=c(0,0.25)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('Male','Female')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='Predicted Probability of AF at 5 Years (%)',y='Density') 
ggsave('charge_pred_density_cal.pdf',
       height=2,width=2.5,units='in',scale=4)

# Use classifier to classify scores into quantiles (size 10)
charge_45$charge_cal_decile <- classifier(risk=charge_45$charge_pred5_cal,ncuts=10)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(charge_45)
charge_obv <- survivor(data=charge_45,risk_data="charge_cal_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
charge_pred <- charge_45[,mean(charge_pred5_cal),by="charge_cal_decile"][order(charge_cal_decile)]

# Plots for visualization
pdf(file='cal_charge_recalibrated.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- charge_pred$V1
y <- do.call(rbind,charge_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2b83ba',
     xaxt='n',xlim=c(0,40),ylim=c(0,40),pch=19,cex=1.5)

axis(1,at=seq(0,40,10),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,40,10),las=1)

segments(-1,-1,41,41,lwd=1.2,lty=2)

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.7,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.7,line=4.5)

dev.off()

# GND and calibration slope
calib_af <- GND.calib(pred=charge_45$charge_pred5_cal/100,tvar=charge_45$af_5y.t,
                      out=charge_45$incd_af_5y,groups=charge_45$charge_cal_decile,
                      cens.t=charge_45$censored,adm.cens=5)
print(calib_af)

# Save out
save(charge_45,file='charge.RData')

# Discrimination and plots using timeROC

# Bootstrap function
auc_boot <- function(status,time,data,response,times=5,runs=200){
  out <- list()
  for (i in 1:runs){
    replicate <- data[sample(1:nrow(data),size=nrow(data),replace=TRUE)]
    out[[i]] <- timeROC(T=replicate[,get(time)],delta=replicate[,get(status)],marker=replicate[,get(response)],
                        cause=1,weighting='marginal',times=times,iid=FALSE)$AUC[2]
    if (i %% 50 == 0 ){print(paste0("I just finished ",i," out of ",runs," runs!"))}
  }
  return(do.call(rbind,out))
}

boot_charge <- auc_boot(status='incd_af_5y',time='af_5y.t',response='charge_startfu',
                       data=charge_45,times=4.999,runs=200)

charge <- timeROC(T=charge_45$af_5y.t, delta=charge_45$incd_af_5y,
                  marker=charge_45$charge_startfu,cause=1,times=c(0,1,2,3,4,4.999))

cstat <- c(charge$AUC[length(charge$AUC)],
           charge$AUC[length(charge$AUC)] - 1.96*sd(boot_charge),
           charge$AUC[length(charge$AUC)] + 1.96*sd(boot_charge))
print(cstat)

## Plotting
pdf(file='roc_charge_timeroc.pdf',height=4,width=4,
    pointsize=3)

# Plot settings
par(oma=c(1,1,1,1))
par(mar=c(4,4.8,1,1))

plot.new()

# Specifics
## Plot 1
plot(charge,4.999,add=T,col='#2b83ba',lwd=1.2)

## Axes
axis(1,at=seq(1,0,-0.2),labels=c('1.0','0.8','0.6','0.4','0.2','0.0'),cex.axis=1.6)
axis(2,at=seq(0,1,0.2),cex.axis=1.6,las=2)

## Labels
title(xlab='1 - Specificity',line=2.5,cex.lab=1.8)
title(ylab='Sensitivity',line=3.2,cex.lab=1.8)

## Legend
legend(0.5,0.2,legend=c('CHARGE-AF (0.783)'),col=c('#2b83ba'),
       lty=1,lwd=1,pch=1,bty='n',cex=1.5)

## Stop
dev.off()

################################# Cumulative incidence of AF across strata of CHARGE risk
# Create strata variable
charge_45[,charge_strat := ifelse(charge_pred5 >= 5,'High',
                                   ifelse(charge_pred5 >= 2.5,'Medium','Low'))]
charge_45[,charge_strat_cal := factor(ifelse(charge_pred5_cal >= 5,'High',
                                    ifelse(charge_pred5_cal >= 2.5,'Medium','Low')),levels=c('High','Medium','Low'))]

# Model
charge_af <- prodlim(Hist(af_5y.t,incd_af_5y)~charge_strat_cal,data=charge_45)

# Plot
CairoPDF(file='km_charge_cal.pdf',height=3,width=3.5,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(4,2,1,1))
plot(charge_af,"cuminc",ylim=c(0,0.15),xlim=c(0,5), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.15,0.05),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,5,1),axis1.labels=as.character(seq(0,5,1)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c("#023858",'#74a9cf','#d0d1e6'), # color of curves
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.15, # legend parameters
     legend.cex=1.8,legend.legend=c("High (\u22655%)","Medium (\u22652.5-5%)","Low (<2.5%)"), # more legend parameters
     atrisk.title='',atrisk.pos=-0.5,atrisk.line=c(1,2.5,4), # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4, # more N at risk parameters
     atrisk.times=c(0,2.5,5), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
mtext("Cumulative risk of AF (%)",side=2,line=0.5,at=0.075,cex=2) # y-axis label
mtext("Years",side=1, line=-1.8,cex=2) # x-axis label
mtext('Stratum',side=1, line=-0.3,cex=1.6,at=-1) # descriptor for N at risk
dev.off()

# Get overall CI and IR of AF
charge_45[,dummy := 1]
setDF(charge_45)
ci_af <- survivor(data=charge_45,risk_data="dummy",time='af_5y.t',status='incd_af_5y',eval.t=5)
print(ci_af)
setDT(charge_45)
ir_af <- ir(data=charge_45,strata="dummy",time='af_5y.t',status='incd_af_5y')
print(ir_af)

