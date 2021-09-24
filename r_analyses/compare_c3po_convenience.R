# Script to compare convenience cohort to c3po

# Depends
library(data.table)
library(survival)
library(prodlim)
library(Cairo)
library(timeROC)

# Loads
load(file='c3po_charge.RData')
load(file='comparison_cohort_charge.RData')

# Distribution of CHARGE-AF
# Probability density plot
x <- list(v1=charge_45$charge_pred5,v2=cc_charge_45$charge_pred5)
data <- melt(x)

# Plot
ggplot() + geom_density(data=data,aes(x=value,fill=L1),alpha=0.55) + 
  scale_x_continuous(breaks=seq(0,20,2),expand=c(0,0),limits=c(0,20)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1),expand=c(0,0),limits=c(0,0.7)) +
  scale_fill_manual(values=c("#2b8cbe","#f03b20"),name='',labels=c('C3PO','Convenience')) +
  theme(panel.background=element_blank(),axis.line=element_line(color='black'),legend.position=c(0.80,0.90),
        axis.text=element_text(size=20,color='black'),plot.margin=unit(c(0.5,1,0.5,0.5),'cm'),
        axis.title.y = element_text(size=20,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=20),legend.text=element_text(size=20)) +
  labs(x='Predicted Probability of AF at 5 Years (%)',y='Density') 
ggsave('charge_compare_pred_density.pdf',
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

## Effect size
c3po_hr <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_std,data=charge_45)
cc_hr <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_std,data=cc_charge_45)

## Discrimination
charge <- timeROC(T=charge_45$af_5y.t, delta=charge_45$incd_af_5y,
                  marker=charge_45$charge_startfu,cause=1,times=4.999)
charge_cc <- timeROC(T=cc_charge_45$af_5y.t, delta=cc_charge_45$incd_af_5y,
                  marker=cc_charge_45$charge,cause=1,times=4.999)

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

boot_cc <- auc_boot(status='incd_af_5y',time='af_5y.t',response='charge',
                        data=cc_charge_45,times=4.999,runs=200)

charge <- timeROC(T=cc_charge_45$af_5y.t, delta=cc_charge_45$incd_af_5y,
                  marker=cc_charge_45$charge,cause=1,times=c(0,1,2,3,4,4.999))

cstat <- c(charge$AUC[length(charge$AUC)],
           charge$AUC[length(charge$AUC)] - 1.96*sd(boot_cc),
           charge$AUC[length(charge$AUC)] + 1.96*sd(boot_cc))

## Plotting
pdf(file='roc_charge_timeroc.pdf',height=4,width=4,
    pointsize=3)

# Plot settings
par(oma=c(1,1,1,1))
par(mar=c(4,4.8,1,1))

plot.new()

# Specifics
## Plot 1
plot(charge,4.999,add=T,col='#bd0026',lwd=1.2)

## Axes
axis(1,at=seq(1,0,-0.2),labels=as.character(seq(0,1,0.2)),cex.axis=1.6)
axis(2,at=seq(0,1,0.2),cex.axis=1.6,las=2)

## Labels
title(xlab='Specificity',line=2.5,cex.lab=1.8)
title(ylab='Sensitivity',line=3.5,cex.lab=1.8)

## Legend
legend(0.5,0.2,legend=c('CHARGE-AF'),col=c('#bd0026'),
       lty=1,lwd=1,pch=1,bty='n',cex=1.5)

## Stop
dev.off()

## Plotting
pdf(file='roc_charge_cc_timeroc.pdf',height=4,width=4,
    pointsize=3)

# Plot settings
par(oma=c(1,1,1,1))
par(mar=c(4,4.8,1,1))

plot.new()

# Specifics
## Plot 1
plot(charge,4.999,add=T,col='#bd0026',lwd=1.2)
par(new=T)
plot(charge_cc,4.999,add=T,col='#2b8cbe',lwd=1.2)

## Axes
axis(1,at=seq(1,0,-0.2),labels=c('1.0','0.8','0.6','0.4','0.2','0.0'),cex.axis=1.6)
axis(2,at=seq(0,1,0.2),cex.axis=1.6,las=2)

## Labels
title(xlab='1-Specificity',line=2.5,cex.lab=1.8)
title(ylab='Sensitivity',line=3.4,cex.lab=1.8)

## Legend
legend(0.5,0.2,legend=c('C3PO (0.783)','Convenience (0.781)'),col=c('#bd0026','#2b8cbe'),
       lty=1,lwd=1,pch=1,bty='n',cex=1.5)

## Stop
dev.off()

# Model
af_c3po <- prodlim(Hist(af_5y.t,incd_af_5y)~1,data=charge_45)
af_cc <- prodlim(Hist(af_5y.t,incd_af_5y)~1,data=cc_charge_45)

# Plot
CairoPDF(file='km_compare.pdf',height=3,width=3.5,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(5,2,1,1))
plot(af_c3po,"cuminc",ylim=c(0,0.08),xlim=c(0,5), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.08,0.01),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,5,1),axis1.labels=as.character(seq(0,5,1)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c("#bd0026"),
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.09, # legend parameters
     legend.cex=2.2,legend.legend="C3PO", # more legend parameters
     atrisk.title='',atrisk.pos=-0.5,atrisk.line=2.8, # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4,atrisk.labels='                     ', # more N at risk parameters
     atrisk.times=c(0,2.5,5), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
par(new=TRUE)
plot(af_cc,"cuminc",ylim=c(0,0.08),xlim=c(0,5), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.08,0.01),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,5,1),axis1.labels=as.character(seq(0,5,1)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c('#2b8cbe'),
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.09, # legend parameters
     legend.cex=2.2,legend.legend=c("Convenience"), # more legend parameters
     atrisk.title='',atrisk.pos=-0.5,atrisk.line=4, # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4,atrisk.labels='                     ', # more N at risk parameters
     atrisk.times=c(0,2.5,5), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
mtext("Cumulative risk of AF (%)",side=2,line=-1.5,at=0.04,cex=2) # y-axis label
mtext("Years",side=1, line=0.6,cex=2) # x-axis label
mtext('Sample',side=1, line=1.2,cex=1.6,at=-1.1) # descriptor for N at risk
mtext('Convenience',side=1, line=4,cex=1.6,at=-1.4)
mtext('C3PO',side=1, line=2.8,cex=1.6,at=-1)
segments(0.3,0.08,0.6,0.08,col='#bd0026',lty=1)
segments(0.3,0.074,0.6,0.074,col='#2b8cbe',lty=1)
mtext('Convenience',side=3, line=-2.95,cex=1.6,at=1.51)
mtext('C3PO',side=3, line=-1.5,cex=1.6,at=1.1)
dev.off()

## Cumulative risk across strata

# Create strata variable
charge_45[,charge_strat := ifelse(charge_pred5 >= 5,'High',
                                  ifelse(charge_pred5 >= 2.5,'Medium','Low'))]
cc_charge_45[,charge_strat := ifelse(charge_pred5 >= 5,'High',
                                      ifelse(charge_pred5 >= 2.5,'Medium','Low'))]

# Model
charge_strat <- prodlim(Hist(af_5y.t,incd_af_5y)~charge_strat,data=charge_45)
cc_charge_strat <- prodlim(Hist(af_5y.t,incd_af_5y)~charge_strat,data=cc_charge_45)

# Plot
CairoPDF(file='km_charge_strat.pdf',height=3,width=3.5,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(4,2,1,1))
plot(charge_strat,"cuminc",ylim=c(0,0.20),xlim=c(0,5), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.20,0.05),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,5,1),axis1.labels=as.character(seq(0,5,1)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c("#bd0026",'#fc4e2a','#fd8d3c'), # color of curves
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.2, # legend parameters
     legend.cex=2.2,legend.legend=c("High","Medium","Low"), # more legend parameters
     atrisk.title='',atrisk.pos=-0.5,atrisk.line=c(1,2.5,4), # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4, # more N at risk parameters
     atrisk.times=c(0,2.5,5), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
mtext("Cumulative risk of AF (%)",side=2,line=0.5,at=0.1,cex=2) # y-axis label
mtext("Years",side=1, line=-1.8,cex=2) # x-axis label
mtext('Stratum',side=1, line=-0.3,cex=1.6,at=-1) # descriptor for N at risk
dev.off()

# Plot
CairoPDF(file='cc_km_charge_strat.pdf',height=3,width=3.5,
         pointsize=5)
par(oma=c(3,1,1,1),mar=c(4,2,1,1))
plot(cc_charge_strat,"cuminc",ylim=c(0,0.20),xlim=c(0,5), # Remove cuminc if you want survival
     lwd=1.5, # width of lines
     background=F, # background horizontal lines, generally set to false
     axis2.at=seq(0,0.20,0.05),axis2.las=2,axis2.cex.axis=1.8, #y-axis labeling parameters
     axis1.at=seq(0,5,1),axis1.labels=as.character(seq(0,5,1)),axis1.padj=0.5,axis1.cex.axis=1.8, #x-axis labeling parameters
     col=c("#d0d1e6",'#74a9cf','#0570b0'), # color of curves
     atrisk.col='black',
     confint=FALSE, # whether you want CI on the curves
     legend.x=0,legend.y=0.20, # legend parameters
     legend.cex=2.2,legend.legend=c("High","Medium","Low"), # more legend parameters
     atrisk.title='',atrisk.pos=-0.5,atrisk.line=c(1,2.5,4), # position of the N at risk rows
     atrisk.cex=1.6,atrisk.interspace=1.4, # more N at risk parameters
     atrisk.times=c(0,2.5,5), # x-axis points where you want the N at risk to show
     xlab='',ylab='') # Empty axis labels, using mtext instead
mtext("Cumulative risk of AF (%)",side=2,line=0.5,at=0.1,cex=2) # y-axis label
mtext("Years",side=1, line=-1.8,cex=2) # x-axis label
mtext('Stratum',side=1, line=-0.3,cex=1.6,at=-1) # descriptor for N at risk
dev.off()

# Function to generate survival estimates per AF risk quantile
survivor <- function(data,risk_data,time,status,eval.t){
  est <- rep(NA,times=length(unique(data[,risk_data])))
  lower <- rep(NA,times=length(unique(data[,risk_data])))
  upper <- rep(NA,times=length(unique(data[,risk_data])))
  for (i in 1:length(unique(data[,risk_data]))){
    km <- survfit(Surv(data[data[,risk_data]==unique(data[,risk_data])[order(unique(data[,risk_data]))][i],time],
                       data[data[,risk_data]==unique(data[,risk_data])[order(unique(data[,risk_data]))][i],status]) ~ 1)
    est[i] <- 1-stepfun(km$time, c(1, km$surv))(eval.t)
    upper[i] <- 1-stepfun(km$time, c(1, km$lower))(eval.t)
    lower[i] <- 1-stepfun(km$time, c(1, km$upper))(eval.t)
  }
  return(data.frame(est=est,upper=upper,lower=lower))
}

charge_45[,dummy := 1]
cc_charge_45[,dummy := 1]

setDF(charge_45); setDF(cc_charge_45)
c3po_obv_1 <- survivor(data=charge_45,risk_data="dummy",time='af_5y.t',status='incd_af_5y',eval.t=5)
cc_obv_1 <- survivor(data=cc_charge_45,risk_data="dummy",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45); setDT(cc_charge_45)

## Calibration
# Quantile sorter
classifier <- function(risk,ncuts){
  cuts <- quantile(risk,probs=seq(0,1,1/ncuts))
  index <- rep(NA,length(risk))
  for (i in 1:(length(cuts)-1)){
    for (j in 1:length(risk)){
      index[j] <- ifelse(risk[j] >= cuts[i],i,index[j])}}
  return(index)
}

# Use classifier to classify scores into quantiles (size 10)
charge_45$charge_decile <- classifier(risk=charge_45$charge_pred5,ncuts=10)
cc_charge_45$charge_decile <- classifier(risk=cc_charge_45$charge_pred5,ncuts=10)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(charge_45); setDF(cc_charge_45)
c3po_obv <- survivor(data=charge_45,risk_data="charge_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
cc_obv <- survivor(data=cc_charge_45,risk_data="charge_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45); setDT(cc_charge_45)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
c3po_pred <- charge_45[,mean(charge_pred5),by="charge_decile"][order(charge_decile)]
cc_pred <- cc_charge_45[,mean(charge_pred5),by="charge_decile"][order(charge_decile)]

# Plots for visualization
pdf(file='cal_charge_c3po.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x_c3po <- c3po_pred$V1
y_c3po <- do.call(rbind,c3po_obv)[1,]*100

x_cc <- cc_pred$V1
y_cc <- do.call(rbind,cc_obv)[1,]*100

plot(x_c3po,y_c3po,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,25),ylim=c(0,25),pch=19,cex=1.5,bty='n',col='#bd0026')

axis(1,at=seq(0,25,5),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,25,5),las=1)

segments(-1,-1,26,26,lwd=1.2,lty=2)

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

# Plots for visualization
pdf(file='cal_charge_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x_cc <- cc_pred$V1
y_cc <- do.call(rbind,cc_obv)[1,]*100

plot(x_cc,y_cc,xlab='',ylab='',yaxt='n',bty='n',
     xaxt='n',xlim=c(0,25),ylim=c(0,25),pch=19,cex=1.5,bty='n',col='#2b8cbe')

axis(1,at=seq(0,25,5),cex.axis=1.7)
axis(2,cex.axis=1.7,at=seq(0,25,5),las=1)

segments(-1,-1,26,26,lwd=1.2,lty=2)

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.8,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.8,line=4.5)

dev.off()

##### Compare incidence rates
charge_45[,start_fu_yrs := start_fu/365.25]

# Categorize age
age_cat <- function(data,age_var){
  for (i in seq(55,85,5)){
  if (i != 85){
  data[get(age_var) >= i & get(age_var) < i+5, age_cat := paste0(i)]
  } else {
    data[get(age_var) >= i, age_cat := paste0(i)]
  }
} 
}

age_cat2 <- function(data,age_var){
  for (i in seq(55,85,10)){
    if (i != 85 & i != 55){
      data[get(age_var) >= i & get(age_var) < i+10, age_cat2 := paste0(i)]
    } else if (i != 55){
      data[get(age_var) >= i, age_cat2 := paste0(i)]
    } else {
      data[get(age_var) < i, age_cat2 := paste0('<',i)]
      data[get(age_var) >= i & get(age_var) < i+10, age_cat2 := paste0(i)]
    }
  } 
}

age_cat(data=charge_45,age_var='start_fu_yrs')
age_cat(data=cc_charge_45,age_var='age_startfu')

age_cat2(data=charge_45,age_var='start_fu_yrs')
age_cat2(data=cc_charge_45,age_var='age_startfu')

# Incidence rates per stratum
ir <- function(data,strata,time,status){
  out <- list()
  for (i in 1:length(unique(data[,get(strata)]))){
    subset <- data[get(strata)==unique(data[,get(strata)])[i]]
    events <- nrow(subset[get(status)==1])
    pt <- sum(subset[,get(time)])
    rate <- events/pt
    out[[i]] <- c(paste0(unique(data[,get(strata)])[i]),events,pt,rate*1000,(rate-1.96*(rate/sqrt(events)))*1000,
                  (rate+1.96*(rate/sqrt(events)))*1000)
  }
  return(do.call(rbind,out))
}

ir_cc_1 <- ir(data=cc_charge_45,strata='dummy',time='af_5y.t',status='incd_af_5y')
ir_c3po_1 <- ir(data=charge_45,strata='dummy',time='af_5y.t',status='incd_af_5y')

ir_cc <- ir(data=cc_charge_45,strata='age_cat',time='af_5y.t',status='incd_af_5y')
ir_c3po <- ir(data=charge_45,strata='age_cat',time='af_5y.t',status='incd_af_5y')

ir_cc2_m <- ir(data=cc_charge_45[sex_cd=='M'],strata='age_cat2',time='af_5y.t',status='incd_af_5y')
ir_c3po2_m <- ir(data=charge_45[Dem.Gender.no_filter=='Male'],strata='age_cat2',time='af_5y.t',status='incd_af_5y')

ir_cc2_f <- ir(data=cc_charge_45[sex_cd=='F'],strata='age_cat2',time='af_5y.t',status='incd_af_5y')
ir_c3po2_f <- ir(data=charge_45[Dem.Gender.no_filter=='Female'],strata='age_cat2',time='af_5y.t',status='incd_af_5y')

### GND
#######################################################################
# R FUNCTION TO CALCULATE GREENWOOD-NAM-D'AGOSTINO CALIBRATION TEST FOR SURVIVAL MODEL
# Most up-to date version of this code is available at http://ncook.bwh.harvard.edu/r-code.html
# Author: Olga Demler, BWH, HMS
# Version 2 - Updated 8/4/2015
# FOR MORE DETAILS SEE Demler, Paynter, Cook "Tests of Calibration and Goodness of Fit 
# in the Survival Setting" Stat Med 2015; 34(10):1659-80. PMID: 25684707
# TO RUN:
# GND.calib(pred,tvar,out,cens.t, groups, adm.cens)
# PARAMETERS:
# pred - PREDICTED PROBABILITIES OF AN EVENT CALCULATED FOR THE FIXED TIME WHICH IS THE SAME FOR ALL OBSERVATIONS (=adm.cens)
# out  - OUTCOME 0/1 1=EVENT
# cens.t - CENSORED/NOT CENSORED INDICATOR 1=CENSORED
# groups - GROUPING ASSIGNMENT FOR EACH OBSERVATION
# adm.cens - END OF STUDY TIME 
# REQUIRES AT LEAST 2 EVENTS PER GROUP, AT LEAST 5 EVENTS PER GROUP IS RECOMMENDED
# IF <2 EVENTS PER GROUP THEN QUITS
#######################################################################
kmdec=function(dec.num,dec.name, datain, adm.cens){
  stopped=0
  data.sub=datain[datain[,dec.name]==dec.num,]
  if (sum(data.sub$out)>1){
    avsurv=survfit(Surv(tvar,out) ~ 1, data=datain[datain[,dec.name]==dec.num,], error="g")
    avsurv.est=ifelse(min(avsurv$time)<=adm.cens,avsurv$surv[avsurv$time==max(avsurv$time[avsurv$time<=adm.cens])],1)
    
    avsurv.stderr=ifelse(min(avsurv$time)<=adm.cens,avsurv$std.err[avsurv$time==max(avsurv$time[avsurv$time<=adm.cens])],0)
    avsurv.stderr=avsurv.stderr*avsurv.est
    
    avsurv.num=ifelse(min(avsurv$time)<=adm.cens,avsurv$n.risk[avsurv$time==max(avsurv$time[avsurv$time<=adm.cens])],0)
    
  } else {
    return(c(0,0,0,0,stopped=-1))
  }
  
  if (sum(data.sub$out)<5) stopped=1
  c(avsurv.est, avsurv.stderr, avsurv.num, dec.num, stopped) 
}#kmdec

GND.calib = function(pred, tvar, out, cens.t, groups, adm.cens){
  
  tvar.t=ifelse(tvar>adm.cens, adm.cens, tvar)
  out.t=ifelse(tvar>adm.cens, 0, out)
  
  datause=data.frame(pred=pred, tvar=tvar.t, out=out.t, count=1, cens.t=cens.t, dec=groups)
  numcat=length(unique(datause$dec))
  groups=sort(unique(datause$dec))
  
  kmtab=matrix(unlist(lapply(groups,kmdec,"dec",datain=datause, adm.cens)),ncol=5, byrow=TRUE)
  
  if (any(kmtab[,5] == -1)) stop("Stopped because at least one of the groups contains <2 events. Consider collapsing some groups.")
  else if (any(kmtab[,5] == 1)) warning("At least one of the groups contains < 5 events. GND can become unstable.\ 
(see Demler, Paynter, Cook 'Tests of Calibration and Goodness of Fit in the Survival Setting' DOI: 10.1002/sim.6428) \
Consider collapsing some groups to avoid this problem.")
  
  hltab=data.frame(group=kmtab[,4],
                   totaln=tapply(datause$count,datause$dec,sum),
                   censn=tapply(datause$cens.t,datause$dec,sum),
                   numevents=tapply(datause$out,datause$dec,sum),
                   expected=tapply(datause$pred,datause$dec,sum),
                   kmperc=1-kmtab[,1], 
                   kmvar=kmtab[,2]^2, 
                   kmnrisk=kmtab[,3],
                   expectedperc=tapply(datause$pred,datause$dec,mean))
  
  hltab$kmnum=hltab$kmperc*hltab$totaln
  hltab$GND_component=ifelse(hltab$kmvar==0, 0,(hltab$kmperc-hltab$expectedperc)^2/(hltab$kmvar))
  
  print(hltab[c(1,2,3,4,10,5,6,9,7,11)], digits=4)
  
  c(df=numcat-1, chi2gw=sum(hltab$GND_component),pvalgw=1-pchisq(sum(hltab$GND_component),numcat-1))
}#GND.calib

charge_45[,censored := ifelse(incd_af_5y==1,0,1)]
cc_charge_45[,censored := ifelse(incd_af_5y==1,0,1)]

# GND and calibration slope
calib_c3po <- GND.calib(pred=charge_45$charge_pred5/100,tvar=charge_45$af_5y.t,
                      out=charge_45$incd_af_5y,groups=charge_45$charge_decile,
                      cens.t=charge_45$censored,adm.cens=5)

calib_cc <- GND.calib(pred=cc_charge_45$charge_pred5/100,tvar=cc_charge_45$af_5y.t,
                      out=cc_charge_45$incd_af_5y,groups=cc_charge_45$charge_decile,
                      cens.t=cc_charge_45$censored,adm.cens=5)

c3po <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_startfu,data=charge_45)
slope_c3po <- c(c3po$coefficients[1],confint(c3po)[1],confint(c3po)[2])

cc <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge,data=cc_charge_45)
slope_cc <- c(cc$coefficients[1],confint(cc)[1],confint(cc)[2])

####################### Re-calibration
## CHARGE SET
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

## CONVENIENCE
# Average beta
cc_charge_45[,avgbeta := mean(charge)]

# Fit survival model
res <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge, data=cc_charge_45)

### derive s0
# set km as a survival function using the average level of each factor
km <- survfit(res, data=data.frame(x1=mean(charge)),type="kaplan-meier")
# Set s0 as the survival coefficient of km
af_s0 <- summary(km, times=c(5))$surv
# Pred risk
cc_charge_45[,charge_pred5_cal := (1-(af_s0)^exp(charge - (avgbeta)))*100]
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
cc_charge_45$charge_cal_decile <- classifier(risk=cc_charge_45$charge_pred5_cal,ncuts=10)

### CALCULATE OBSERVED RISK IN EACH QUANTILE
setDF(charge_45); setDF(cc_charge_45)
charge_obv <- survivor(data=charge_45,risk_data="charge_cal_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
cc_charge_obv <- survivor(data=cc_charge_45,risk_data="charge_cal_decile",time='af_5y.t',status='incd_af_5y',eval.t=5)
setDT(charge_45); setDT(cc_charge_45)

### CALCULATE AVERAGE PREDICTED RISK IN EACH QUANTILE
charge_pred <- charge_45[,mean(charge_pred5_cal),by="charge_cal_decile"][order(charge_cal_decile)]
cc_charge_pred <- cc_charge_45[,mean(charge_pred5_cal),by="charge_cal_decile"][order(charge_cal_decile)]

# Plots for visualization
pdf(file='cal_charge_recalibrated_red.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- charge_pred$V1
y <- do.call(rbind,charge_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#bd0026',
     xaxt='n',xlim=c(0,45),ylim=c(0,45),pch=19,cex=1.5)

axis(1,at=seq(0,45,5),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,45,5),las=1)

segments(-1,-1,46,46,lwd=1.2,lty=2)

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.7,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.7,line=4.5)

dev.off()

# Plots for visualization
pdf(file='cal_charge_recalibrated_cc.pdf',height=4,width=4,
    pointsize=3)
par(mar=c(4,6,1,1))
par(oma=c(1,1,1,1))

x <- cc_charge_pred$V1
y <- do.call(rbind,cc_charge_obv)[1,]*100

plot(x,y,xlab='',ylab='',yaxt='n',bty='n',col='#2b8cbe',
     xaxt='n',xlim=c(0,45),ylim=c(0,45),pch=19,cex=1.5)

axis(1,at=seq(0,45,5),cex.axis=1.7)
axis(2,cex.axis=1.6,at=seq(0,45,5),las=1)

segments(-1,-1,46,46,lwd=1.2,lty=2)

mtext("Predicted risk of AF at 5 years (%)",side=1,cex=1.7,line=3)
mtext("Incidence of AF at 5 years (%)",side=2,cex=1.7,line=4.5)

dev.off()

# GND and calibration slope
calib_af <- GND.calib(pred=charge_45$charge_pred5_cal/100,tvar=charge_45$af_5y.t,
                      out=charge_45$incd_af_5y,groups=charge_45$charge_cal_decile,
                      cens.t=charge_45$censored,adm.cens=5)

af <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge_startfu,data=charge_45)
slope_af_cc <- c(af$coefficients[1],confint(af)[1],confint(af)[2])

calib_af_cc <- GND.calib(pred=cc_charge_45$charge_pred5_cal/100,tvar=cc_charge_45$af_5y.t,
                      out=cc_charge_45$incd_af_5y,groups=cc_charge_45$charge_cal_decile,
                      cens.t=cc_charge_45$censored,adm.cens=5)

cc_af <- coxph(Surv(af_5y.t,incd_af_5y) ~ charge,data=cc_charge_45)
slope_af <- c(cc_af$coefficients[1],confint(cc_af)[1],confint(cc_af)[2])