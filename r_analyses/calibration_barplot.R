library(Cairo)

### 1: Slopes
# Calibration barplot
## Input values
af_c3po <- c(0.77,0.75,0.79)
wf_c3po <- c(0.67,0.65,0.70)
bf_c3po <- c(0.60,0.53,0.67)
wm_c3po <- c(0.70,0.68,0.73)
bm_c3po <- c(0.88,0.77,1.00)

af_cc <- c(0.69,0.68,0.70)
wf_cc <- c(0.64,0.62,0.65)
bf_cc <- c(0.56,0.51,0.61)
wm_cc <- c(0.70,0.68,0.71)
bm_cc <- c(0.87,0.79,0.95)

# Plot 1 : ALL
CairoPDF(file='cal_barplot.pdf',height=3,width=4,
         pointsize=5)
par(oma=c(6,1,1,1),mar=c(2,5,3,1))
coords <- barplot(c(wf_c3po[1],wf_cc[1],
                    bf_c3po[1],bf_cc[1],
                    wm_c3po[1],wm_cc[1],
                    bm_c3po[1],bm_cc[1],
                    af_c3po[1],af_cc[1]),
                  col=c(rep(c('#2b8cbe','#bd0026'),4),'#2ca25f','#8856a7'),
                  yaxt='n',bty='n',border = NA,ylim=c(0.5,1),space=c(0.2,rep(c(0.2,0.5),3),0.2,3,0.2))
par(xpd=TRUE)

segments(0,1,max(coords)+0.5,1,lty=5,col='lightgray')
mtext("Calibration Slope",2,cex=1.5,line=2.8)

# Labels
mtext('CHARGE-AF',1,line=5.7,cex=1.5,at=(coords[9]+coords[10])/2)
mtext('All',1,line=3.5,cex=1.2,at=(coords[9]+coords[10])/2)
mtext('PCE',1,line=5.7,cex=1.5,at=(coords[4]+coords[5])/2)
mtext('White',1,line=3,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Women',1,line=4,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Black',1,line=3,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('Women',1,line=4,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('White',1,line=3,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Men',1,line=4,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Black',1,line=3,cex=1.2,at=(coords[7]+coords[8])/2)
mtext('Men',1,line=4,cex=1.2,at=(coords[7]+coords[8])/2)

# Y-axis
axis(2,at=seq(0.5,1,0.1),las=2,cex.axis=1.5,pos=0)
mtext('Number of individuals included (in 1,000s)',2,line=6,cex=1.8)

# Error bars and counts
lb <- c(wf_c3po[2],wf_cc[2],bf_c3po[2],bf_cc[2],wm_c3po[2],wm_cc[2],bm_c3po[2],bm_cc[2],af_c3po[2],af_cc[2])
ub <- c(wf_c3po[3],wf_cc[3],bf_c3po[3],bf_cc[3],wm_c3po[3],wm_cc[3],bm_c3po[3],bm_cc[3],af_c3po[3],af_cc[3])

# Looping function that calls barplot coords
n<-1
for (i in coords){
  arrows(x0=i,y0=lb[n],x1=i,y1=ub[n],angle=90,length=0.04,lwd=0.5,code=3)
  n <- n+1}

dev.off()

### 1: ICIs
# Calibration barplot
## Input values
af_c3po <- c(0.028,0.027,0.030)
wf_c3po <- c(0.018,0.017,0.020)
bf_c3po <- c(0.030,0.023,0.036)
wm_c3po <- c(0.024,0.022,0.027)
bm_c3po <- c(0.012,0,0.025)

af_cc <- c(0.03551639,0.03471266,0.03632012)
wf_cc <- c(0.032,0.031,0.034)
bf_cc <- c(0.046,0.040,0.053)
wm_cc <- c(0.041,0.038,0.043)
bm_cc <- c(0.028,0.018,0.037)

# Plot 1 : ALL
CairoPDF(file='cal_barplot_ici.pdf',height=3,width=4,
         pointsize=5)
par(oma=c(4,1,1,1),mar=c(2,5,3,1))
coords <- barplot(c(wf_c3po[1],wf_cc[1],
                    bf_c3po[1],bf_cc[1],
                    wm_c3po[1],wm_cc[1],
                    bm_c3po[1],bm_cc[1],
                    af_c3po[1],af_cc[1]),
                  col=c(rep(c('#2b8cbe','#bd0026'),4),'#2ca25f','#8856a7'),
                  yaxt='n',bty='n',border = NA,ylim=c(0,0.08),space=c(0.2,rep(c(0.2,0.5),3),0.2,3,0.2))

mtext("Calibration Error",2,cex=1.5,line=3.5)

# Labels
mtext('CHARGE-AF',1,line=5.7-2.2,cex=1.5,at=(coords[9]+coords[10])/2)
mtext('All',1,line=3.5-2.2,cex=1.2,at=(coords[9]+coords[10])/2)
mtext('PCE',1,line=5.7-2.2,cex=1.5,at=(coords[4]+coords[5])/2)
mtext('White',1,line=3-2.2,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Women',1,line=4-2.2,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Black',1,line=3-2.2,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('Women',1,line=4-2.2,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('White',1,line=3-2.2,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Men',1,line=4-2.2,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Black',1,line=3-2.2,cex=1.2,at=(coords[7]+coords[8])/2)
mtext('Men',1,line=4-2.2,cex=1.2,at=(coords[7]+coords[8])/2)

# Y-axis
axis(2,at=seq(0,0.08,0.02),las=2,cex.axis=1.5,pos=0)

# Legend
par(xpd=TRUE)
legend(x=max(coords)-4.2,y=0.083,legend=c("C3PO","Convenience"),
       lty=1,lwd=2,bty='n',cex=1.5,col=c('#8856a7','#2ca25f'),seg.len = 0.6)

# Error bars and counts
lb <- c(wf_c3po[2],wf_cc[2],bf_c3po[2],bf_cc[2],wm_c3po[2],wm_cc[2],bm_c3po[2],bm_cc[2],af_c3po[2],af_cc[2])
ub <- c(wf_c3po[3],wf_cc[3],bf_c3po[3],bf_cc[3],wm_c3po[3],wm_cc[3],bm_c3po[3],bm_cc[3],af_c3po[3],af_cc[3])

# Looping function that calls barplot coords
n<-1
for (i in coords){
  arrows(x0=i,y0=lb[n],x1=i,y1=ub[n],angle=90,length=0.04,lwd=0.5,code=3)
  n <- n+1}

dev.off()

### 1: Recalibrated ICIs
# Calibration barplot
## Input values
af_c3po <- c(0.019,0.018,0.021)
wf_c3po <- c(0.034,0.031,0.037)
bf_c3po <- c(0.057,0.050,0.064)
wm_c3po <- c(0.032,0.029,0.035)
bm_c3po <- c(0.010,0,0.024)

af_cc <- c(0.028,0.027,0.029)
wf_cc <- c(0.047,0.044,0.049)
bf_cc <- c(0.074,0.067,0.081)
wm_cc <- c(0.039,0.037,0.042)
bm_cc <- c(0.012,0.0026,0.022)

# Plot 1 : ALL
CairoPDF(file='cal_barplot_ici_recal.pdf',height=3,width=4,
         pointsize=5)
par(oma=c(4,1,1,1),mar=c(2,5,3,1))
coords <- barplot(c(wf_c3po[1],wf_cc[1],
                    bf_c3po[1],bf_cc[1],
                    wm_c3po[1],wm_cc[1],
                    bm_c3po[1],bm_cc[1],
                    af_c3po[1],af_cc[1]),
                  col=c(rep(c('#2b8cbe','#bd0026'),4),'#2ca25f','#8856a7'),
                  yaxt='n',bty='n',border = NA,ylim=c(0,0.08),space=c(0.2,rep(c(0.2,0.5),3),0.2,3,0.2))

mtext("Calibration Error",2,cex=1.5,line=3.5)

# Labels
mtext('CHARGE-AF',1,line=5.7-2.2,cex=1.5,at=(coords[9]+coords[10])/2)
mtext('All',1,line=3.5-2.2,cex=1.2,at=(coords[9]+coords[10])/2)
mtext('PCE',1,line=5.7-2.2,cex=1.5,at=(coords[4]+coords[5])/2)
mtext('White',1,line=3-2.2,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Women',1,line=4-2.2,cex=1.2,at=(coords[1]+coords[2])/2)
mtext('Black',1,line=3-2.2,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('Women',1,line=4-2.2,cex=1.2,at=(coords[3]+coords[4])/2)
mtext('White',1,line=3-2.2,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Men',1,line=4-2.2,cex=1.2,at=(coords[5]+coords[6])/2)
mtext('Black',1,line=3-2.2,cex=1.2,at=(coords[7]+coords[8])/2)
mtext('Men',1,line=4-2.2,cex=1.2,at=(coords[7]+coords[8])/2)

# Y-axis
axis(2,at=seq(0,0.08,0.02),las=2,cex.axis=1.5,pos=0)

# Error bars and counts
lb <- c(wf_c3po[2],wf_cc[2],bf_c3po[2],bf_cc[2],wm_c3po[2],wm_cc[2],bm_c3po[2],bm_cc[2],af_c3po[2],af_cc[2])
ub <- c(wf_c3po[3],wf_cc[3],bf_c3po[3],bf_cc[3],wm_c3po[3],wm_cc[3],bm_c3po[3],bm_cc[3],af_c3po[3],af_cc[3])

# Legend
par(xpd=TRUE)
legend(x=max(coords)-4.2,y=0.083,
       legend=c("C3PO","Convenience"),lty=1,lwd=2,seg.len = 0.6,
       bty='n',cex=1.5,col=c('#2ca25f','#8856a7'))

# Looping function that calls barplot coords
n<-1
for (i in coords){
  arrows(x0=i,y0=lb[n],x1=i,y1=ub[n],angle=90,length=0.04,lwd=0.5,code=3)
  n <- n+1}

dev.off()