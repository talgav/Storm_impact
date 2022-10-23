# fitting2.r - replacing the fitting methods with functions, that are all
#  in PLBfunctions.r. Some axes limits may be chosen explicitly for the
#  simulated data set and so are not automated. 5th June 2015.



source("PLBfunctions.r")  

working_data_mle <- working_data[,c(3,4,13,14,18,19,22,23,25,29,21,30,31)]
working_data_mle <- working_data_mle[rep(1:nrow(working_data_mle), working_data_mle$Number_Ind),]
x <- subset(working_data_mle,SiteNo == "KATZA" & Trans_Location == "T"  & Weight >= 20)$Weight

log.x = log(x)                      # to avoid keep calculating
sum.log.x = sum( log.x ) 
xmin = min(x)
xmax = max(x)



# MLE (maximum likelihood method) calculations.

# Use analytical value of MLE b for PL model (Box 1, Edwards et al. 2007)
#  as a starting point for nlm for MLE of b for PLB model.

PL.bMLE = 1/( log(min(x)) - sum.log.x/length(x)) - 1
    
PLB.minLL =  nlm(negLL.PLB, p=PL.bMLE, x=x, n=length(x),
    xmin=xmin, xmax=xmax, sumlogx=sum.log.x) #, print.level=2 )

PLB.bMLE = PLB.minLL$estimate

# 95% confidence intervals for MLE method.

PLB.minNegLL = PLB.minLL$minimum

# Values of b to test to obtain confidence interval. For the real movement data
#  sets in Table 2 of Edwards (2011) the intervals were symmetric, so make a
#  symmetric interval here.

bvec = seq(PLB.bMLE - 0.5, PLB.bMLE + 0.5, 0.00001) 
    
PLB.LLvals = vector(length=length(bvec))  # negative log-likelihood for bvec
for(i in 1:length(bvec))
    {
        PLB.LLvals[i] = negLL.PLB(bvec[i], x=x, n=length(x), xmin=xmin,
            xmax=xmax, sumlogx=sum.log.x)   
    }
critVal = PLB.minNegLL  + qchisq(0.95,1)/2
                    # 1 degree of freedom, Hilborn and Mangel (1997) p162.
bIn95 = bvec[ PLB.LLvals < critVal ]
                    # b values in 95% confidence interval
PLB.MLE.bConf = c(min(bIn95), max(bIn95))
if(PLB.MLE.bConf[1] == min(bvec) | PLB.MLE.bConf[2] == max(bvec))
  { windows()
    plot(bvec, PLB.LLvals)
    abline(h = critVal, col="red")
    stop("Need to make bvec larger - see R window")   # Could automate
  }

# To plot rank/frequency style plot:
# xLim = c(0.8*xmin, 10^ceiling(log10(xmax)))    # and use xaxs="i"
# xLim = c(xmin, xmax) 
# yLim = c(1, n)
plot(sort(x, decreasing=TRUE), 1:length(x), log="xy",
     xlab=expression(paste("Values, ", italic(x))),
     ylab=expression( paste("Number of ", values >= x), sep=""),
     mgp=mgpVals, xlim = c(xmin, xmax), ylim = c(1, n), axes=FALSE)
xLim = 10^par("usr")[1:2]
yLim = 10^par("usr")[3:4]

logTicks(xLim, yLim, xLabelSmall = c(5, 50, 500))   # Tick marks.

x.PLB = seq(min(x), max(x), length=1000)     # x values to plot PLB
y.PLB = (1 - pPLB(x = x.PLB, b = PLB.bMLE, xmin = min(x.PLB),
                  xmax = max(x.PLB))) * length(x)    
lines(x.PLB, y.PLB, col="red") #, lty=5)
legJust(c("(h) MLE", paste("b=", signif(PLB.bMLE, 3), sep="")), inset=inset,
        logxy=TRUE)
# legend("topright", c(" (h) MLE", paste("b=", signif(PLB.bMLE, 3), sep="")),
#                     bty="n", inset=inset)


# legend("topright", paste("(h) MLE exponent=", signif(PLB.bMLE, 3)), bty="n",
#        inset=inset)

# To add the curves at the limits of the 95% confidence interval:
#for(i in c(1, length(bIn95)))       # for(i in 1:length(bIn95))  to see all vals
#    {
#      lines(x.PLB, (1 - pPLB(x = x.PLB, b = bIn95[i], xmin = min(x.PLB),
#                  xmax = max(x.PLB))) * length(x), col="red", lty=3)
#    }  

dev.off()


# Standard histogram, but with a break in the y-axis. See PLBfunctions.r for
#  code.
postscript("standHist.eps", height = 2.7, width = figwidth/2,
           horizontal=FALSE,  paper="special")  #  height=4, width=4   was 7,4
par(mai=c(0.6, 0.6, 0.2, 0.3))
# mgpVals = c(2, 0.5, 0)            # mgp values   2.0, 0.5, 0
gap.barplot.cust(hLlin.list$counts, gap=c(9,980),
                 ytics = c(seq(0, 8, by=4), seq(980, 988, by=4)),
                                        #  = c(0, 20, 40, 860, 880, 900),
                 midpoints = hLlin.list$mids,
                 breakpoints = hLlin.list$breaks,
                 xlim = c(-10,max(hLlin.list$breaks)+10),
                 yaxs="i",
                 ylim= c(0, 17),  # max = max(y) - gap[2] + gap[1] + some space
                 col=rep("grey", length(hLlin.list$counts)),
                 xaxs="i", xlab=expression(paste("Values, ", italic(x))),
                 ylab="Count in each bin", mgp=c(1.8,0.5,0))
                               # , default: mgp=c(3,1,0))# , mgp=c(1.8,0.5,0))
# mgp is margin line in mex units, for axis title, axis labels and axis line
dev.off()


# Plotting fit of MLE method on linear-log axes, to help explain in the
#  Appendix why the red curve on log-log axes (Figure 2h) does not pass
#  through the maximum data point. This will be Figure A.1 in Appendix.

postscript("fitting2aMLElinlog.eps", height = 5.4, width = figwidth,
           horizontal=FALSE, paper="special")
par(mai=c(0.8, 0.8, 0.2, 0.3))
mgpVal = c(2, 0.5, 0)
plot(sort(x, decreasing=TRUE), 1:length(x), log="x",
     xlab=expression(paste("Values, ", italic(x))),
     ylab=expression( paste("Number of ", values >= x), sep=""),
     xlim = c(xmin, xmax), ylim = c(0, n), axes=FALSE,  mgp=mgpVal) 
xLim = 10^par("usr")[1:2]
# yLim = 10^par("usr")[3:4]
yLim = NULL


logTicks(xLim, yLim=NULL, xLabelSmall = c(5, 50, 500), mgp=mgpVal) # Tick marks.

yBig = c(0, 500, 1000)
# Big labelled:
axis(2, at= yBig, labels = yBig, mgp=mgpVal)
# Small unlabelled:
axis(2, seq(yBig[1], yBig[length(yBig)], by=100), labels=rep("", 11), tcl=-0.2,
     mgp=mgpVal)
# Small labelled:
# axis(2, at=yLabelSmall, labels=yLabelSmall, mgp=mgpVal, tcl=tclSmall)
 

x.PLB = seq(min(x), max(x), length=1000)     # x values to plot PLB
y.PLB = (1 - pPLB(x = x.PLB, b = PLB.bMLE, xmin = min(x.PLB),
                  xmax = max(x.PLB))) * length(x)    
lines(x.PLB, y.PLB, col="red") #, lty=5)
# legJust(c("MLE", paste("b=", signif(PLB.bMLE, 3), sep="")), inset=inset,
#        logxy=TRUE)

dev.off()

save.image(file = "fitting2.RData")


