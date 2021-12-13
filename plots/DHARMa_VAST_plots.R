##code adjusted from Zack Oyafuso : https://github.com/zoyafuso-NOAA/MS_OM_GoA/blob/e8e13133340aaf6c1a3a293b3c7d0fa888c79b59/ForMadison/appendix_VAST_output.R#L367
library(VAST)
library(ggplot2)
library(viridis)
library(ggpubr)

##load VAST model fit##
fit <- readRDS(paste0(getwd,"model_fit.RDS"))

###################################################################
######QQ PLOT AND RESIDUAL VS PREDICTED PLOTS ######################
###################################################################
dyn.load(dynlib("VAST_v9_2_0")) ##change "VAST_v9_2_0" to your compiled VAST cpp version number
dharmaRes = summary( fit, what="residuals" )

## create figures
par(mfrow=c(1,2))
gap::qqunif(dharmaRes$scaledResiduals, 
            pch = 2, 
            bty = "n",
            logscale = F, 
            col = "black", 
            cex = 0.6,
            cex.main = 1, 
            ann = F, 
            cex.axis = 1.5)

mtext(side = 1, line = 2, text = "Expected", cex = 1.5)
mtext(side = 2, line = 2.5, text = "Observed", cex = 1.5)

box()
box(which = "figure")
text(x = -0.31,
     y = 1.05,
     xpd = NA,
     cex = 1.5)

DHARMa::plotResiduals(dharmaRes,
                      rank = TRUE,
                      ann = F,
                      xlim = c(0, 1),
                      cex = 1.5)
mtext(side = 1,
      line = 2,
      text = "Rank-Transformed Model Predictions",
      cex = 1.5)
mtext(side = 2,
      line = 2,
      text = "Standardized Residual",
      cex = 1.5)
box(which = "figure")
