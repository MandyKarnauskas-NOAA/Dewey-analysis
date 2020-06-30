#########################################################
#  M. Karnauskas - 30 June 2020                         #
#  Code for running the "Dewey analysis"                #
#  Processing of charter boat social media              #
#########################################################

# Set directory 
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/Dewey_analysis")

# Install libraries ------------------------------------
if ("RColorBrewer" %in% available.packages() == FALSE) { install.packages("RColorBrewer") } 
if ("fields" %in% available.packages() == FALSE) { install.packages("fields") } 
library(RColorBrewer)
library(fields)

# Load data --------------------------------------------
d <- read.table("Charter_PhotoCount.csv", header=T, sep=",")

# Format dates -----------------------------------------
d$date <- as.Date(paste0(d$day, "/", sprintf("%02d", d$month)), "%d/%m")
d$doy <- as.numeric(strftime(d$date, format = "%j"))                        # convert to day of year

# Create list of top species ---------------------------
sort(colSums(d[9:39], na.rm=T))
splis <- c("mahi_bailers", "mahi_gaffers","wahoo", "Blackfin.Tuna", "Bonita", "Kingfish", 
           "Yellowtail.Snapper", "Vermillion.Snapper")
spleg <- c("Mahi-bailers", "Mahi-gaffers","Wahoo", "Blackfin tuna", "Bonita", "Kingfish", 
           "Yellowtail snapper", "Vermilion snapper")

d$mahi_gaffers <- d$M_mahi_gaffers + d$F_mahi_gaffers

# Plot formatting specs ---------------------------------
cols <- c("#00000050", "#00000099", "#FF000050", "#FFFF0050", "#00FF0050", "#00FFFF50", "#0000FF50", "#FF00FF50")
dlis <- c("1jan", "1mar", "1may", "1jul", "1sep", "1nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------
plot(d$doy, d$mahi_bailers,  xlab="day of year", ylab="number caught", axes=F, ylim=c(0, max(d[9:39], na.rm=T)),
                                    col=cols[1], pch=17, cex=0.7)
points(d$doy, d$mahi_gaffers,       col=cols[2], pch=17)
points(d$doy, d$wahoo,              col=cols[3], pch=16, cex=1.5)
points(d$doy, d$Blackfin.Tuna,      col=cols[4], pch=19)
points(d$doy, d$Bonita,             col=cols[5], pch=19)
points(d$doy, d$Kingfish,           col=cols[6], pch=19)
points(d$doy, d$Yellowtail.Snapper, col=cols[7], pch=19)
points(d$doy, d$Vermillion.Snapper, col=cols[8], pch=19)

axis(1, at=dy, lab=month.abb[c(1,3,5,7,9,11)]); axis(2, las=2); box()
legend("topleft", spleg, col=cols, pch=c(17, 17, 16, rep(19, 5)), pt.cex=c(0.7, 1, 1.5, rep(1,5)))


# Plot smoothed data ------------------------------------
plot(d$doy, d$Bonita, ylim=c(0,17), col=0, 
     xlab="day of year", ylab="", axes=F)
mtext(side=2, line=2, "relative importance")
axis(1, at=dy, lab=month.abb[c(1,3,5,7,9,11)]); box()
legend("topleft", spleg, lwd=3, col=cols)

for (i in 1:length(splis))  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  ks <- ksmooth(d$doy, y, "normal", bandwidth=40) 
  lines(ks$x, ks$y, col=cols[i], lwd=3)  
}


# Plot monthly barplot ----------------------------------
m1 <- c()
for (i in 1:length(splis))  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  m <- tapply(y, d$month, sum, na.rm=T)
  m1 <- cbind(m1, m)
}

barplot(t(m1), beside=T, col=1:8, names.arg = month.abb[1:11], las=2, ylim=c(0,200))
abline(h=0)
legend("topleft", spleg, col=1:8, pch=15)


# Plot correlations ----------------------------------
d1 <- d[,which(names(d) %in% splis)]
d1[is.na(d1)] <- 0
plot(d1)

dcor <- cor(d1, method="spearman")
dcor[which(dcor==1)] <- NA

par(mar=c(8,8,1,1))
image.plot(dcor, axes=F, col = brewer.pal(n = 11, name = "RdBu"), breaks=seq(-0.6, 0.6, length.out=12))
axis(1, at=seq(0,1,length.out=8), lab=spleg, las=2)
axis(2, at=seq(0,1,length.out=8), lab=spleg, las=2)
box()


# The end ------------------------------