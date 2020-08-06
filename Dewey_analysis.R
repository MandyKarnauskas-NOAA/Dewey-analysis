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
if ("vegan" %in% available.packages() == FALSE) { install.packages("vegan") } 
if ("viridis" %in% available.packages() == FALSE) { install.packages("viridis") } 
library(RColorBrewer)
library(fields)
library(vegan)
library(MASS)
library(viridis)

# Load data --------------------------------------------
#d <- read.table("Charter_PhotoCount_SouthFlorida_Keys.csv", header=T, sep=",")
d <- read.table("Charter_PhotoCount_NC_VA.csv", header=T, sep=",", quote = "")
head(d)
tail(d)
names(d)

table(d$region, useNA = "always")
table(d$marina, useNA = "always")
table(d$company, useNA = "always")
table(d$photo_type, useNA = "always")
table(d$photo_id, useNA = "always")
table(d$year, useNA = "always")

totalMahi <- rowSums(d[,grep("mahi", tolower(names(d)))], na.rm=T)
d <- d[,-grep("mahi", tolower(names(d)))]
d$mahi <- totalMahi

# Format dates -----------------------------------------
d$date <- as.Date(paste0(d$day, "/", sprintf("%02d", d$month)), "%d/%m")
d$doy <- as.numeric(strftime(d$date, format = "%j"))                        # convert to day of year

names(d)
names(d)[9:(ncol(d)-2)]

# Create list of top species ---------------------------
sort(colSums(d[9:(ncol(d)-2)], na.rm=T))

splis <- names(sort(colSums(d[9:(ncol(d)-2)], na.rm=T), decreasing=T))[1:8]

#spleg <- c("Mahi-bailers", "Mahi-gaffers","Wahoo", "Blackfin tuna", "Bonita", "Kingfish", "Yellowtail snapper", "Vermilion snapper")
spleg <- c("Mahi", "Tunas", "Tilefish", "Wahoo", "Triggerfish", "Kingfish", "Black seabass", "Amberjacks")

cbind(splis, spleg)

# Plot formatting specs ---------------------------------
# cols <- c("#00000050", "#00000099", "#FF000050", "#FFFF0050", "#00FF0050", "#00FFFF50", "#0000FF50", "#FF00FF50")
cols <- c("#00000030", "#FFFF0050", "#00FF0050", "#FF000050", "#00FFFF50", "#0000FF90", "#FF00FF90", "#00000099")
pchs <- c(17, 19, 19, 16, 19, 19, 19, 19)
cexs <- c(1, 1, 1, 1.5, 1, 1, 1, 1) 

cbind(splis, spleg, cols, pchs, cexs)
plot(rep(1, 8), 1:8, col = cols, pch = pchs, cex = cexs, axes=F)
text(rep(0.9, 8), 1:8, splis)

dlis <- c("1jan", "1mar", "1may", "1jul", "1sep", "1nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------
plot(d$doy, d$mahi,  xlab = "time of year", ylab = "number caught", 
     axes = F, ylim = c(0, max(d[9:(ncol(d)-2)] + 10, na.rm = T)), col = 0)

for (i in 1:8) { 
  points(d$doy, d[,which(names(d) == splis[i])], col = cols[i], pch = pchs[i], cex = cexs[i])                        
  } 

axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(2, las = 2); box()
legend("topleft", spleg, col = cols, pch = pchs, pt.cex = cexs, ncol = 1)


# Plot smoothed data ------------------------------------
plot(d$doy, d$mahi, ylim = c(0, 27), col = 0, 
     xlab = "time of year", ylab = "", axes = F)
mtext(side = 2, line = 2, "relative importance")
axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)]); box()
legend("topleft", spleg, lwd=3, col=cols)

for (i in 1:length(splis))  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  ks <- ksmooth(d$doy, y, "normal", bandwidth=40, range.x = c(1, 365), n.points = 365) 
  lines(ks$x, ks$y, col=cols[i], lwd=3)  
}

# Plot monthly barplot ----------------------------------
m1 <- c()
for (i in 1:length(splis))  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  m <- tapply(y, d$month, sum, na.rm=T)
  mp <- m / sum(m)
  m1 <- cbind(m1, mp)
}

barplot(t(m1), beside=T, col=1:8, names.arg = month.abb[1:11], 
        axes=F, #ylim=c(0, 1000), 
        ylab = "proportion of catch",
        main = "relative distribution of catch by month")
axis(2, las = 2)
#abline(h=0)
legend("topleft", spleg, col = 1:8, pch = 15, cex = 1.2)

# Plot correlations ----------------------------------
d1 <- d[,which(names(d) %in% splis)]
d1[is.na(d1)] <- 0
plot(d1)

dcor <- cor(d1, method="spearman")
dcor[which(dcor==1)] <- NA

par(mar=c(8,8,1,3))
image.plot(dcor, axes=F, 
           col = brewer.pal(n = 11, name = "RdBu"), 
           breaks=seq(-0.6, 0.6, length.out = 12))
axis(1, at=seq(0,1,length.out=8), lab=spleg, las=2)
axis(2, at=seq(0,1,length.out=8), lab=spleg, las=2)
mtext(side = 4, line = 1, "correlation coefficient")
box()


# NMDS ----------------------------------

d1 <- d[1:1069, 9:27]
d1[is.na(d1)] <- 0

d2 <- d[1:1069,]
d2 <- d2[-which(rowSums(d1) == 0),]
d1 <- d1[-which(rowSums(d1) == 0),]

d1 <- d1 + abs(rnorm(prod(dim(d1)), sd=0.01))

#for (i in 1:ncol(d1))  { 
#    d1[,i] <- d1[,i] / max(d1[,i])
#  }

d2$month[179] <- 6
mclass <- cut(d2$month, breaks = c(0, 5.5, 7.5, 9.5, 11.5))

d$reg2 <- NA
d2$reg2[which(d2$marina == "Hatteras Harbor Marina")] <- "Hatteras"
d2$reg2[which(d2$marina == "Oregon Inlet Fishing Center")] <- "Wanchese"
d2$reg2[which(d2$marina == "Virginia Beach Fishing Center")] <- "VA Beach"
d2$reg2[which(d2$marina == "Pirate's Cove Marina")] <- "Wanchese"
d2$reg2[which(d2$marina == "Sensational Sportfishing")] <- "Morehead"

d2$mon2 <- d2$month
d2$mon2[is.na(d2$mon2)] <- 6
d2$mon2[d2$mon2 <=4] <- 4
table(d2$mon2, useNA = "always")

dim(d1)

plot(colSums(d1))

d1.dist <- vegdist(d1, method = "manhattan")
pc <- isoMDS(d1.dist, k=2, trace=T, tol = 1e-3)

#pc <- metaMDS(d1d, k = 2, autotransform = F, trymax = 50)

x <- pc$points[,1]
y <- pc$points[,2]

plot(x, y, col = as.numeric(mclass), pch=16)
plot(x, y, col = as.numeric(d2$marina), pch=16)
plot(x, y, col = d2$year - 2018, pch=16)
plot(x, y, col = as.numeric(d2$region), pch=16)

dmar.ano  <- anosim(d1.dist, d2$marina)
dyr.ano  <- anosim(d1.dist, d2$year)
dreg.ano  <- anosim(d1.dist, d2$region)
dmon.ano  <- anosim(d1.dist, d2$mon2)
dreg2.ano  <- anosim(d1.dist, d2$reg2)
dmon2.ano  <- anosim(d1.dist, mclass)

summary(dmar.ano)
summary(dreg.ano)
summary(dreg2.ano)
summary(dyr.ano)
summary(dmon.ano)
summary(dmon2.ano)

# plot ANOSIM results -------------------------
par(mfrow=c(3,2), mex = 1.5, mar = c(9,3,1,1))
plot(dmar.ano, las=2, xlab="")
plot(dreg.ano, las=2, xlab="")

par(mex = 1.5, mar = c(4,3,1,1))
plot(dreg2.ano, las=2, xlab="")
plot(dyr.ano, las=1, xlab="")
plot(dmon.ano, las=1, xlab="", axes = F)
axis(1, at = 1:9, lab = c("Between", month.abb[4:11]), las = 2)
axis(2); box()
plot(dmon2.ano, las=2, xlab="", axes=F)
axis(1, at = 1:5, lab = c("Between", "Jan-May", "Jun-Jul", "Aug-Sep", "Oct-Nov"), las = 2)
axis(2); box()

# ordination plots -------------------

par(mfrow=c(3,2), mex = 1.5, mar = c(1,1,1,1))

cols <- viridis(5)
plot(x, y, col = cols[as.numeric(d2$marina)-1], pch=16) #, xlim = c(-1.5, 2))
ordiellipse(pc, d2$marina, col = cols, lwd=2, kind = "sd", label = F)
tab <- table(as.numeric(d2$marina)-1, d2$marina)
legend("topleft", ncol = 2, colnames(tab)[2:6], col = cols, pch = 16, cex=0.8)

cols <- c(1, 2)
plot(x, y, col = cols[as.numeric(d2$region)-1], pch=16)
ordiellipse(pc, d2$region, col = cols, lwd=2, kind = "sd", label = F)
legend("topleft", ncol = 2, c("NC", "VA"), col = cols, pch = 16)

cols <- 5:8
table(as.numeric(as.factor(d2$reg2)), d2$reg2)
plot(x, y, col = cols[as.numeric(as.factor(d2$reg2))], pch=16)
ordiellipse(pc, d2$reg2, col = cols, lwd=2, kind = "sd", label = F)
legend("topleft", ncol = 2, c("Hatteras", "Morehead", "VA Beach", "Wanchese"), col = cols, pch = 16)

cols <- c(3, 4)
plot(x, y, col = cols[d2$year - 2018], pch=16)
ordiellipse(pc, d2$year, col = cols, lwd=2, kind = "sd", label = F)
legend("topleft", ncol = 2, c("2019", "2020"), col = cols, pch = 16)

cols <- viridis(8)
plot(x, y, col = cols[(as.numeric(d2$mon2) - 3)], pch=16)
ordiellipse(pc, d2$mon2, col = cols, lwd=2, kind = "sd")
legend("topleft", ncol = 4, c(month.abb[4:11]), col = cols, pch = 16)

cols <- viridis(4)
plot(x, y, col = cols[as.numeric(mclass)], pch=16)
ordiellipse(pc, mclass, col = cols, lwd=2, kind = "sd", label = F)
legend("topleft", ncol = 4, c("Jan-May", "Jun-Jul", "Aug-Sep", "Oct-Nov"), col = cols, pch = 16)



# The end ------------------------------