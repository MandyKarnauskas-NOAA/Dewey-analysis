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
library(RColorBrewer)
library(fields)
library(vegan)

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
spleg <- c("Mahi", "Tunas", "Wahoo", "Triggerfish", "Tilefish", "Kingfish", "Amberjacks", "Black seabass")

cbind(splis, spleg)

# Plot formatting specs ---------------------------------
# cols <- c("#00000050", "#00000099", "#FF000050", "#FFFF0050", "#00FF0050", "#00FFFF50", "#0000FF50", "#FF00FF50")
cols <- c("#00000030", "#FFFF0050", "#FF000050", "#00FF0050", "#00FFFF50", "#0000FF50", "#FF00FF50", "#00000099")
dlis <- c("1jan", "1mar", "1may", "1jul", "1sep", "1nov")
dy <- as.numeric(strftime(as.Date(dlis, "%d%b"), format = "%j"))

# Plot raw data -----------------------------------------
plot(d$doy, d$mahi,  xlab = "time of year", ylab = "number caught", 
     axes = F, ylim = c(0, max(d[9:(ncol(d)-2)] + 10, na.rm = T)), col = 0)

points(d$doy, d[,which(names(d) == splis[1])], col = cols[1], pch = 17)                        
points(d$doy, d[,which(names(d) == splis[2])], col = cols[2], pch = 19)
points(d$doy, d[,which(names(d) == splis[3])], col = cols[3], pch = 16, cex = 1.5)
points(d$doy, d[,which(names(d) == splis[4])], col = cols[4], pch = 19)
points(d$doy, d[,which(names(d) == splis[5])], col = cols[5], pch = 19)
points(d$doy, d[,which(names(d) == splis[6])], col = cols[6], pch = 19)
points(d$doy, d[,which(names(d) == splis[7])], col = cols[7], pch = 19)
points(d$doy, d[,which(names(d) == splis[8])], col = cols[8], pch = 19)

axis(1, at = dy, lab = month.abb[c(1,3,5,7,9,11)])
axis(2, las = 2); box()
legend("topleft", spleg, col = cols, 
       pch = c(17, 19, 16, rep(19, 5)), 
       pt.cex=c(0.7, 1, 1.5, rep(1,5)), ncol = 1)


# Plot smoothed data ------------------------------------
plot(d$doy, d$mahi, ylim = c(0, 27), col = 0, 
     xlab = "time of year", ylab = "", axes = F)
mtext(side=2, line=2, "relative importance")
axis(1, at=dy, lab=month.abb[c(1,3,5,7,9,11)]); box()
legend("topleft", spleg, lwd=3, col=cols)

for (i in 1:length(splis))  { 
  y <- d[,which(names(d) == splis[i])]
  y[is.na(y)] <- 0
  ks <- ksmooth(d$doy, y, "normal", bandwidth=25, range.x = c(1, 365), n.points = 365) 
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
d1 <- d[,which(names(d) %in% splis)]
d1[is.na(d1)] <- 0

#d1 <- d1[which(d$marina == "Oregon Inlet Fishing Center"),]

#d1 <- d1[-which(rowSums(d1) == 0),]
#d1$mahi <- d1$mahi / 100
#d1$Tuna.Complex <- d1$Tuna.Complex / 100
dim(d1)

plot(colSums(d1))


dm <- distance(d1, "bray")
out <- nmds(dm, mindim = 2, maxdim = 2)

#d1 <- scale(d1)

pc <- metaMDS(d1, distance = "bray", k = 2, try = 20, trymax = 30,  
              autotransform =TRUE, 
              noshare = "stepacross", wascores = TRUE, 
              expand = TRUE,  trace = 1, plot = FALSE, 
              weakties = TRUE)


plot(pc)

x <- pc$points[,1]
y <- pc$points[,2] 
tab <- pc$stress


# The end ------------------------------