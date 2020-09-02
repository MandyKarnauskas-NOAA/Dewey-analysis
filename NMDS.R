#########################################################
#  M. Karnauskas - 30 June 2020                         #
#  Code for running the "Dewey analysis"                #
#  Processing of charter boat social media              #
#  NMDS and ANOSIM code                                 #
#########################################################

# Set directory 
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/Dewey_analysis")

# Install libraries ------------------------------------
if ("vegan" %in% available.packages() == FALSE) { install.packages("vegan") } 
if ("viridis" %in% available.packages() == FALSE) { install.packages("viridis") } 

library(vegan)
library(MASS)
library(viridis)

# Load data --------------------------------------------
dfl <- read.table("Charter_SouthFloridaKeys_9_2_20_906Count.csv", 
                header = T, sep = ",", check.names = F, quote = "")
dnc <- read.table("Charter__NCVA_8_12_20.csv", 
                header = T, sep = ",", check.names = F, quote = "")

head(dfl)
head(dnc)

names(dnc)[which(names(dnc) %in% names(dfl))]
names(dnc)[-which(names(dnc) %in% names(dfl))]

names(dfl)[1] <- tolower(names(dfl)[1])
names(dnc)[grep("Unk", names(dnc))] <- "mahi_gaffers" 
names(dfl)[grep("Tilefish", names(dfl))] <- "Tilefish" 
names(dfl)[grep("Spanish", names(dfl))] <- "Spanish_Mackerel" 
names(dnc)[grep("Blackbelly_Rosefish", names(dnc))] <- "Blackbelly Rosefish" 
dnc$tunas <- dnc$Tuna_Complex
dfl$tunas <- dfl$`Blackfin Tuna` + dfl$`Skipjack Tuna`
names(dfl)[grep("Amberjack", names(dfl))] <- "Amberjacks" 
names(dnc)[grep("Amberjack", names(dnc))] <- "Amberjacks" 
dfl$groupers <- rowSums(dfl[,grep("Grouper", names(dfl))], na.rm = T)
dnc$groupers <- dnc$`Grouper Complex` + dnc$Black_Sea_Bass

table(dnc$mahi_gaffers + dnc$M_mahi_gaffers + dnc$F_mahi_gaffers == dnc$Mahi_Gaffers)
dfl$Mahi_Gaffers <- dfl$M_mahi_gaffers + dfl$F_mahi_gaffers + dfl$mahi_gaffers
dfl$Ribbonfish <- NA
dfl$Marlin <- NA

names(dnc)[which(names(dnc) %in% names(dfl))]
names(dnc)[-which(names(dnc) %in% names(dfl))]

dfl <- dfl[,-grep("mahi_gaffers", names(dfl))]
dfl <- dfl[,-grep("Tuna", names(dfl))]
dfl <- dfl[,-grep("Grouper", names(dfl))]


dnc2 <- dnc[which(names(dnc) == names(dfl[1]))]
for (i in 2:ncol(dfl))  { 
  if (ncol(dnc[which(names(dnc) == names(dfl[i]))]) == 1 ) {
  dnc2 <- cbind(dnc2, dnc[which(names(dnc) == names(dfl[i]))]) } else {
    dnc2 <- cbind(dnc2, rep(NA, nrow(dnc)))
  }   }

names(dfl)[-which(names(dnc2) == names(dfl))]
names(dnc2) <- names(dfl)
d <- rbind(dfl, dnc2)

names(d)
table(d$region, useNA = "always")
table(d$marina, useNA = "always")
table(d$company, useNA = "always")
table(d$photo_id, useNA = "always")
table(d$year, useNA = "always")
table(d$month, useNA = "always")
table(d$day, useNA = "always")
table(d$photo_type, useNA = "always")
dim(d)

d <- d[,-grep("notes", tolower(names(d)))]

names(d)

# NMDS ----------------------------------
d[is.na(d)] <- 0

dim(d)
d <- d[-which(rowSums(d[,9:(ncol(d))]) == 0),]
dim(d)

d1 <- d[,9:(ncol(d))]

d$mclass <- cut(d$month, breaks = c(0, 5.5, 7.5, 9.5, 12.5))

d$reg2 <- NA
d$reg2[which(d$marina == "Hatteras Harbor Marina")] <- "Hatteras"
d$reg2[which(d$marina == "Oregon Inlet Fishing Center")] <- "Wanchese"
d$reg2[which(d$marina == "Virginia Beach Fishing Center")] <- "VA Beach"
d$reg2[which(d$marina == "Pirate's Cove Marina")] <- "Wanchese"
d$reg2[which(d$marina == "Sensational Sportfishing")] <- "Morehead"
r1 <- c("Miami", "Hollywood", "Ft. Lauderdale", "Pompano Beach", "Deerfield Beach")
r2 <- c("Boynton Beach", "West Palm Beach", "Riviera Beach", "Jupiter")
r3 <- c("Islamorada", "Key Largo", "Key West", "Marathon")
d$reg2[which(d$marina %in% r1)] <- "Miami - Deerfield"
d$reg2[which(d$marina %in% r2)] <- "Boynton - Jupiter"
d$reg2[which(d$marina %in% r3)] <- "FL Keys"

table(d$reg2)
table(d$reg2, d$marina)
table(d$mclass)
table(d$mclass, d$region)

d$mon2 <- d$month
table(d$mon2, useNA = "always")
d$mon2[d$mon2 <= 4] <- 4
d$mon2[d$mon2 >= 10] <- 10

table(d$mon2, d$region)

d1 <- d1 + abs(rnorm(prod(dim(d1)), sd=0.01))  # jitter data because some exact like entries

#for (i in 1:ncol(d1))  {                      # scale by max abundance
#    d1[,i] <- d1[,i] / max(d1[,i])
#  }

dim(d1)
plot(colSums(d1))

#pc <- metaMDS(d1d, k = 2, autotransform = F, trymax = 50)

met <- c("manhattan", "euclidean", "canberra", "clark", "bray", 
         "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", 
         "binomial", "chao", "cao", "kulczynski")

#cols <- yarrr::transparent(cols25(10), trans.val = 0.5)

par(mfrow=c(4,4), mex=0.5, mar = c(2,3,3,1))

for (i in 1:length(met)) {
  
d1.dist <- vegdist(d1, method = met[i])
pc <- isoMDS(d1.dist, k=2, trace=T, tol = 1e-3)

x <- pc$points[,1]
y <- pc$points[,2]

plot(x, y, col = as.numeric(as.factor(d$region)), pch=16, cex=0.5)
ordiellipse(pc, d$region, col = 1:4, lwd=2, kind = "sd", label = F)
mtext(side = 3, paste(met[i], "    stress = ", round(pc$stress)), cex=0.8)
}

# unscaled
# scaled

d1.dist <- vegdist(d1, method = met[i])
pc <- isoMDS(d1.dist, k=2, trace=T, tol = 1e-3)
x <- pc$points[,1]
y <- pc$points[,2]

par(mfrow=c(3,2))
plot(x, y, col = cols[as.numeric(d$mclass)], pch=16)
plot(x, y, col = cols[as.numeric(d$marina)], pch=16)
plot(x, y, col = cols[(d$year - min(d$year)+1)], pch=16)
plot(x, y, col = cols[as.numeric(d$region)], pch=16)
plot(x, y, col = cols[as.numeric(as.factor(d$reg2))], pch=16)
plot(x, y, col = cols[as.numeric(d$mon2)-3], pch=16)

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
ordiellipse(pc, d2$marina, col = cols, lwd=2, kind = "sd", label = T)
tab <- table(as.numeric(d2$marina)-1, d2$marina)
legend("topleft", ncol = 2, colnames(tab)[2:6], col = cols, pch = 16, cex=0.8)

cols <- c(1, 2)
plot(x, y, col = cols[as.numeric(d2$region)-1], pch=16)
ordiellipse(pc, d2$region, col = cols, lwd=2, kind = "sd", label = T)
legend("topleft", ncol = 2, c("NC", "VA"), col = cols, pch = 16)

cols <- 5:8
table(as.numeric(as.factor(d2$reg2)), d2$reg2)
plot(x, y, col = cols[as.numeric(as.factor(d2$reg2))], pch=16)
ordiellipse(pc, d2$reg2, col = cols, lwd=2, kind = "sd", label = T)
legend("topleft", ncol = 2, c("Hatteras", "Morehead", "VA Beach", "Wanchese"), col = cols, pch = 16)

cols <- c(3, 4)
plot(x, y, col = cols[d2$year - 2012], pch=16)
ordiellipse(pc, d2$year, col = cols, lwd=2, kind = "sd", label = T)
legend("topleft", ncol = 2, c("2019", "2020"), col = cols, pch = 16)

cols <- viridis(12)
plot(x, y, col = cols[(as.numeric(d2$mon2))], pch=16)
ordiellipse(pc, d2$mon2, col = cols, lwd=2, kind = "sd", label = T, cex=1)
legend("topleft", ncol = 4, c(month.abb[4:11]), col = cols, pch = 16)

cols <- viridis(4)
plot(x, y, col = cols[as.numeric(mclass)], pch=16)
ordiellipse(pc, mclass, col = cols, lwd=2, kind = "sd", label = T)
legend("topleft", ncol = 4, c("Jan-May", "Jun-Jul", "Aug-Sep", "Oct-Nov"), col = cols, pch = 16)



# The end ------------------------------