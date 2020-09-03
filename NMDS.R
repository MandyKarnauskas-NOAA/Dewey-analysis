#########################################################
#  M. Karnauskas - 30 June 2020                         #
#  Code for running the "Dewey analysis"                #
#  Processing of charter boat social media              #
#  NMDS and ANOSIM code                                 #
#########################################################

# Set directory ----------------------------------------
rm(list=ls())
setwd("C:/Users/mandy.karnauskas/Desktop/participatory_workshops/Dewey_analysis")

# Install libraries ------------------------------------
if ("vegan" %in% available.packages() == FALSE) { install.packages("vegan") } 
if ("viridis" %in% available.packages() == FALSE) { install.packages("viridis") } 

library(vegan)
library(MASS)
library(viridis)
library(yarrr)

# Load data --------------------------------------------
dfl <- read.table("Charter_SouthFloridaKeys_9_2_20_906Count.csv", 
                header = T, sep = ",", check.names = F, quote = "")

dnc <- read.table("Charter__NCVA_8_12_20.csv", 
                header = T, sep = ",", check.names = F, quote = "")

#dfl$MAHI <- rowSums(dfl[,grep("ahi", names(dfl))], na.rm = T)
#dfl <- dfl[,-grep("mahi", names(dfl))]
#d <- dfl

dnc <- dnc[,-grep("Mahi_Gaffers", names(dnc))]
dnc$MAHI <- rowSums(dnc[,grep("ahi", names(dnc))], na.rm = T)
dnc <- dnc[,-grep("ahi", names(dnc))]
d <- dnc

# ONLY for analysis of two regions together ------------

head(dfl)
head(dnc)
names(dnc)[which(names(dnc) %in% names(dfl))]
names(dnc)[-which(names(dnc) %in% names(dfl))]

# modify names so that they match 
names(dfl)[1] <- tolower(names(dfl)[1])
names(dnc)[grep("Unk", names(dnc))] <- "mahi_gaffers" 
names(dfl)[grep("Tilefish", names(dfl))] <- "Tilefish" 
names(dfl)[grep("Spanish", names(dfl))] <- "Spanish_Mackerel" 
names(dnc)[grep("Blackbelly_Rosefish", names(dnc))] <- "Blackbelly Rosefish" 
dnc$tunas <- dnc$Tuna_Complex
names(dfl)[grep("Amberjack", names(dfl))] <- "Amberjacks" 
names(dnc)[grep("Amberjack", names(dnc))] <- "Amberjacks" 

dfl$tunas <- dfl$`Blackfin Tuna` + dfl$`Skipjack Tuna`      # lump tunas
dfl$groupers <- rowSums(dfl[,grep("Grouper", names(dfl))], na.rm = T)
dnc$groupers <- dnc$`Grouper Complex` + dnc$Black_Sea_Bass  # lump groupers

table(dnc$mahi_gaffers + dnc$M_mahi_gaffers + dnc$F_mahi_gaffers == dnc$Mahi_Gaffers) # check that equivalent
dfl$Mahi_Gaffers <- dfl$M_mahi_gaffers + dfl$F_mahi_gaffers + dfl$mahi_gaffers

dfl$Ribbonfish <- NA                              # add two species not in FL 
dfl$Marlin <- NA

names(dnc)[which(names(dnc) %in% names(dfl))]     # check names
names(dnc)[-which(names(dnc) %in% names(dfl))]

dfl <- dfl[,-grep("mahi_gaffers", names(dfl))]    # remove lumped columns
dfl <- dfl[,-grep("Tuna", names(dfl))]
dfl <- dfl[,-grep("Grouper", names(dfl))]

dnc2 <- dnc[which(names(dnc) == names(dfl[1]))]   # sort columns in NC to match FL
for (i in 2:ncol(dfl))  { 
  if (ncol(dnc[which(names(dnc) == names(dfl[i]))]) == 1 ) {
  dnc2 <- cbind(dnc2, dnc[which(names(dnc) == names(dfl[i]))]) } else {
    dnc2 <- cbind(dnc2, rep(NA, nrow(dnc)))
  }   }

names(dfl)[-which(names(dnc2) == names(dfl))]
names(dnc2) <- names(dfl)
d <- rbind(dfl, dnc2)        # combine into single data frame


# assign other variables ------------------------------------
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

#d <- d[,-grep("notes", tolower(names(d)))]
names(d)

d[is.na(d)] <- 0           # convert NAs to zeros
dim(d)
which(rowSums(d[,9:(ncol(d))]) == 0)
#d <- d[-which(rowSums(d[,9:(ncol(d))]) == 0),]
dim(d)

d1 <- d[,9:(ncol(d))]     # separate object with spp counts only

seas <- cut(d$month, breaks = c(0, 3.5, 6.5, 9.5, 12.5))  # approximate seasons
labs <- c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")
seas <- cut(d$month, breaks = c(0, 5.5, 7.5, 9.5, 12.5))  # approximate seasons
labs <- c("Jan-May", "Jun-Jul", "Aug-Sep", "Oct-Dec")
d$mclass <- factor(labs[as.numeric(seas)], levels = labs)

d$reg2 <- NA                                              # manually define regions
d$reg2[grep("Hatteras", d$marina)] <- "Hatteras"
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
table(d$reg2, useNA = "always")
table(d$reg2, d$marina)
table(d$mclass, useNA = "always")
table(d$mclass, d$region)

d$mon <- d$month                       # group months with few samples
table(d$mon, useNA = "always")
d$mon[d$mon <= 4] <- 4
d$mon[d$mon >= 10] <- 10
d$mon2 <- month.abb[d$mon]
d$mon2[which(d$mon2 == "Apr")] <- "Jan-Apr"
d$mon2[which(d$mon2 == "Oct")] <- "Oct-Dec"
d$mon2 <- factor(d$mon2, levels = c("Jan-Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct-Dec"))
table(d$mon2, d$reg2, useNA = "always")

table(d$year)                         # group years with few samples
d$yr2 <- as.character(d$year)
d$yr2[which(d$year <= 2017)] <- "2013-17"
table(d$yr2, useNA = "always")

# operations to spp matrix prior to NMDS -----------------
dim(d1)
plot(colSums(d1))

sort(colSums(d1 != 0) / nrow(d1) * 100)   # look at occurrence rates
hist(colSums(d1 != 0) / nrow(d1) * 100, breaks = 40)
which(colSums(d1 != 0) / nrow(d1) * 100 < 2)
which(colSums(d1 != 0) / nrow(d1) * 100 > 2)

splis <- which(colSums(d1 != 0) / nrow(d1) * 100 > 2)
d1 <- d1[splis]
dim(d1) 
head(d1)

djit <- d1 + abs(rnorm(prod(dim(d1)), sd=0.01))  # jitter data because some exact like entries

#for (i in 1:ncol(d1))  { d1[,i] <- d1[,i] / max(d1[,i]) }  # scale by max abundance

# NMDS ----------------------------------
#pc1 <- metaMDS(djit, k = 3, autotransform = T, trymax = 25, noshare = 0)

d1.dist <- vegdist(djit, method = "bray")
pc <- isoMDS(d1.dist, k = 2, trace = T, tol = 1e-3)
#pc <- isoMDS(d1.dist, k = 3, trace = T, tol = 1e-3)

# ANOSIM ---------------------------
reg.ano <- anosim(d1.dist, d$region)
reg2.ano <- anosim(d1.dist, d$reg2)
mar.ano <- anosim(d1.dist, d$marina)
yr.ano <- anosim(d1.dist, d$yr2)
mon.ano <- anosim(d1.dist, d$mon2)
sea.ano <- anosim(d1.dist, d$mclass)

summary(reg.ano)
summary(reg2.ano)
summary(mar.ano)
summary(yr.ano)
summary(mon.ano)
summary(sea.ano)

Rvals <- c(reg.ano$statistic, reg2.ano$statistic, mar.ano$statistic, 
           yr.ano$statistic, mon.ano$statistic, sea.ano$statistic)

# plot ANOSIM results -------------------------

par(mfrow=c(3,2), mex = 1.0, mar = c(11,5,2,1))

plot(reg.ano, las = 2, xlab = "", ylab = "")
plot(reg2.ano,las = 2, xlab = "", ylab = "")
plot(mar.ano, las = 2, xlab = "", ylab = "")
plot(yr.ano,  las = 2, xlab = "", ylab = "")
plot(mon.ano, las = 2, xlab = "", ylab = "")
plot(sea.ano, las = 2, xlab = "", ylab = "")

# ordination plots -------------------

par(mfrow=c(3,2), mex = 1.0, mar = c(2,3,2,1))

cols <- transparent(alphabet(20), trans.val = 0.5)
factors <- c("region", "reg2", "marina", "yr2", "mon2", "mclass")
labs <- c("region", "subregion", "marina", "year", "month", "season")

for (i in 1:6)  {
  fact <- d[,which(names(d) == factors[i])]
  
  ta <- table(as.numeric(as.factor(fact)), fact); ta
  plot(pc$points[,1], pc$points[,2], col = cols[as.numeric(as.factor(fact))], 
       main = labs[i], pch = 16, cex = 0.6, xlim = c(-1, 1))  #  xlim = c(-0.8, 0.8))
  ordiellipse(pc, fact, col = cols, lwd = 2, kind = "sd", label = F)
  legend("topright", ncol = 1, colnames(ta), col = cols[as.numeric(rownames(ta))], pch = 16, cex = 1)
  text(x = -0.85, y = 0.95, paste("R =", round(Rvals[i], 2)), cex = 1.2)
}


# NMDS sensitivity analysis for distance method -----------

met <- c("manhattan", "euclidean", "canberra", "clark", "bray", "jaccard", "gower", 
         "altGower", "horn", "binomial", "kulczynski", "raup")  #"morisita", "mountford", "chao", "cao")

par(mfrow=c(4,3), mex=0.5, mar = c(2,3,3,1))

for (i in 1:length(met)) {
  d1.dist <- vegdist(djit, method = met[i])
  pc <- isoMDS(d1.dist, k = 2, trace = T, tol = 1e-3)

  plot(pc$points[,1], pc$points[,2], col = as.numeric(as.factor(d$reg2)), pch=16, cex=0.5)
  ordiellipse(pc, d$reg2, col = 1:4, lwd=2, kind = "sd", label = T)
  mtext(side = 3, paste(met[i], "    stress = ", round(pc$stress)), cex=0.8)
}

# ANOSIM sensitivity analysis for distance methods ----------------

met <- c("manhattan", "canberra", "clark", "bray", "jaccard", "gower", "horn",  
         "binomial", "kulczynski", "raup")   #"morisita", "mountford", "chao", "cao")

ano.res <- NA

for (i in 1:length(met)) {
  d1.dist <- vegdist(djit, method = met[i])
  
  #  yr.ano  <- anosim(d1.dist, d$year)
  sea.ano <- anosim(d1.dist, d$mclass)
  #  mon.ano <- anosim(d1.dist, d$mon2)
  mar.ano <- anosim(d1.dist, d$marina)
  reg.ano <- anosim(d1.dist, d$region)
  reg2.ano <- anosim(d1.dist, d$reg2)
  
  res <- c(sea.ano$statistic, mar.ano$statistic, reg.ano$statistic, reg2.ano$statistic)
  p <- c(sea.ano$signif, mar.ano$signif, reg.ano$signif, reg2.ano$signif)
  resp <- c(res, p)
  ano.res <- cbind(ano.res, resp)  
  cat(met[i])
}
ano.res

# The end ------------------------------