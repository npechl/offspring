## Trying to simulate and define the probability of mating events and store them in an object, following specific parameters 

info<-read.csv("mock-parents.csv")
nind=nrow(info)
nloci=ncol(info)-1

# defining arrays with male and female parents (in this case, as individuals can act as hermaphrodites, a parent can be both sexes)

fparents <- info$tree_id
mparents <- info$tree_id

# create a cross-table with all possible mating combinations - initial values missing (NA)

mating_all <- matrix(NA, nind+1, nind+1)
mating_all[1,2:(nind+1)]<-fparents
mating_all[2:(nind+1),1]<-mparents
mating_all[1,1]<-"random"

## random mating (including selfing)
# if chosen, all values in the mating_all matrix must be 1, thus equal probabilities for all mating combinations

mating_all[2:(nind+1),2:(nind+1)] = 1
x<-as.data.frame(mating_all)

## random mating (excluding selfing)
# if chosen, selfing is not allowed (probability 0) and all other combinations are random (probability 1)

mating_all[1,1]<-"random_no_selfing"
mating_all[2:(nind+1),2:(nind+1)] = 1
x<-as.data.frame(mating_all)
for (i in 1:nind) {
    if (x[i+1,1] == x[1,i+1]){
        x[i+1,i+1]<-0
    } 
}

## complete selfing
# if chosen, only sef-mating is allowed, thus same partner has probability 1 and all other combinations 0

mating_all[1,1]<-"selfing"
mating_all[2:(nind+1),2:(nind+1)] = 0
x<-as.data.frame(mating_all)
for (i in 1:nind) {
    if (x[i+1,1] == x[1,i+1]){
        x[i+1,i+1]<-1
    } 
}

## mixed mating
# if chosen, user defines an inbreeding index s and an outcrossing index t (t+s=1). All non selfed mating events are random

s<-0.3
t<-1-s
mating_all[1,1]<-"mixed"
mating_all[2:(nind+1),2:(nind+1)] = t
x<-as.data.frame(mating_all)
for (i in 1:nind) {
    if (x[i+1,1] == x[1,i+1]){
        x[i+1,i+1]<-s
    } 
}

## structured data in populations
# individuals must be divided in groups, here called populations
# here we introduce the information of populations in a separate file, but can be in the same data file as genotypes
# we arbitarily divide mock.parents in 5 populations, here called mock1 to mock5 and include them in object pop

pop <- matrix(NA, nind)
pop[1:100] = "mock1"
pop[101:200] = "mock2"
pop[201:300] = "mock3"
pop[301:400] = "mock4"
pop[401:500] = "mock5"

# defining arrays with the populatoin information of male and female parents (in this case, as individuals can act as hermaphrodites, a parent can be both sexes)
popf <- pop
popm <- pop

## complete population structure - no gene flow 
# if chosen, only individuals from the same population may mate, thus same population has probability 1 and all other combinations 0

mating_all[1,1]<-"structured"
mating_all[2:(nind+1),2:(nind+1)] = 0
x<-as.data.frame(mating_all)
for (j in 1:nind) {
    for (i in 1:nind) {
        if (popf[i] == popm[j]){
            x[i+1,j+1]<-1
        } else if (popf[j] == popm[i]){
            x[i+1,j+1]<-1
        }
    }
}

## partial population structure with gene flow 
# if chosen, user defines a gene flow rate (m) among populations.

m <- 0.05
mating_all[1,1]<-"partial structure"
mating_all[2:(nind+1),2:(nind+1)] = m
x<-as.data.frame(mating_all)
for (j in 1:nind) 
{
    for (i in 1:nind) 
    {
        if (popf[i] == popm[j]){
            x[i+1,j+1]<-1-m
        } else if (popf[j] == popm[i]){
            x[i+1,j+1]<-1-m
        }
    }
}

## Isolation by distance - IBD
# Genetic data often have geographical coordinates for each individual sampled. We will use real data from hop plants in Greece

info<-read.csv("hop.csv")
nind=nrow(info)
nloci=ncol(info)-1

# Defining arrays with male and female parents (we consider individuals as hermaphrodites, but in the actual case of hop, this is completely false, as hop is dioecious and only female plants were harvested. 

fparents <- info$plant
mparents <- info$plant

# create a cross-table with all possible mating combinations - initial values missing (NA)

mating_all <- matrix(NA, nind+1, nind+1)
mating_all[1,2:(nind+1)]<-fparents
mating_all[2:(nind+1),1]<-mparents
mating_all[1,1]<-"IBD"

# Introduce a file with the geographic coordinates of hop plants. Column "longitude" must be first.

coord<-read.csv("hop_coord.csv")

# calculating distances (in meters) between plants using package "geosphere"

library(geosphere)
dist<-distm(coord, fun=distGeo)

## IBD - linear
# We model gene flow using the IBD model, where the longest the distance the smallest the probability of mating between two plants. Simplest model is the linear one. We find the largest distance and divide all distances with the largest one and finally we reverse the distance table (and make it a similarity one).

max<-max(dist)
dist1<-dist/max
sim<-1-dist1
mating_all[2:(nind+1),2:(nind+1)]<-sim
x<-as.data.frame(mating_all)

## IBD - linear without selfing
# If selfing is allowed, most mating events will be selfing, as distance will always be 0 and mating probability will be 1. So we may exclude selfing, especially in plants where this is not common.

max<-max(dist)
dist1<-dist/max
sim<-1-dist1
mating_all[2:(nind+1),2:(nind+1)]<-sim
mating_all[1,1]<-"IBD_no_selfing"
x<-as.data.frame(mating_all)

for (i in 1:nind) {
    if (x[i+1,1] == x[1,i+1]){
        x[i+1,i+1]<-0
    } 
}

## IBD - linear with limit
# Most pollen distribution models have a distance limit for pollen flow, around 30m. Selfing will be again excluded. All mating events with plants further apart than 30m, will have 0 mating probability.

limit<-30
dist1<-dist/limit
sim<-1-dist1
sim[sim <0]<-0
mating_all[2:(nind+1),2:(nind+1)]<-sim
mating_all[1,1]<-"IBD_Limit_no_selfing"
x<-as.data.frame(mating_all)

for (i in 1:nind) 
{
    if (x[i+1,1] == x[1,i+1]){
        x[i+1,i+1]<-0
    } 
}
























## Trying to create a function with the code that creates a progeny set with user defined size, from random combinations of a user defined set of female and male parents of a given dataset

library(dplyr)
library(tidyr)

info<-read.csv("mock-parents.csv")

## Start the function "offspring)

offspring <- function(info, fparents=nrow(info), mparents=nrow(info), prog=nrow(info)) {
    
    # Size of parents dataset
    nind=nrow(info)
    nloci=ncol(info)-1
    cat("The parents dataset contains", nind, "individuals and", nloci, "loci.", "\n")
    
    cat("Number of female parents:", fparents, "\n")
    cat("Number of male parents:", mparents, "\n")
    cat("Number of progeny produced:", prog, "\n")
    
    # define datasets with female and male parents for this mating season
    f1<-info %>% sample_n(fparents, replace = FALSE)
    m1<-info %>% sample_n(mparents, replace = FALSE)
    
    # Creating *prog* random mating events (pairs of trees). Each mating event should result in one offspring. Participating parents will be randomly chosen from the defined parent datasets respectively
    
    f<-f1 %>% sample_n(prog, replace = TRUE)
    m<-m1 %>% sample_n(prog, replace = TRUE)
    
    cat("Number of mating female parents:", length(unique(f[,1])), "\n")
    cat("Number of mating male parents:", length(unique(m[,1])), "\n")
    
    mating_events <- data.frame(female = f[,1], male = m[,1])
    # mating_events
    mating_combinations <- table(f$tree_id, m$tree_id)
    # mating_combinations
    
    # For each mating event, one multilocus gamete must be produced from each parent. With nloci loci, there are 2^nloci possible combinations for each gamete to occure. From each locus, one of the two gametes will be randomly selected. 
    
    # Female gametes: First we prepare an empty matrix
    fgametes <- matrix(NA, prog, nloci)
    
    # Start a loop for all female gametes
    for(j in 1:prog)
    {
        # Start a loop to define the multi-locus female gamete of one individual
        for(i in 1:nloci)
        { 
            geno <- f[j,i+1]
            alleles <- unlist(strsplit(geno, split = "/"))
            gamete <- sample(alleles,1)
            gamete
            fgametes[j,i] <- gamete
        }
    }
    
    # Male gametes: First we prepare an empty matrix
    mgametes <- matrix(NA, prog, nloci)
    
    # Start a loop for all female gametes
    for(j in 1:prog)
    {
        # Start a loop to define the multi-locus female gamete of one individual
        for(i in 1:nloci)
        { 
            geno <- m[j,i+1]
            alleles <- unlist(strsplit(geno, split = "/"))
            gamete <- sample(alleles,1)
            gamete
            mgametes[j,i] <- gamete
        }
    }
    
    # Merge two datasets into one progeny file
    progeny <<- matrix(NA, prog, nloci+1)
    colnames(progeny) <<- colnames(info)
    
    # Write tree names
    for (i in 1:prog) 
    {
        progeny[i,1] <<- paste("tree", i, sep = "_")
    }
    
    # Create the progeny genotype file
    progeny[,2:(nloci+1)] <<- paste(fgametes[,1:nloci], mgametes[,1:nloci], sep = "/")
    
}

# export the progeny dataset as .csv
write.csv(progeny,file="mock-progeny3.csv", row.names = TRUE)

## Calculate some basic genetic parameters for datasets
library(adegenet)

# Size of parents dataset
nind=nrow(info)
nloci=ncol(info)-1
prog=nrow(progeny)

obj_par<-df2genind(info[,c(2:(nloci+1))], sep="/")
obj_par

obj_pro<-df2genind(progeny[,c(2:(nloci+1))], sep="/")
obj_pro

sum_par<-summary(obj_par)
sum_pro<-summary(obj_pro)

# Expected and observed hererozygosity and allelic richness

Ho_par <- mean(sum_par$Hobs)
He_par <- mean(sum_par$Hexp)
F_par <- (He_par-Ho_par)/He_par
Na_par <- mean(obj_par$loc.n.all)

Ho_pro <- mean(sum_pro$Hobs)
He_pro <- mean(sum_pro$Hexp)
F_pro <- (He_pro-Ho_pro)/He_pro
Na_pro <- mean(obj_pro$loc.n.all)

h_table = matrix(c(nind, Na_par, Ho_par, He_par, F_par, prog, Na_pro, Ho_pro, He_pro, F_pro), ncol=5, byrow=TRUE)
colnames(h_table) = c("N", "Na/L", "Ho", "He", "F")
rownames(h_table) <- c("parents", "progeny")
h_table

# Mating system parameters

cat("Number of female parents:", length(unique(f[,1])), "\n")
cat("Number of male parents:", length(unique(m[,1])), "\n")

mating_events$self <-  ifelse(mating_events$female == mating_events$male, 1, 0)
cat("Number of selfing events", sum(mating_events$self), "\n")

## Some plots describing parents and progeny

# Expected and observed heterozygosities
par(mfrow=c(2,2))
boxplot(sum_par$Hexp, main="Expected heterozygosity", ylab="Hexp", xlab="parents", ylim=c(0,1))
boxplot(sum_par$Hobs, main="Observed heterozygosity", ylab="Hobs", xlab="parents", ylim=c(0,1))
boxplot(sum_pro$Hexp, main="Expected heterozygosity", ylab="Hexp", xlab="progeny", ylim=c(0,1))
boxplot(sum_pro$Hobs, main="Observed heterozygosity", ylab="Hobs", xlab="progeny", ylim=c(0,1))

# Inbreeding coefficients per locus
par(mfrow=c(2,1))
Fl_par <- (sum_par$Hexp-sum_par$Hobs)/sum_par$Hexp
barplot(Fl_par, ylab="F",
        main="Inbreeding coefficient per locus - parents")

Fl_pro <- (sum_pro$Hexp-sum_pro$Hobs)/sum_pro$Hexp
barplot(Fl_pro, ylab="F",
        main="Inbreeding coefficient per locus - progeny")

# Number of alleles per locus
par(mfrow=c(2,1))
barplot(sum_par$loc.n.all, ylab="Number of alleles",
        main="Number of alleles per locus - parents")

barplot(sum_pro$loc.n.all, ylab="Number of alleles",
        main="Number of alleles per locus - progeny")

## PCA

pca1 <- dudi.pca(obj_par, scale = FALSE, scannf = FALSE, nf = 3)
pca1

pca2 <- dudi.pca(obj_pro, scale = FALSE, scannf = FALSE, nf = 3)
pca2

# eigenvalues
par(mfrow=c(1,2))
barplot(pca1$eig[1:100], main = "PCA eigenvalues - parents", col = heat.colors(100))
barplot(pca2$eig[1:100], main = "PCA eigenvalues - progeny", col = heat.colors(100))

# pca individual plots
par(mfrow=c(1,2))
colorplot(pca1$li, pca1$li, transp=TRUE, cex=3, xlab="PC 1", ylab="PC 2")
title("Genetic diversity PCA of the mock parents population, axes 1-2")
abline(v=0,h=0,col="grey", lty=2)

colorplot(pca2$li, pca2$li, transp=TRUE, cex=3, xlab="PC 1", ylab="PC 2")
title("Genetic diversity PCA of the mock progeny population, axes 1-2")
abline(v=0,h=0,col="grey", lty=2)

# Using poppr - gac

# A genotype accumulation curve is a tool that allows you to assess how much power you have to discriminate between unique individuals given a random sample of n loci. This analysis is particularly important for clonal organisms to confirm that a plateau has been reached in the number of loci necessary to discriminate individuals.

library(poppr)

gac1 <- genotype_curve(obj_par, sample = 1000, quiet = TRUE)
title("Genotype accumulation curve - parents")

gac2 <- genotype_curve(obj_pro, sample = 1000, quiet = TRUE)
title("Genotype accumulation curve - progeny")

# A locus table

table1 <- locus_table(obj_par)
table1
table2 <- locus_table(obj_pro)
table2

# diversity indexes

diversity1<-poppr(obj_par)
diversity1
diversity2<-poppr(obj_pro)
diversity2

# using pegas - HW test

library(pegas)

hwe1 <- hw.test(obj_par, B = 1000)
hwe1
hwe2 <- hw.test(obj_pro, B = 1000)
hwe2

# using lattice - HW test
library("lattice")

alpha  <- 0.05
hwe21 <- hwe1
hwe21[hwe1 > alpha] <- 1
hwe22 <- hwe1
hwe22[hwe2 > alpha] <- 1


levelplot(t(hwe21))
levelplot(t(hwe22))

library("vegan")
par(mfrow=c(2,1))
tab1 <- mlg.table(obj_par, plot = FALSE)
min_sample1 <- min(rowSums(tab1))
rarecurve(tab1, sample = min_sample1, xlab = "Sample Size", ylab = "Expected MLGs")

tab2 <- mlg.table(obj_pro, plot = FALSE)
min_sample2 <- min(rowSums(tab2))
rarecurve(tab2, sample = min_sample2, xlab = "Sample Size", ylab = "Expected MLGs")


N1      <- diversity1$N      # number of samples
lambda1 <- diversity1$lambda # Simpson's index
CSim1<-(N1/(N1 - 1)) * lambda1              # Corrected Simpson's index

N2      <- diversity2$N      # number of samples
lambda2 <- diversity2$lambda # Simpson's index
CSim2<-(N2/(N2 - 1)) * lambda2              # Corrected Simpson's index

mlg.table(obj_par)
mlg.table(obj_pro)
