

IBD <- function(parents, metadata, coord_cols = c("longitude", "latitude")) {


    t <- parents[[1]] |> combn(2)

    ppairs <- data.table(
        "parent1" = c(parents[[1]], t[1, ]),
        "parent2" = c(parents[[1]], t[2, ])
    )

    ppairs <- ppairs[order(parent1, parent2)]

    colnames(ppairs) <- c("parent1", "parent2")

    dist <- distm(t, fun = distGeo)





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


}
