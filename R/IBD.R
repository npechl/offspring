

IBD <- function(parents, coord, coord_cols = c("longitude", "latitude"), selfing = FALSE, limit = -1) {


    # t <- parents[[1]] |> combn(2)
    #
    # ppairs <- data.table(
    #     "parent1" = c(parents[[1]], t[1, ]),
    #     "parent2" = c(parents[[1]], t[2, ])
    # )
    #
    # ppairs <- ppairs[order(parent1, parent2)] |> split(by = "parent1")

    dist <- coord[, coord_cols, with = FALSE] |>
        as.matrix(rownames = coord[[1]]) |>
        distm(fun = distGeo)

    colnames(dist) <- parents[[1]]
    row.names(dist) <- parents[[1]]

    max_d <- ifelse(limit > 0, limit, max(dist))

    mating <- 1 - (dist / max_d)

    ppairs <- mating |>
        as.data.frame() |>
        setDT(keep.rownames = "parent1") |>
        melt(id.vars = "parent1",
             variable.factor = FALSE, value.factor = FALSE,
             variable.name = "parent2", value.name = "score")

    ppairs$score <- fifelse(ppairs$score < 0, 0, ppairs$score)

    if(!selfing) {

        ppairs$score <- fifelse(ppairs$parent1 == ppairs$parent2, 0, ppairs$score)

    }

    # ----------------------------------------------------------------------------------------
    ## IBD - linear with limit
    # Most pollen distribution models have a distance limit for pollen flow, around 30m. Selfing will be again excluded. All mating events with plants further apart than 30m, will have 0 mating probability.

    # limit<-30
    # dist1<-dist/limit
    # sim<-1-dist1
    # sim[sim <0]<-0
    # mating_all[2:(nind+1),2:(nind+1)]<-sim
    # mating_all[1,1]<-"IBD_Limit_no_selfing"
    # x<-as.data.frame(mating_all)
    #
    # for (i in 1:nind)
    # {
    #     if (x[i+1,1] == x[1,i+1]){
    #         x[i+1,i+1]<-0
    #     }
    # }


}
