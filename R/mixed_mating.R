

mixed_mating <- function(parents, inbreeding_index = .3) {

    t <- parents[[1]] |> combn(2)

    ppairs <- data.table(
        "parent1" = c(parents[[1]], t[1, ]),
        "parent2" = c(parents[[1]], t[2, ])
    )

    ppairs <- ppairs[order(parent1, parent2)]

    colnames(ppairs) <- c("parent1", "parent2")


    ppairs$score <- fifelse(ppairs$parent1 == ppairs$parent2, inbreeding_index, 1 - inbreeding_index)

    return(ppairs)

}
