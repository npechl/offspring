


population <- function(parents, structure, complete = TRUE, gene_flow_rate = .05) {


    t <- parents[[1]] |> combn(2)

    ppairs <- data.table(
        "parent1" = c(parents[[1]], t[1, ]),
        "parent2" = c(parents[[1]], t[2, ])
    )

    ppairs <- ppairs[order(parent1, parent2)]

    colnames(ppairs) <- c("parent1", "parent2")

    if(complete) {
        ppairs$score <- fifelse(structure[ppairs$parent1] == structure[ppairs$parent2], 1, 0)
    } else {
        ppairs$score <- fifelse(structure[ppairs$parent1] == structure[ppairs$parent2], 1 - gene_flow_rate, gene_flow_rate)
    }

    return(ppairs)
}
