
#' Title
#'
#' @param parents data.table
#' @param mating character
#' @param selfing logical
#'
#' @return data.table
#' @export
#'
#' @importFrom utils combn
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
random_mating <- function(parents, mating = "random", selfing = TRUE) {

    t <- parents[[1]] |> combn(2)

    ppairs <- data.table(
        "parent1" = c(parents[[1]], t[1, ]),
        "parent2" = c(parents[[1]], t[2, ])
    )

    ppairs <- ppairs[order(parent1, parent2)]

    colnames(ppairs) <- c("parent1", "parent2")


    if(mating == "random") {

        if(selfing) {
            ppairs$score <- 1
        } else {
            ppairs$score <- fifelse(ppairs$parent1 == ppairs$parent2, 0, 1)
        }

    } else if(mating == "complete") {
        ppairs$score <- fifelse(ppairs$parent1 == ppairs$parent2, 1, 0)
    } else {
        stop("Provide a suitable input for mating event: either `random` or `complete` is accepted")
    }


    return(ppairs)
}
