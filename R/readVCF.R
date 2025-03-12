



#' Title
#'
#' @param path Path to VCF file.
#'
#' @return data.table
#' @export
#'
#' @importFrom vcfR read.vcfR
#' @importFrom vcfR vcfR2tidy
#' @importFrom data.table dcast
readVCF <- function(path) {

    varo <- path |> read.vcfR(verbose = FALSE)

    varo@fix[, "ID"] <- paste0(
        varo@fix[, "CHROM"], ":",
        varo@fix[, "POS"], "_",
        varo@fix[, "REF"], "_",
        varo@fix[, "ALT"]
    )

    # fix <- varo |> getFIX()
    #
    # fix[, ]

    varo <- varo |> vcfR2tidy(single_frame = TRUE, verbose = FALSE)
    varo <- varo$dat |> setDT()

    trees <- varo[, c("Indiv", "ID", "gt_GT_alleles"), with = FALSE] |>
        dcast(Indiv ~ ID, value.var = "gt_GT_alleles")

    return(trees)
}
