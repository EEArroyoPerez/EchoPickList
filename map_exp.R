#' @title map_exp
#'
#' @description Generates a plate map for an experiment with all combinations of the defined variables, randomly assigned to the plate positions indicated in a defined area.
#'
#' @param cols Vector of plate column numbers to consider for the experiment.
#' @param rows Vector of plate row numbers to consider for the experiment.
#' @param ... Variables to combine for the experiment.
#' @param seed Random seed for designation of wells to each condition.
#' 
#'
#' @return A tibble containing all possible combinations of the Variables in the input, and a random selection of wells assigned from the area designated by cols + rows
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' testmath(4)

map_exp <- function(cols, rows, ..., seed){
    map = tidyr::crossing(...)
    wells = tidyr::crossing(cols, rows)
    wells = paste0(LETTERS[wells$rows], wells$cols)
    if (nrow(map) > length(wells)){
        stop("Area too small for all combinations of variables")}
    else {          
    set.seed(seed)
    map$Well = sample(wells, nrow(map))
    return(map)
    }
}


