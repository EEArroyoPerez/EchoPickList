#' @title add_ctrl
#'
#' @description Add wells to an experiment map over a subset of the variables in the original. Useful for adding controls, or additional independent experiments.
#'
#' @param map Experiment map to add control wells to.
#' 
#' @param cols Vector of plate column numbers to consider for the additional wells.
#' 
#' @param rows Vector of plate row numbers to consider for the additional wells.
#' 
#' @param ... Subset of variable. The name must match a variable present in the map.
#'
#' @param seed Random seed for designating remaining wells in the plate. Defaults to 3.
#' 
#' @return The input map tibble, with the additional wells appended at the bottom.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' testmath(4)

add_ctrl <- function(map, cols, rows, ..., seed = 3) { 
    tidyr::crossing(rows =LETTERS[rows], cols) %>% #Generate list of wells available in the plate
        dplyr::mutate(Well = paste0(rows, cols)) %>% dplyr::select(Well) %>%
        dplyr::anti_join(map, by = dplyr::join_by(Well)) %>% dplyr::pull(Well) -> wells
    tidyr::crossing(...) -> ctrls # Generate combinations of selected variables
    dplyr::select(map, ! dplyr::all_of(c(names(ctrls), "Well"))) %>%
        unique() %>% #Generate table with unique values of unselected variables
        tidyr::crossing(ctrls) %>%
        dplyr::mutate(Well = sample(wells, nrow(.))) %>%
        dplyr::bind_rows(map) -> map2
    return(map2)
} #Need to add a warning about table size, and maybe support for other plate formats
