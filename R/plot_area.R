#' @title plot_area
#'
#' @description GGplot2 visualizer of the plate area selected for the experiment.
#'
#' @param cols Numeric vector of plate columns selected for the experiment.
#'
#' @param rows Numeric vector of plate rows selected for the experiment.
#'
#' @param plate_format Plate format used. Defaults to 384.
#' @return A plot of the layout of the plate
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' testmath(4)
#'

plot_area <- function(cols, rows, plate_format = 384){
        if (plate_format == 6) {
        x = 3
        y = 2} else if (plate_format == 24) {
        x = 6
        y = 4} else if (plate_format == 96) {
        x = 12
        y = 8} else if (plate_format == 384) {
        x = 24
        y = 16} else if (plate_format == 1536){
        x = 48
        y = 32} else {print("Unknown plate format")}

        plate = tidyr::crossing(cols, rows) %>%
            dplyr::mutate(Selected = TRUE) %>%
            dplyr::right_join(tidyr::crossing(cols = c(1:x), rows = c(1:y))) %>%
            dplyr::mutate(Selected = tidyr::replace_na(Selected, FALSE)) %>%
            dplyr::mutate(rows = LETTERS[rows])

        ggplot2::ggplot(plate, ggplot2::aes(x = cols, y = rows, fill = Selected)) + ggplot2::geom_raster() + ggplot2::scale_y_discrete(limits = rev)
}

       



