#' @title chk_vol
#'
#' @description Summarize volume transferred from each Source Well in a picklist
#'
#' @param pl Picklist table. The names of the columns are assumed to be "Src_Well" and "Transfer_Volume"
#' 
#' @return A tibble with the total volume transferred from each source well
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' testmath(4)
#'
chk_vol <- function(pl){
    dplyr::group_by(pl, Src_Well ) %>%
        dplyr::summarise(total = sum(Transfer_Volume))
}

#Visualize selected area
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
            mutate(Selected = TRUE) %>%
            right_join(tidyr::crossing(cols = c(1:x), rows = c(1:y))) %>%
            mutate(Selected = tidyr::replace_na(Selected, FALSE)) %>%
            mutate(rows = LETTERS[rows])

        ggplot(plate, aes(x = cols, y = rows, fill = Selected)) + geom_raster() + scale_y_discrete(limits = rev)
}

       










