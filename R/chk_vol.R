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
chk_vol <- function(pl, plate_col = "SrcPlateName", sample_col = "SampleName"){
    group_by_at(pl, c("SrcWell", plate_col, sample_col )) %>%
        summarise(total = sum(XferVol))   
}


       










