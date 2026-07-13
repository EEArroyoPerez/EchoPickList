#' @title chk_vol
#'
#' @description Summarize volume transferred from each Source Well in a picklist
#'
#' @param pl Picklist table. The names of the columns are assumed to be "Src_Well" and "Transfer_Volume"
#'
#' @param plate_col Name of column containing source plate names. Default is "SrcPlateName"
#'
#' @param sample_col Name of column containing sample name. Default is "SampleName"
#' 
#' @return A tibble with the total volume transferred from each source well
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
chk_vol <- function(pl, plate_col = "SrcPlateName", sample_col = "SampleName"){
    dplyr::group_by_at(pl, c("SrcWell", plate_col, sample_col )) %>%
        dplyr::summarise(total = sum(XferVol))   
}


       










