#' @title picklist
#'
#' @description Generates a picklist from an experiment map and a source plate map, for use in the Beckmann Echo software suite
#'
#' @param map Experiment map to follow
#' @param src_plate Plate map of the source plates to use for this dispense. Should be a single table.
#' @param xfer_vol One of the following:
#' Value of volume to dispense (in nL)
#' Vector of volumes to dispense, of length == nrow(map)
#' Name of a column in `map` corresponding to that vector
#' 
#' @param variables String or vector of strings with the name of the columns in `src_plate` to use for matching
#'
#' @param dest_plate Name to assign to the Destination Plate, or the name of the column in the `map` containing the Destination Plate Name
#'
#' @param src_plate_name Name of 'Source Plate Name' column in src_plate, or Name to be assigned
#'
#' @param src_plate_type Name of 'Destination Plate type' column in src_plate, or value to be assigned. 'DMSO' and 'Water' are interpreted readily for a 384PP plate.
#'
#' 
#' @param src_well Name of Source Well column in src_plate
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

#Create picklist for desired variable(s)
backfill <- function(pl, backfill_wells,
                     src_plate_name = "Src_Plate_Name", #Name of column containing plate name, if only 1 source plate is used, or name of Control plate if many
                     xfer_vol = "Transfer_Volume",
                     src_well = "Src_Well"){
    maxvol = max(pl[[xfer_vol]])
    pl2 = pl
    pl2[[xfer_vol]] = maxvol - pl2[[xfer_vol]]
    pl2 = pl2[pl2[[xfer_vol]] > 2.4,]
    pl2[[src_well]] = rep_len(backfill_wells, length.out=nrow(pl2))

    if (src_plate_name == "Src_Plate_Name"){ #Little loop to keep default source plate name
        if(length(unique(pl[[src_plate_name]])) == 1){
            pl2$Src_Plate_Name = unique(pl[[src_plate_name]])}
        else {stop("Multiple Source Plates given but none specified")}
    }
    else{ pl2$Src_Plate_Name = src_plate_name} # But to allow specifying a control source plate                        
    pl2 = rbind(pl, pl2)
    return(pl2)
}

chk_vol <- function(pl){
    group_by(pl, Src_Well ) %>%
        summarise(total = sum(Transfer_Volume))
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

       









##To change well notation formats
pad_well <- function(Well){   #Add padding 0 to make all well names the same length
    col = gsub("\\D", "", Well)
    row = gsub("\\d", "", Well)
    paste0(row, stringr::str_pad(col, 2, "left", "0"))
}

unpad_well <- function(Well) {    #Remove padding zeros
    col = as.numeric(gsub("\\D", "", Well))
    row = gsub("\\d", "", Well)
    paste0(row, col)
}
