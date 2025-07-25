#' @title picklist
#'
#' @description Generates a picklist from an experiment map and a source plate map, for use in the Beckmann Echo software suite
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



#Create picklist for desired variable(s)
picklist <- function(map, #plate map
                     src_plate,
                     xfer_vol, #Name of 'Transfer Volume' column in map,
                                        #or value to be assigned,
                                        # or vector of values to be assigned
                     variables, #String or vector of strings with the names of the variables to include
                     dest_plate = "Dest_Plate_Name", #Name of 'Destination Plate Name' column in map, or value to be assigned
                     src_plate_name = "Src_Plate_Name", #Name of 'Source Plate Name' column in src_plate, or value to be assigned
                     src_plate_type = "DMSO",#Name of 'Destination Plate type' column in src_plate, or value to be assigned
                     src_well = "Well" #Name of Source Well column in src_plate
                     ){
    dplyr::mutate(map, Dest_Well = Well) %>%
        dplyr::select(all_of(variables), Dest_Well) %>%
        dplyr::left_join(src_plate, by = variables) -> pl

    if (nrow(pl) > nrow(map)) { #Check if source samples have multiple wells assigned
        print("Some source variables have multiple wells; Corresponding Source Wells were evenly distributed")
        
        dplyr::mutate(map, Dest_Well = Well) %>%
            dplyr::select(all_of(variables), Dest_Well) %>%  
            group_by_at(variables) %>%
            mutate(row_idx = row_number()) %>%
            left_join(src_plate %>% 
                      group_by_at(variables) %>%
                      mutate(source_idx = row_number()), 
                      by = variables, relationship = "many-to-many") %>%
            mutate(match_idx = (row_idx - 1) %% n_distinct(source_idx) + 1) %>% #Distribute redundant source wells among the destination wells
            filter(source_idx == match_idx) %>%
            ungroup()%>%
            select(!c(row_idx, source_idx, match_idx)) -> pl
    }
    
    if( ! dest_plate %in% names(map)){ 
        pl$Dest_Plate_Name = dest_plate} #dest_plate can be the name to be assigned to the destination plates, 
    else {pl$Dest_Plate_Name = map[[dest_plate]]} #or the name of the column containing the destination plate name

    if( ! src_plate_name %in% names(src_plate)){ 
        pl$Src_Plate_Name = src_plate_name}
     else {pl$Src_Plate_Name = pl[[src_plate_name]]} 
     
    if( ! src_plate_type %in% names(src_plate)){ 
        pl$Src_Plate_Type = src_plate_type}
    else {pl$Src_Plate_Type = pl[[src_plate_type]]}
    dplyr::mutate(pl, Src_Plate_Type = ifelse(Src_Plate_Type == "DMSO", "384PP_DMSO2",
                                              ifelse(Src_Plate_Type == "Water", "384PP_AQ_GP3", Src_Plate_Type))) -> pl

    if(length(xfer_vol) == nrow(pl)){
        pl$Transfer_Volume = 2.5 * round(xfer_vol/2.5)} #Convert to multiples of 2.5 nL
    else if(length(xfer_vol) == 1){
            if( ! xfer_vol %in% names(map)){
                pl$Transfer_Volume = 2.5 * round(xfer_vol/2.5)} #Convert to multiples of 2.5 nL
            else {pl$Transfer_Volume = 2.5 * round(map[[xfer_vol]] / 2.5)}
        }
    else {stop("Number of volume values does not match number of samples")}
               
    pl$Src_Well = pl[[src_well]]
    
    pl = dplyr::filter(pl, Transfer_Volume > 2.4) #remove invalid volumes
    pl = dplyr::select(pl, Src_Well, Src_Plate_Name, Src_Plate_Type, Transfer_Volume, Dest_Well, Dest_Plate_Name)
    if(nrow(pl) < nrow(map)){
        warning("Volumes below 1.25 nL were removed")}
    return(pl)
}

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
