#' @title picklist
#'
#' @description Generates a picklist from an experiment map and a source plate map, for use in the Beckmann Echo software suite
#'
#' @param map Experiment map to follow
#' @param src_plate Plate map of the source plate(s) to use for this dispense. Should be a single table.
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

picklist <- function(map, #plate map
                     src_plate,
                     xfer_vol, #Name of 'Transfer Volume' column in map,
                                        #or value to be assigned,
                                        # or vector of values to be assigned
                     variables, #String or vector of strings with the names of the variables to include
                     dest_plate = "Dest_Plate_Name", #Name of 'Destination Plate Name' column in map, or value to be assigned
                     src_plate_name = "Src_Plate_Name", #
                     src_plate_type = "DMSO",#
                     src_well = "Well" #
                     ){
    dplyr::mutate(map, Dest_Well = Well) %>%
        dplyr::select(all_of(variables), Dest_Well) %>%
        dplyr::left_join(src_plate, by = variables) -> pl

    if (nrow(pl) > nrow(map)) { #Check if source samples have multiple wells assigned
        print("Some Source variables have multiple wells; Corresponding Source Wells were evenly distributed")
        
        dplyr::mutate(map, Dest_Well = Well) %>%
            dplyr::select(dplyr::all_of(variables), Dest_Well) %>%  
            dplyr::group_by_at(variables) %>%
            dplyr::mutate(row_idx = row_number()) %>%
            dplyr::left_join(src_plate %>% 
                      dplyr::group_by_at(variables) %>%
                      dplyr::mutate(source_idx = row_number()), 
                      by = variables, relationship = "many-to-many") %>%
            dplyr::mutate(match_idx = (row_idx - 1) %% dplyr::n_distinct(source_idx) + 1) %>% #Distribute redundant source wells among the destination wells
            dplyr::filter(source_idx == match_idx) %>%
            dplyr::ungroup()%>%
            dplyr::select(!c(row_idx, source_idx, match_idx)) -> pl
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

