#' @title round_vol
#'
#' @description Rounds values to multiples of the droplet size. Default droplet is 2.5 nL
#'
#' @param vol Vector of volumes to convert
#' @param droplet Size of the droplet used. Default is 2.5 nL
#' @return A vector of values rounded to multiples of the droplet size
#'
#' @export
#'
#' @examples
#'
#' round_vol(c(5, 6.5, 8, 10))
#'

round_vol <- function(vol,
                      droplet = 2.5){
    vol2 = 2.5 * round(vol/2.5)
    if(sum(vol2 == 0) > 0){
        warning("Some volumes were rounded down to 0")}
    return(vol2)
}
