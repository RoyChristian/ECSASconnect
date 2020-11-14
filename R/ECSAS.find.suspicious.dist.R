#'@export
#'@title Find ECSAS watches with suspicious distances
#'
#'@description Find watches where the distance traveled as calculated by lat/long vs dead reckoning seems suspicious.
#'  This function looks for watches whose length in km as calculated by dead-reckoning (speed * time) 
#'  is \emph{significantly} different than that calculated by using [geosphere::distGeo()] from start and end
#'  positions. A watch is flagged as significantly different if the difference between the two distance 
#'  measures is \code{>= thresh} percent of the smaller of the two distances. 
#'
#'@param dat a dataframe, such as returned by \code{\link{ECSAS.extract()}}, containing, at a minimum, fields
#'  named \code{LatStart, LatEnd, LongStart, LongEnd, PlatformSpeed, CalcDurMin.}
#'  
#'@param thresh (as percentage; default 50) maximum percentage of the smaller of the two distances that the 
#'   difference is allowed to be before being flagged as problematic. 
#'@return Returns \code{dat} with the following addtional columns added:
#'
#'\tabular{ll}{
#'   \code{dist_geo_km} \tab distance as calculated by [geosphere::distGeo()] \cr
#'   \code{dist_dr_km} \tab distance as calculated by dead reckoning: \eqn{PlatformSpeed * (CalcDurMin / 60) * 1.852}.
#'      Note the conversion from nautical miles to km.\cr
#'   \code{dist_diff_km} \tab difference in the two distances \cr
#'   \code{dist_pct_diff} \tab percentage difference (of the smaller distance) \cr
#' }
#' 
#' Users should look closely at watches with large \code{pct_diff} AND/OR large \code{dist_diff_km}.
#' 
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract()}}
#'
ECSAS.find.suspicious.dist <- function(dat = NULL, thresh = 50, debug = FALSE){
  
  if(debug) browser()
  
  # do arg checking
  
  dat %>% 
    dplyr::mutate(dist_dr_km = PlatformSpeed * (CalcDurMin / 60) * 1.852,
           dist_geo_km = geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000,
           dist_diff_km = abs(dist_dr_km - dist_geo_km),
           pct_diff = dplyr::case_when(dist_dr_km <= dist_geo_km ~ (dist_diff_km/dist_dr_km) * 100,
                                       TRUE ~ (dist_diff_km/dist_geo_km) * 100)
    ) %>% 
    dplyr::filter(pct_diff >= thresh)
}