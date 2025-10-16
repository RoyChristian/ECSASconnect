#'@export
#'@title Find ECSAS watches with suspicious positions
#'
#'@description Find watches where the difference in distance traveled as calculated by lat/long 
#'   vs dead reckoning seems suspicious, or where unreasonable speeds would be necessary.
#'  
#'@details This function uses two heuristics to find watches with suspicious position and/or speed and time information.
#'  
#'  Firstly, it looks for watches whose length in km as calculated by dead-reckoning (speed * time) 
#'  is \emph{significantly} different than that calculated by [geosphere::distGeo()] using start and end
#'  positions. A watch is flagged as significantly different if the difference between the two distance 
#'  measures is \code{>= diff.thresh.pct} percent of the smaller of the two distances. 
#'
#'  Secondly, for watches with both start and end positions, it looks for watches that would require an 
#'  unreasonable speed (greater than \code{speed.thresh}) to achieve that distance during the watch (given by
#'  \code{dat$CalcDurMin}).
#' 
#'@param dat a dataframe, such as returned by \code{\link{ECSAS.extract()}}, containing, at a minimum, fields
#'  named \code{LatStart, LatEnd, LongStart, LongEnd, PlatformSpeed, CalcDurMin.}
#'  
#'@param diff.thresh.pct (numeric, as percentage, default 50) maximum percentage of the smaller of the two distances that the 
#'   difference is allowed to be before being flagged as problematic. 
#'   
#'@param speed.thresh (numeric, in knots, default 20) the function will return watches where the speed required to achieve the 
#'   distance traveled (given by start and end coordinates) would be greater than 
#'   \code{speed.thresh}.
#'   
#'@param leave (numeric, debualt NULL) vector of WatchID's to ignore problems with. Typically these 
#'   will have been vetted by hand.
#'
#'@return Returns \code{dat} with the following additional columns added:
#'
#'\tabular{ll}{
#'   \code{dist_geo_km} \tab distance as calculated by [geosphere::distGeo()] \cr
#'   \code{dist_dr_km} \tab distance as calculated by dead reckoning: \eqn{PlatformSpeed * (CalcDurMin / 60) * 1.852}.
#'      Note the conversion from nautical miles to km.\cr
#'   \code{dist_diff_km} \tab difference in the two distances \cr
#'   \code{dist_pct_diff} \tab percentage difference (of the smaller distance) \cr
#'   \code{reqd_speed} \tab the speed (in knots) that would be required to move from the start location to the 
#'      end location during \code{CalcDurMin}
#' }
#' 
#'@note Only returns one row per watch even if there are multiple observations per watch.
#'
#'@section Author:Dave Fifield
#'
#'@seealso \code{\link{ECSAS.extract()}}
#'
ECSAS.find.suspicious.posn <- function(dat = NULL,
                                       diff.thresh.pct = 50,
                                       speed.thresh = 20,
                                       leave = NULL) {
  
  # need to do arg checking
  
  dat %>% 
    dplyr::distinct(CruiseID, WatchID, .keep_all = T) %>% 
    dplyr::mutate(dist_dr_km = PlatformSpeed * (CalcDurMin / 60) * 1.852,
           dist_geo_km = geosphere::distGeo(cbind(LongStart, LatStart), cbind(LongEnd,  LatEnd))/1000,
           dist_diff_km = abs(dist_dr_km - dist_geo_km),
           pct_diff = dplyr::case_when(dist_dr_km <= dist_geo_km ~ (dist_diff_km/dist_dr_km) * 100,
                                       TRUE ~ (dist_diff_km/dist_geo_km) * 100),
           reqd_speed = dplyr::case_when(is.na(LatStart) | is.na(LongStart) | is.na(LatEnd) | is.na(LongEnd) ~ 
                                                NA_real_,
            TRUE ~ (dist_geo_km/1.852)/(CalcDurMin/60))
    ) %>% 
    dplyr::filter((pct_diff >= diff.thresh.pct | reqd_speed > speed.thresh) &
                   !(WatchID %in% leave))
}