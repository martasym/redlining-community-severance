#' Function coming from original version of et package developed at BSC (https://earth.bsc.es/gitlab/es/et) by Laura Cifuentes and Roberto Serrano 
#' Interpolate 2-dimensional data to a set of points over time
#'
#' This function interpolates a 3D-array (2 space dimensions and 1 time dimension) of data to a 2D-array (1 time dimension and 1 station dimension), using bilinear interpolation given the corresponding weights and coordinates.
#' @param xin Array of data to be interpolated of dimensions x, y, time.
#' @param weight Dataframe containing a pair of weights \code{wx,wy} and a pair of coordinates \code{ix,iy} per each station \code{stationId}.
#' @return 2D-array of interpolated data
#' @export

compute_weighted_data = function (xin, weight) {
  
  wxc<-weight$wx
  wyc<-weight$wy
  ixc<-weight$ix
  iyc<-weight$iy

  time = dim(xin)[3]
  npts = length(wxc)
  xout<-matrix(data = NA, nrow = npts, ncol = time)
  for (klev in seq(1:time)){  #loop for time
    for (stat in seq(1:npts)){ #loop over inm stations
      np=stat
      if (ixc[np] != 0 && iyc[np] != 0){
        xout[stat,klev] = (1.-wxc[np])*(1.-wyc[np])*xin[ixc[np]  ,iyc[np]  ,klev] +
          wxc[np] *(1.-wyc[np])*xin[ixc[np]+1,iyc[np],klev] + 
          (1.-wxc[np]) * wyc[np] * xin[ixc[np],iyc[np]+1,klev] +
          wxc[np] * wyc[np] * xin[ixc[np]+1,iyc[np]+1,klev]
      } else {
        xout[stat,klev] <- NA
      }    
    }
  }
  return(xout)
}
