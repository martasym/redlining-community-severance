#' Function coming from original version of et package developed at BSC (https://earth.bsc.es/gitlab/es/et) by Laura Cifuentes and Roberto Serrano 
#' Compute weights for grid to point interpolation
#'
#' This function computes the closest coordinate points and weights associated for the interpolation of a set of station-points (lat,lon) for a given grid of latitudes and longitudes
#' @param lat Matrix containing the grid of latitudes to interpolate from.
#' @param lon Matrix containing the grid of longitudes to interpolate from.
#' @param lon_st Vector of station longitudes.
#' @param lat_st Vector of station latitudes.
#' @param stationId Vector of station IDs.
#' @return Dataframe containing the weights (wx,wy) and the closest coordinate (ix,iy) for each station (stationId).
#' @examples 
#' lat1 = seq(30,50,by=3)
#' lon1 = seq(-10,10, by=3)
#' lat2 <-t(matrix(rep(lat1,length(lon1)),length(lat1),length(lon1)))
#' lon2 <-(matrix(rep(lon1,length(lat1)),length(lon1),length(lat1)))
#' calcul_weights(lat = lat2,
#'                lon = lon2,
#'                lat_st = c(40),
#'                lon_st = c(2), 
#'                stationId = c("ID001"))
#' @export 

calcul_weights = function (lat, lon, lon_st, lat_st, stationId) {
  
  latWRF <- lat
  lonWRF <- lon
  
  xc<-lonWRF[,]
  yc<-latWRF[,]
  x<- lon_st
  y<- lat_st
  wx<-numeric()
  wy<-numeric()
  ix<-numeric()
  iy<-numeric()
  npts=length(x) 
  nxx=length(xc[,1]) 
  nyy=length(xc[1,])
  ntry=100
  
  for (i in seq(1:npts)){ # loop over all interpolation points
    iflag = 0
    # Search whether the point is within one of the polygons               
    cat(i,"/",npts,"\n")
    for (nx in seq(1:(nxx-1))){
      for (ny in seq(1:(nyy-1))){
        nsec = 0
        if(iflag == 0){
          if( (yc[nx+1,ny+0]!=yc[nx+0,ny+0])
              && (y[i]<=max(yc[nx+1,ny+0],yc[nx+0,ny+0])) 
              && (y[i]>=min(yc[nx+1,ny+0],yc[nx+0,ny+0])) ){
            a = (yc[nx+1,ny+0]-y[i]) / (yc[nx+1,ny+0]-yc[nx+0,ny+0])
            if(a>=0 && a<1){
              b = a *(xc[nx+0,ny+0]-x[i]) + (1-a)*(xc[nx+1,ny+0]-x[i])
              if(b>0){ nsec = nsec + 1 }
            }
          }
          if( (yc[nx+1,ny+1]!=yc[nx+1,ny+0])
              && (y[i]<=max(yc[nx+1,ny+1],yc[nx+1,ny+0]))
              && (y[i]>=min(yc[nx+1,ny+1],yc[nx+1,ny+0])) ){ 
            a = (yc[nx+1,ny+1]-y[i]) / (yc[nx+1,ny+1]-yc[nx+1,ny+0])
            if(a>=0 && a<1){ 
              b = a *(xc[nx+1,ny+0]-x[i]) + (1-a)*(xc[nx+1,ny+1]-x[i])
              if(b>0){ nsec = nsec + 1 }
            }
          }
          if( (yc[nx+0,ny+1]!=yc[nx+1,ny+1])
              && (y[i]<=max(yc[nx+0,ny+1],yc[nx+1,ny+1]))
              && (y[i]>=min(yc[nx+0,ny+1],yc[nx+1,ny+1])) ){ 
            a = (yc[nx+0,ny+1]-y[i]) / (yc[nx+0,ny+1]-yc[nx+1,ny+1])
            if(a>=0 && a<1){ 
              b = a *(xc[nx+1,ny+1]-x[i]) + (1-a)*(xc[nx+0,ny+1]-x[i])
              if(b>0){ nsec = nsec + 1 }
            }
          }
          if( (yc[nx+0,ny+0]!=yc[nx+0,ny+1])
              && (y[i]<=max(yc[nx+0,ny+0],yc[nx+0,ny+1]))
              && (y[i]>=min(yc[nx+0,ny+0],yc[nx+0,ny+1])) ){ 
            a = (yc[nx+0,ny+0]-y[i]) / (yc[nx+0,ny+0]-yc[nx+0,ny+1])
            if(a>=0 && a<1){ 
              b = a *(xc[nx+0,ny+1]-x[i]) + (1-a)*(xc[nx+0,ny+0]-x[i])
              if(b>0){ nsec = nsec + 1 }
            }
          }
          if(x[i]==xc[nx+0,ny+0] && y==yc[nx+0,ny+0]){nsec = 1}
          if(x[i]==xc[nx+1,ny+0] && y==yc[nx+1,ny+0]){nsec = 1}
          if(x[i]==xc[nx+1,ny+1] && y[i]==yc[nx+1,ny+1]){nsec = 1}
          if(x[i]==xc[nx+0,ny+1] && y[i]==yc[nx+0,ny+1]){nsec = 1}
          
          if(nsec == 1){ 
            ix[i] = nx
            iy[i] = ny
            iflag = 1
          }
        } # if iflag=0
      } #enddo
    } #enddo
    
    # Search for weights                                                   
    if(iflag == 1){
      d2min = 1e20
      nx = ix[i]
      ny = iy[i]
      for (ntx2 in seq(1:(ntry+1))){
        ntx = ntx2-1
        a0 = ntx/ntry
        a1 = 1. - a0
        for(nty2 in seq(1:(ntry+1))){ 
          nty = nty2-1
          b0 = nty/ntry
          b1 = 1. - b0
          xx =a0*b0*xc[nx+1,ny+1]+a1*b0*xc[nx,ny+1]+a0*b1*xc[nx+1,ny]+a1*b1*xc[nx,ny]
          yy =a0*b0*yc[nx+1,ny+1]+a1*b0*yc[nx,ny+1]+a0*b1*yc[nx+1,ny]+a1*b1*yc[nx,ny]          
          d2 = (x[i]-xx)*(x[i]-xx)+(y[i]-yy)*(y[i]-yy)
          if(d2<d2min){ 
            d2min = d2
            a  = a0
            b  = b0
          }
        }
      }
      wx[i]=a
      wy[i]=b
    } else {
      ix[i]=0
      iy[i]=0
      wx[i]=0
      wy[i]=0
    }
  } #enddo
  data = data.frame(wx,wy,ix,iy,stationId)
  return(data)
}
