#This file gives functions to construct osm tiles and do geographic analysis.

#Functions to create maps
sec <- function(x) {
  1 / cos(x)
}

lonlat2xy <- function(lat_deg, lon_deg, zoom) {
  n <- 2^zoom
  
  x <- (n * (lat_deg + 180)) %/% 360
  lon_rad <- lon_deg * pi / 180
  y <- (n * (1 - log(tan(lon_rad) + sec(lon_rad)) / pi)) %/% 2
  
  list(x = x, y = y)
}

xy2lonlat <- function(x, y, zoom) {
  n <- 2^zoom
  
  lon_deg <- x / n * 360.0 - 180.0
  lat_rad <- atan(sinh(pi * (1 - 2 * y / n)))
  lat_deg <- lat_rad * 180.0 / pi
  
  list(lon_deg = lon_deg, lat_deg = lat_deg)
}

get_tile <- function(url) {
  # build a local path
  path <- stringr::str_extract(url, "/\\d+/\\d+/\\d+.png")
  local_png <- here::here(file.path("data", "osm-tiles", path))
  
  if (!file.exists(local_png)) {
    dir.create(dirname(local_png), showWarnings = FALSE, recursive = TRUE)
    
    # add header
    h <- curl::new_handle()
    curl::handle_setheaders(h, `User-Agent` = "Yutani's blog post")
    
    curl::curl_download(url, destfile = local_png)
  }
  
  png::readPNG(local_png)
}