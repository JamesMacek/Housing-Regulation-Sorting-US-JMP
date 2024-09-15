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
  local_png <- here::here(file.path("DataV2", "osm-tiles", path))
  
  if (!file.exists(local_png)) {
    dir.create(dirname(local_png), showWarnings = FALSE, recursive = TRUE)
    
    # add header
    h <- curl::new_handle()
    curl::handle_setheaders(h, `User-Agent` = "Yutani's blog post")
    
    curl::curl_download(url, destfile = local_png)
  }
  
  png::readPNG(local_png)
}

createMappingArgs <- function(boundingBox,#bounding box is coordinates of box map
                             additionalZoom) { #additional zoom will do something to make map pretty
  
  #Extracting open street map tiles to overlay zoning districts
  # see https://yutani.rbind.io/post/2018-06-09-plot-osm-tiles/ for code
  x_len <-  boundingBox["xmax"] -  boundingBox["xmin"]
  y_len <-  boundingBox["ymax"] -  boundingBox["ymin"]
  
  # calculate the minimum zoom level that is smaller than the lengths
  x_zoom <- sum(x_len < 360 / 2^(0:19)) - 1
  y_zoom <- sum(y_len < 170.1022 / 2^(0:19)) - 1
  zoom <- min(x_zoom, y_zoom)
  
  #add additional zoom
  zoom <- zoom + additionalZoom
  rm(x_zoom, y_zoom)
  
  #Create tiles
  xy <- lonlat2xy(boundingBox[c("xmin", "xmax")],  boundingBox[c("ymin", "ymax")], zoom)
  tiles <- expand.grid(x = seq(xy$x["xmin"], xy$x["xmax"]),
                       y = seq(xy$y["ymin"], xy$y["ymax"]))
  
  #Open street map api tiles
  urls <- sprintf("https://a.tile.openstreetmap.org/%d/%d/%d.png", zoom, tiles$x, tiles$y)
  
  #Using get tiles function for pngs, read them directly into R
  pngs <- map(urls, get_tile)
  
  #Getting tile positions
  nw_corners <- pmap_dfr(tiles, xy2lonlat, zoom = zoom)
  # add 1 to x and y to get the south-east corners
  se_corners <- pmap_dfr(mutate_all(tiles, `+`, 1), xy2lonlat, zoom = zoom)
  
  names(nw_corners) <- c("xmin", "ymax")
  names(se_corners) <- c("xmax", "ymin")
  
  tile_positions <- bind_cols(nw_corners, se_corners)
  rm(se_corners, nw_corners)
  
  #Setting up data to use pmap for plotting
  args <- tile_positions %>% mutate(raster = pngs)
  
  return(args)
  
}

