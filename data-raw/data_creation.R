read_one_country <- function(country) {

# files list:
  tmp <- paste0("data-raw/", country)
  files <- paste0(tmp, "/", dir(tmp))

# IDs:
  ids <- sub("^.*_(.*)-kml\\.kml$", "\\1", files)

# the list of SpatialPolygonsDataFrame objects:
  klms <- lapply(files, function(x) rgdal::readOGR(x, "sql_statement"))

# extracting the polygons:
  the_polygons <- lapply(klms, function(x) x@polygons[[1]])

# assigning the IDs of each of these Polygons objects:
  for(i in seq_along(the_polygons)) the_polygons[[i]]@ID <- ids[i]

# making it a SpatialPolygons object:
  reserves_sp <- SpatialPolygons(the_polygons, proj4string = klms[[1]]@proj4string)

# function that reads the attributes for one nature reserve:
  read_attributes <- function(x) {
    a <- xml2::read_xml(x)
    tmp <- xml2::as_list(a)[[c(2, 4)]]$Placemark$ExtendedData$SchemaData
    att <- sapply(tmp, function(x) attr(x, "name"))
    content <- unlist(tmp[!sapply(att, is.null)])
    setNames(content, unlist(att))
  }

# the attributes table for all the nature reserves:
  attr_data <- lapply(files, read_attributes)
  attr_data <- lapply(attr_data, function(x) data.frame(t(x)))
  attr_data <- dplyr::bind_rows(attr_data)
  rownames(attr_data) <- ids

# Making and returning the SpatialPolygonsDataFrame object:
  SpatialPolygonsDataFrame(reserves_sp, attr_data)
}

# ------------------------------------------------------------------------------

vietnam <- read_one_country("vietnam")
devtools::use_data(vietnam, overwrite = TRUE)

# ------------------------------------------------------------------------------
