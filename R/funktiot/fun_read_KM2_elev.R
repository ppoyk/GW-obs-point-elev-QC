# Function to read KM2 DTM elevations from multiple files.
# Files are distinguished by and ID, followed by '_'.
# (More flexible naming of files not implemented)
# The returned elevation is associated with the provided ID.

read_KM2_elev <- function(folder, ids, lon, lat) {
  # Combine given parameters to a table
  coords <- data.frame(id = ids, lon = lon, lat = lat)

  # Init results var
  results <- data.frame(id = ids, elev = rep(NA, length.out = length(ids)))

  # Loop through provided ids, try to find matching file
  for (id in ids) {
    # Search saved file by id. If found, load as raster
    file <- list.files(folder,
      pattern = paste0("^", id, "_.*\\.tif$"),
      ignore.case = F, full.names = T
    )
    if (!rlang::is_empty(file)) {
      file <- terra::rast(file)
      # Extract elevation from the file on the measured reference point coordinates
      km2_elev <- terra::extract(
        file,
        coords[coords[, 1] == id, c("lon", "lat")],
        xy = F, ID = F
      )[1, ]
      # Insert elev to column by paikka_id
      results[results$id == id, "elev"] <- km2_elev
    } else { # File not found, give NA elevation
      results[results$id == id, "elev"] <- NA_real_
    }
  }

  return(results)
}
