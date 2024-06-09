


read_DTM_elev <- function(folder, ids, lon, lat, c_name = NULL) {
  
  # Combine inputs
  df <- data.frame(id = ids, lon = lon, lat = lat)
  
  # Read elevations using the df
  future::plan("multisession")
  elev_df <- future.apply::future_lapply(seq_len(nrow(df)),
                                         future.seed = T,#Required to work
                                         FUN = function(r) {
        # Search for the file in folder, based on ID
    fil <- list.files(folder, paste0("^", df[r,"id"],"\\.rds$"), full.names = T)
    
    # Return NA for elevation if file not found
    if (rlang::is_empty(fil)) {
      warning("DTM for ",df[r,"id"]," not found!")
      row <- data.frame(id = df[r,"id"], el = NA_real_)
      names (row) <- c("id","elev"); return(row)
    } else {
      # Read elevation from file, and return it
      dtm <- terra::rast(readRDS(fil))
      # Extract elevation from the DTM in given coordinates
      elev <- terra::extract(dtm,
                           df[r, c("lon","lat")],
                           ID = FALSE)
      # Combine results with ID, which is returned
      row <- data.frame(id = df[r,"id"], el = elev)
      names(row) <- c("id","elev"); return(row)
      }
    })
  future::plan("sequential")
  
  elev_df <- do.call(rbind, elev_df) # Format from list into df
  # Rename cols of output
  if (is.null(c_name))
    colnames(elev_df) <- c("id", "elev")
  else colnames(elev_df) <- c("id", c_name)

  return(elev_df)
}

