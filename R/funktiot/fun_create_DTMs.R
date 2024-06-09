
# Clip LAS data on the supplied points, create DMS with fixed parameters, save
# DTMs on disk for further use, and read the elevation of said DTMs.
# 
# Coords where elevations are read can be shifted within DTM, default is clip center

# Possible to do clean DTM creation, skipping DTM check from disk (slow)

#================== DEPRECATED (not used in the project) ======================#
# Superseded by fut_create_DTMs. Simpler function with parallel processing.
# Also with more DTM creation algorithm options

create_DTMs <- function(ktlg = NA,
                        lons, lats, ids,
                        margin, # Margin for clipping the LAS data
                        elev_lons = lons, elev_lats = lats,
                        folder,
                        clean = F, #force new dtms, dont attempt reading from disk
                        dtm_type = NULL, # multi-choice or NULL for all
                        reso, # Starting resolution for LAS DTMs
                        elev_only #Read only elev (requires all id files on disk)
                        ) {
  
#==============================================================================#
.Deprecated("fut_create_DTMs")
#==============================================================================#
  
h <- margin # Define clip size (margin) to be shorter

# Define model names to be used in folder and variable structure
modelnames <- c("las_avg","tin","krig15","krig50","krig100")

# Check input validity (not comprehensive)
if (any(!dplyr::between(elev_lons, lons-h, lons+h) | 
        !dplyr::between(elev_lats, lats-h, lats+h)))
  stop("Elevation reading coordinates outside of clip margins")
if (!is.null(dtm_type)) {
  selection <- match.arg(dtm_type, modelnames, several.ok = T)
  if (!all(dtm_type %in% selection))
    warning(dtm_type[!dtm_type %in% selection]," is not valid choice! Ignored.")
} else selection <- NULL

# Merge input vectors into one variable
df <- data.frame(ids, lons,lats, elev_lons,elev_lats) |>
  `names<-`(c("id","lon","lat","elev_lon","elev_lat"))



# Check/create folders for DTM storage / loading
if (grepl(D$data, folder))
  dtm_trunkpath <- check_dir(folder, ensure = T)
else stop("Attempted to save DTMs outside the data folder ",D$data)
# Create sub-folders for each model and save paths in a variable
dtm_dir <- lapply(file.path(dtm_trunkpath,modelnames), check_dir, ensure = T)
names(dtm_dir) <- modelnames

# Create a df for storing the read elevations
elev_df <- data.frame(matrix(ncol = length(modelnames), nrow = nrow(df)))
colnames(elev_df) <- paste0(modelnames,"_elev")

# Iterate through all supplied points.
# Create various DTMs for every point and read the elevations
for (i in 1:nrow(df)) {
  
  # Form the paths on which to save and load DTMs
  dtm_filepaths <- lapply(
    dtm_dir, file.path, paste0(df[[i, "id"]],".rds"))
  
  # Get point coordinates
  lon <- df[i, "lon"]
  lat <- df[i, "lat"]
  
  if (elev_only == T) {
    # Rajataan pistepilven alue paikkakohtaisesta katalogista (Round for consistency)
    paikka_laz <- lidR::clip_rectangle(ktlg,
      round(lon-h), round(lat-h), round(lon+h), round(lat+h))
    if (lidR::is.empty(paikka_laz)) {
      warning(paste("Paikan",df[i,"id"],"pistepilvi tyhjä. DTM:iä ei luotu"))
      next 
    }
    # Otetaan mukaan vain maanpinta
    paikka_laz <- lidR::filter_poi(paikka_laz, Classification == 2)
  }
  
  # Create DTMs with multiple methods. Do all, or only those requested.
  # Also reads elevations of the given points from requested DTMs

  # LAS average elevation (bilinear fill)
  if (modelnames[[1]] %in% selection || is.null(selection)) {
    # Create LAS average DTM (if not found or clean run requested)
    if (file.exists(dtm_filepaths$las_avg) && clean == F) {
      dtm_las <- terra::rast(readRDS(dtm_filepaths$las_avg))
    } else {
      if (!exists("paikka_laz"))
        stop(dtm_filepaths$las_avg," not found on disk and no LAS data\n",df[i,])
      # Create the highest resolution dtm (sparse data leaves holes, filled below)
      dtm_las <- lidR::pixel_metrics(paikka_laz, ~mean(Z), res = reso)
      # Täydennetään puolet alkuresol. olevalla, bilineaar. interpoloidul. aineistolla
      dtm2x <- terra::disagg(lidR::pixel_metrics(paikka_laz, ~mean(Z), res = reso * 2),
                             fact = reso * 4,
                             method = "bilinear")
      # Täydennetään tulokset tyhjille alueille (resample täsmää eri lähtöresolla
      # muodostetun rasterin alkuperäisen rajoihin ja solumäärään (mm. rajaus))
      dtm_las <- terra::cover(dtm_las,
                              terra::resample(dtm2x, dtm_las, method = "near"))
      # Otetaan neljäsosaresoluutio interpoloituna
      dtm4x <- terra::disagg(lidR::pixel_metrics(paikka_laz, ~mean(Z), res = reso * 4),
                             fact = reso * 8,
                             method = "bilinear")
      dtm_las <- terra::cover(dtm_las,
                              terra::resample(dtm4x, dtm_las, method = "near"))
      # Sama kahdeksasosaresoluutiolla
      dtm8x <- terra::disagg(lidR::pixel_metrics(paikka_laz, ~mean(Z), res = reso * 8),
                             fact = reso * 16,
                             method = "bilinear")
      dtm_las <- terra::cover(dtm_las,
                              terra::resample(dtm8x, dtm_las, method = "near"))

      # Save on disk for future use (creation can be skipped in future)
      saveRDS(terra::wrap(dtm_las), file = dtm_filepaths$las_avg)
    }
    # Read DTM elevation from the requested coordinates
    elev_df[i,"las_avg_elev"] <- terra::extract(dtm_las,
                                                df[i, c("elev_lon","elev_lat")],
                                                ID = FALSE)
  }
  
  ##### Triangulated Irregular Network (TIN)
  if (modelnames[[2]] %in% selection || is.null(selection)) {
    if (file.exists(dtm_filepaths$tin) && clean == F) {
      dtm_tin <- terra::rast(readRDS(dtm_filepaths$tin))
    } else {
      if (!exists("paikka_laz"))
        stop(dtm_filepaths$tin," not found on disk and no LAS data\n",df[i,])
      dtm_tin <- lidR::rasterize_terrain(
        paikka_laz,
        res = reso,
        algorithm = lidR::tin())
      saveRDS(terra::wrap(dtm_tin), file = dtm_filepaths$tin)
    }
    elev_df[i,"tin_elev"] <- terra::extract(dtm_tin,
                                            df[i, c("elev_lon","elev_lat")],
                                            ID = FALSE)
  }
  
  ##### Kriging + KNN
  if (modelnames[[3]] %in% selection || is.null(selection)) {
    if (file.exists(dtm_filepaths$krig15) && clean == F) {
      dtm_krig15 <- terra::rast(readRDS(dtm_filepaths$krig15))
    } else {
      if (!exists("paikka_laz"))
        stop(dtm_filepaths$krig15," not found on disk and no LAS data\n",df[i,])
      dtm_krig15 <- lidR::rasterize_terrain(
        paikka_laz,
        res = reso,
        algorithm = lidR::kriging(k = 15L))
      saveRDS(terra::wrap(dtm_krig15), file = dtm_filepaths$krig15)
    }
    elev_df[i,"krig15_elev"] <- terra::extract(dtm_krig15,
                                               df[i, c("elev_lon","elev_lat")],
                                               ID = FALSE)
  }
  if (modelnames[[4]] %in% selection || is.null(selection)) {
    if (file.exists(dtm_filepaths$krig50) && clean == F) {
      dtm_krig50 <- terra::rast(readRDS(dtm_filepaths$krig50))
    } else {
      if (!exists("paikka_laz"))
        stop(dtm_filepaths$krig50," not found on disk and no LAS data\n",df[i,])
      dtm_krig50 <- lidR::rasterize_terrain(
        paikka_laz,
        res = reso,
        algorithm = lidR::kriging(k = 50L))
      saveRDS(terra::wrap(dtm_krig50), file = dtm_filepaths$krig50)
    }
    elev_df[i,"krig50_elev"] <- terra::extract(dtm_krig50,
                                               df[i, c("elev_lon","elev_lat")],
                                               ID = FALSE)
  }
  if (modelnames[[5]] %in% selection || is.null(selection)) {
    if (file.exists(dtm_filepaths$krig100) && clean == F) {
      dtm_krig100 <- terra::rast(readRDS(dtm_filepaths$krig100))
    } else {
      if (!exists("paikka_laz"))
        stop(dtm_filepaths$krig100," not found on disk and no LAS data\n",df[i,])
      dtm_krig100 <- lidR::rasterize_terrain(
        paikka_laz,
        res = reso,
        algorithm = lidR::kriging(k = 100L))
      saveRDS(terra::wrap(dtm_krig100), file = dtm_filepaths$krig100)
    }
    elev_df[i,"krig100_elev"] <- terra::extract(dtm_krig100,
                                                df[i, c("elev_lon","elev_lat")],
                                                ID = FALSE)
  }
  message("DTM(s) created for point ",df[i,"id"])
}

elev_df <- cbind(ids,elev_df) # Combine with supplied IDs
colnames(elev_df)[1] <- "id" # Set correct output col name for IDs 
return(elev_df)

}


