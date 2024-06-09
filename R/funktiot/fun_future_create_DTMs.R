
# Clip LAS data on the supplied points, create DMS with fixed parameters, and
# save DTMs on disk for further use.
# DTM creation on the 1st time is slow, especially for more points and more DTM types

# Possible to do clean DTM creation, skipping DTM check from disk (slow)

fut_create_DTMs <- function(ktlg = NA,
                        lons, lats, ids,
                        margin, # Margin for clipping the LAS data
                        folder,
                        clean = F, #force new dtms, dont attempt reading from disk
                        dtm_type = NULL, # multi-choice or NULL for all
                        reso # Starting resolution for LAS DTMs
) {
  
  h <- margin # Define clip size (margin) to be shorter
  
  # Define model names to be used in folder and variable structure
  modelnames <-
    c("las_avg","tin","krig15","krig30","krig50","krig100",
      "knnidw15_2","knnidw15_3","knnidw50_2","knnidw50_3")
  
  # Check input validity (not comprehensive)
  if (!is.null(dtm_type)) {
    selection <- match.arg(dtm_type, modelnames, several.ok = T)
    if (!all(dtm_type %in% selection))
      warning(dtm_type[!dtm_type %in% selection]," is not valid choice! Ignored.")
  } else selection <- NULL
  
  # Merge input vectors into one variable
  df <- data.frame(ids, lons,lats) |>
    `names<-`(c("id","lon","lat"))
  
  
  
  # Check/create folders for DTM storage / loading
  if (grepl(D$data, folder))
    dtm_trunkpath <- check_dir(folder, ensure = T)
  else stop("Attempted to save DTMs outside the data folder ",D$data)
  
  # Create sub-folders for each model and save paths in a variable
  if (is.null(selection)) { # If DTMs requested for all models, use all 'modelnames'
    dtm_dir <- lapply(file.path(dtm_trunkpath,modelnames), check_dir, ensure=T)
    names(dtm_dir) <- modelnames
    
    # Create a df for storing the read elevations
    elev_df <- data.frame(matrix(ncol = length(modelnames), nrow = nrow(df)))
    colnames(elev_df) <- paste0(modelnames,"_elev")
    
  } else { # dtm_type=selection!=NULL, consider only requested models
    dtm_dir <- lapply(file.path(dtm_trunkpath,selection), check_dir, ensure=T)
    names(dtm_dir) <- selection
    elev_df <- data.frame(matrix(ncol = length(selection), nrow = nrow(df)))
    colnames(elev_df) <- paste0(selection,"_elev")
  }
  
  
  # Iterate through all supplied points.
  # Create various DTMs for every point and read the elevations
  future::plan("multisession", workers = future::availableCores()) # Prep future
  
  elev_df <- future.apply::future_lapply(
    seq_len(nrow(df)), # Iterate through df rows
    future.seed = T, # Gives error without
    FUN = function(i) {
    
    # Form the paths on which to save and load DTMs
    dtm_filepaths <- lapply(
      dtm_dir, file.path, paste0(df[[i, "id"]],".rds"))
    
    # Get point coordinates
    lon <- df[i, "lon"]
    lat <- df[i, "lat"]
    
    
      # Rajataan pistepilven alue annetusta katalogista (Round for consistency)
      paikka_laz <- lidR::clip_rectangle(ktlg,
                                         round(lon - h), round(lat - h),
                                         round(lon + h), round(lat + h))
      if (lidR::is.empty(paikka_laz)) {
        warning(p("Paikan",df[i,"id"],"pistepilvi tyhjä. DTM:iä ei luotu"),immediate.=T)
        return(FALSE) # Continue to next index
      }
      # Otetaan mukaan vain maanpinta
      paikka_laz <- lidR::filter_poi(paikka_laz, Classification == 2)
    
    # Create DTMs with multiple methods. Do all, or only those requested.
    # Also reads elevations of the given points from requested DTMs
    
    # LAS average elevation (bilinear fill)
    if ("las_avg" %in% selection || is.null(selection)) {
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
    }
      
      # TODO: Create a pattern to loop through the functions in "algorithm"
      # Done by setting "modelnames" to a named list containing the funs
    
    ##### Triangulated Irregular Network (TIN)
    if ("tin" %in% selection || is.null(selection)) {
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
    }
    
    ##### Kriging + KNN
    if ("krig15" %in% selection || is.null(selection)) {
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
    }
    if ("krig30" %in% selection || is.null(selection)) {
      if (file.exists(dtm_filepaths$krig30) && clean == F) {
        dtm_krig30 <- terra::rast(readRDS(dtm_filepaths$krig30))
      } else {
        if (!exists("paikka_laz"))
          stop(dtm_filepaths$krig30," not found on disk and no LAS data\n",df[i,])
        dtm_krig30 <- lidR::rasterize_terrain(
          paikka_laz,
          res = reso,
          algorithm = lidR::kriging(k = 30L))
        saveRDS(terra::wrap(dtm_krig30), file = dtm_filepaths$krig30)
      }
    }
    if ("krig50" %in% selection || is.null(selection)) {
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
    }
    if ("krig100" %in% selection || is.null(selection)) {
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
    }
    if ("knnidw15_2" %in% selection || is.null(selection)) {
      if (file.exists(dtm_filepaths$knnidw15_2) && clean == F) {
        dtm_knnidw15_2 <- terra::rast(readRDS(dtm_filepaths$knnidw15_2))
      } else {
        if (!exists("paikka_laz"))
          stop(dtm_filepaths$knnidw15_2," not found on disk and no LAS data\n",df[i,])
        dtm_knnidw15_2 <- lidR::rasterize_terrain(paikka_laz, res = reso,
          algorithm = lidR::knnidw(k = 15, p = 2))
        saveRDS(terra::wrap(dtm_knnidw15_2), file = dtm_filepaths$knnidw15_2)
      }
    }
    if ("knnidw15_3" %in% selection || is.null(selection)) {
      if (file.exists(dtm_filepaths$knnidw15_3) && clean == F) {
        dtm_knnidw15_3 <- terra::rast(readRDS(dtm_filepaths$knnidw15_3))
      } else {
        if (!exists("paikka_laz"))
          stop(dtm_filepaths$knnidw15_3," not found on disk and no LAS data\n",df[i,])
        dtm_knnidw15_3 <- lidR::rasterize_terrain(paikka_laz, res = reso,
                            algorithm = lidR::knnidw(k = 15, p = 3))
        saveRDS(terra::wrap(dtm_knnidw15_3), file = dtm_filepaths$knnidw15_3)
      }
    }
    if ("knnidw50_2" %in% selection || is.null(selection)) {
      if (file.exists(dtm_filepaths$knnidw50_2) && clean == F) {
        dtm_knnidw50_2 <- terra::rast(readRDS(dtm_filepaths$knnidw50_2))
      } else {
        if (!exists("paikka_laz"))
          stop(dtm_filepaths$knnidw50_2," not found on disk and no LAS data\n",df[i,])
        dtm_knnidw50_2 <- lidR::rasterize_terrain(paikka_laz, res = reso,
                            algorithm = lidR::knnidw(k = 50, p = 2))
        saveRDS(terra::wrap(dtm_knnidw50_2), file = dtm_filepaths$knnidw50_2)
      }
    }
    if ("knnidw50_3" %in% selection || is.null(selection)) {
      if (file.exists(dtm_filepaths$knnidw50_3) && clean == F) {
        dtm_knnidw50_3 <- terra::rast(readRDS(dtm_filepaths$knnidw50_3))
      } else {
        if (!exists("paikka_laz"))
          stop(dtm_filepaths$knnidw50_3," not found on disk and no LAS data\n",df[i,])
        dtm_knnidw50_3 <- lidR::rasterize_terrain(paikka_laz, res = reso,
                            algorithm = lidR::knnidw(k = 50, p = 3))
        saveRDS(terra::wrap(dtm_knnidw50_3), file = dtm_filepaths$knnidw50_3)
      }
    }
    message("DTM(s) created for point ",df[i,"id"])
  })
  
  future::plan("sequential") # Close parallel processing.
  message("All requested DTMs created")
}


