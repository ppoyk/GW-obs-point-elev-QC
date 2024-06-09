# This script compares the measured GPS points in monitoring pipe locations to
# the reported elevations of various DTMs in the same locations.
# 
# GPS observations are considered more accurate than DTMs, so the DTM performance
# can be compared, unless the differences between the DTMs is less than the
# accuracy of GPS measurements.



# Create df with no duplicate GPS points
ref_GPS <- ref_mittaukset[
  !duplicated(cbind(ref_mittaukset$North, ref_mittaukset$East)), ]
# Rename coordinate columns for easier manipulation (terra::)
ref_GPS <- dplyr::rename(ref_GPS, lon = East)
ref_GPS <- dplyr::rename(ref_GPS, lat = North)


#### Download KM2 files for places measured (some not included previously) #####

# Funktio, joka valmistelee aineiston lataamisen Maanmittauslaitoksen 2x2m
# resoluution maankorkeusaineistosta haluttujen koordinaattien ympäriltä.
# Aloittaa siis työn MML:n serverillä.
valmistele_2m_aineisto <- function(lon, lat, marginaali=USERCONF$loc_dtm_margin) {
  
  # Muodostetaan http POST-pyyntö
  # Pyynnön URL määrittely
  req <- httr2::request("https://avoin-paikkatieto.maanmittauslaitos.fi/tiedostopalvelu/ogcproc/v1/processes/korkeusmalli_2m_bbox/execution")
  # Pyynnön headerin/otsikon lisääminen
  req <- httr2::req_headers(req, "Content-Type" = "application/json")
  # Pyynnön vaatimien tietojen määrittely (alueen rajat)
  req <- httr2::req_body_json(req,
                              list(id = "korkeusmalli_2m_bbox",
                                   inputs = list(
                                     boundingBoxInput = c(
                                       lon - marginaali, lat - marginaali,
                                       lon + marginaali, lat + marginaali),
                                     fileFormatInput = "TIFF")))
  # API avaimen määrittely
  req <- httr2::req_url_query(req,
                              `api-key` = check_key(D$secrets, "MML_avain.txt"))
  # Pyynnön suorittaminen 
  vastaus <- httr2::req_perform(req)
  vastaus <- jsonlite::fromJSON(rawToChar(vastaus$body))
  return(vastaus)
}


# Define folder where files will be saved
ref_km2_dir <- check_dir(D$data, "vertaa_dtm", "GPS_KM2", ensure = T)


# Download new files only if any KM2 file is missing, or if download is forced
if (all(file_test("-f",
  f.path(ref_km2_dir, paste0(ref_GPS$paikka_id,"_",ref_GPS$tunnus,".tif"))))
  &&
  !identical(USERCONF$pakota_lataus_KM2, TRUE)) {
  message("All KM2 files for reference measurements found ✅")
} else {

# Prepare and execute TIFF downloads:
if (curl::has_internet()) {
lataustyot <- ref_GPS[, c("paikka_id","tunnus","Point")]
# Aloitetaan joka putkelle lataustyö, ja tallennetaan sen URL tauluun
for (i in 1:nrow(ref_GPS)) {
  # Syötetään putkitaulun rivi valmistelufunktioon
  pisteen_vastaus <- valmistele_2m_aineisto(lon = ref_GPS[[i, "lon"]],
                                            lat = ref_GPS[[i, "lat"]],
                                            marginaali = USERCONF$loc_dtm_margin)
  # Haetaan työn URL valmistelun vastauksesta
  lataustyot[i, "url"] <- pisteen_vastaus$links$href
  # Haetaan työn aloituksen status
  lataustyot[i, "status"] <- pisteen_vastaus$status
}
rm(i, pisteen_vastaus)


# Odotetaan että lataustyö on valmistunut palvelimella ennen kuin ladataan
i <- 1 # Ladattavan indeksin alustaminen
while (i <= nrow(lataustyot)) {
  # Jos työn URL on NA, huomautetaan virheestä
  if (is.na(lataustyot[i, "url"])) {
    stop(paste("GPS-pisteen KM2 lataus ei onnistunut",lataustyot[i,]))
  } else { # Muuten jatketaan normaalisti aineiston lataukseen
    
    # Haetaan työn ajantasainen tieto palvelimelta
    req <- httr2::request(lataustyot[i, "url"])
    req <- httr2::req_url_query(req,
                                `api-key` = check_key(D$secrets, "MML_avain.txt"))
    vastaus <- httr2::req_perform(req)
    vastaus <- jsonlite::fromJSON(rawToChar(vastaus$body))
    
    # Tarkistetaan onko työ epäonnistunut palvelimella
    if (vastaus$status %in% c("failed", "dismissed"))
      stop(paste("VIRHE: Palvelimen vastaus:", vastaus$message))
    
    # Jos työ ei ole valmis palvelimella, odotetaan 2 s
    if (vastaus$progress != 100 || vastaus$status %in% c("running", "accepted")) {
      message(paste("Lataustyön valmistelu palvelimella kesken..."))
      message(paste0("Progress ", vastaus$progress, "/100"))
      Sys.sleep(2)
    } else {
      # Ladataan yhden kiintopisteen alue
      tallennuspolku <- file.path(ref_km2_dir,
                                  paste0(lataustyot[i,"paikka_id"],"_",
                                         lataustyot[i,"tunnus"],".tif"))
      
      # Latauslinkkiin päästään seuraamalla aikaisempaa vastausta
      req <- httr2::request(vastaus$links$href)
      req <- httr2::req_url_query(req,
                                  `api-key`=check_key(D$secrets,"MML_avain.txt"))
      vastaus <- httr2::req_perform(req)
      vastaus <- jsonlite::fromJSON(rawToChar(vastaus$body))
      
      # Jos ladattavan tiedoston koko on 0, siirrytään seuraavaan kp:seen.
      if (vastaus$results$length[1] == 0) {
        message(paste("GPS-piste",lataustyot[i,"tunnus"],"ohitettu. KM2:n ulkopuolella"))
        i <- i + 1
      } else {
        # Muuten ladataan tiedosto
        file_url <- vastaus$results$path[1]
        # Lataa tiedosto (pitää olla binarynä)
        download.file(file_url, tallennuspolku, mode="wb", timeout=120, quiet=T)
        
        # Tarkistetaan että löytyyhän tiedosto levyltä
        if (file.exists(file.path(tallennuspolku))) {
          message(paste("-GPS-pisteen",lataustyot[i,"tunnus"],"aineisto ladattu"))
        } else stop(paste("GPS-pisteen aineistoa ei ladattu:",tallennuspolku))
        
        i <- i + 1 # Siirrytään seuraavaan vasta kun progress=100 ja ladattu.
      }
    }
  }
}
rm(vastaus, file_url, tallennuspolku, req)
message("KM2 elevation model data downloaded for reference points")
} # End download section (internet connection condition)
} # Skip downloads if all files found and download not forced

# Read elevations from KM2 data
ref_GPS[["km2_elev"]] <- NA # Prepare col
for (id in ref_GPS$paikka_id[!is.na(ref_GPS$paikka_id)]) {
  # Search saved file by paikka_id and load as raster
  file <- terra::rast(list.files(ref_km2_dir,
                                 pattern = paste0("^",id,"_",".*\\.tif$"),
                                 ignore.case = F, full.names = T))
  # Extract elevation from the file on the measured reference point coordinates
  km2_elev <- terra::extract(file,
                             ref_GPS[ref_GPS$paikka_id == id, c("lon","lat")],
                             xy = F, ID = F)[1,]
  ref_GPS[ref_GPS$paikka_id == id, "km2_elev"] <- km2_elev # Insert elev to column
}
message("KM2 elevation model elevations read for reference points")
rm(file, km2_elev, id, ref_km2_dir)




##### Create a LAS catalog of the reference measured areas #####################


# Define folder where to save LAS-based reference measurement DTMs
ref_dtm_dir <- check_dir(D$data,"vertaa_dtm","GPS_LAS_DTM", ensure = T)
# Define IDs which are used to save the files
save_ids <- paste0(ref_GPS$tunnus,"_",ref_GPS$paikka_id)


if (USERCONF["ajo_las_alustalla"] == T) { # Scan files only if on LAS platform
  
  # Tarkistetaan, ajetaanko koodia CSC:n Laseralustan ympäristössä, jossa kaikki
  # LAS aineisto saatavilla (Käytetään täyttä aineistoa katalogin skannaukseen)
  D$las <- file.path("L:/automaattinen/") # Osoitetaan suoraan ainestoon laseralustalla
  if (file_test("-d", D$las)) { # Kokeillaan löytyykö polku aineistoon oikeasti
    message("LAS-katalogi tullaan skannaamaan täydestä aineistosta: ",D$las)
    taysi_las <- TRUE # Lippu mahdollista tulevaa tarvetta varten
  } else {
    message("Oletettuja laseraineiston kansioita ei löydy...","Valitse kansio:")
    D$las <- choose_directory(title = "Valitse LAS-aineiston juurikansio")
    writeLines(c("Kansioksi valittu",D$las))
  }
  
# Find files of the production areas where ref measurements were done (Turtakangas
# station in Pyhäjoki prod area, which data is missing. Available at end of 2024)
ref_map_shts <- c("R4134F1","R4134F3", # Turtakangas in Pyhäjoki production area
  "R4332H1","R4332H3","R4334B1",
  "R4332G2","R4332G4","R4334A2","R4334A4",
  "R4332G1","R4332G3","R4334A1","R4334A3", # Rokua in Vaala p.a.
  "R5121E4","R5121G2","R5121E3","R5121G1", #Alakangas in Vaala/Ristijärvi p.a.'s
  "S4332G2","S4332G4","S4332G1","S4332G3","S4334A1") #Vengasvaara in Pudasjärvi p.a.

ref_las_files <- shell(paste0(
  'find -L ',D$las,
  ' -regex ".*\\(',paste0(ref_map_shts, collapse = "\\|"), 
  '\\).*\\.laz$"', # Files w/ prod area in path, and ending in .laz
  ' -type f'), # Files only
  intern = T)

# Drop duplicated files of the same location. Leave only one in newest year folder  
if(anyDuplicated(basename(ref_las_files))) {
  for (fi in unique(basename(ref_las_files))) {
    if (sum(grepl(fi, ref_las_files)) > 1) { # If file occurs more than once
      dupfiles <- ref_las_files[grepl(fi, ref_las_files)]
      # Get file years from the path (root year folder)
      fileyears <- as.numeric(sapply(dupfiles, substr,
                          start = nchar(D$las)+2, stop = nchar(D$las)+5))
      maxyear <- max(fileyears)
      # Drop files which are older than newest of the duplicate files
      dropped_files <- dupfiles[which(fileyears < maxyear)]
      # If more than 1 file remains, halt. More checks needed on what to scan
      if (length(dropped_files) < length(dupfiles) - 1) 
        stop("Too many files with same filename from the newest year: ",dupfiles)
      else
        ref_las_files <- ref_las_files[!ref_las_files %in% dropped_files]
    }
  }
  rm(fi, dupfiles, fileyears, maxyear, dropped_files)
}
# Check if duplicate removal successful
if (anyDuplicated(basename(ref_las_files)))
  stop("Duplicate LAS-files: ",ref_las_files[duplicated(basename(ref_las_files))])

message("Referenssimittausten alueiden LAS-tiedostot listattu")
message("Muodostetaan referenssimittausten LAS-katalogi...")
ref_ktlg <- lidR::readLAScatalog(folder = ref_las_files,
# Pudotetaan jo alusta luokat "overlap","isolated", "low point", "air points",
# "fault points", "high vegetation", jotta kevennetään datakuormaa
# Lisätietoa https://www.maanmittauslaitos.fi/kartat-ja-paikkatieto/asiantuntevalle-kayttajalle/tuotekuvaukset/laser-scanning-data-5-p 
                                 filter = "-drop_class 12 16 7 15 17 5 -drop_overlap",
                                 progress = F) 
# Catalog examined at separated locations, not one unified area.
lidR::opt_independent_files(ref_ktlg) <- TRUE
message("Referenssimittausten katalogi muodostettu")
rm(ref_las_files, ref_map_shts)


# Create all evaluated types of DTMs and read the elevations

# Some interpolation errors occur, given as warnings. Should not affect results.
# Mostly caused by open water in plot area, which have no points. (Manually checked)
fut_create_DTMs(ref_ktlg, ref_GPS$lon, ref_GPS$lat,
                ids = save_ids,
                folder = ref_dtm_dir,
                margin = USERCONF$loc_dtm_margin,
                clean = USERCONF$luo_kaikki_5p_dtm,
                reso = USERCONF$DEV$dtm_luonti_solukoko)
message("DTMs created for reference points for DTM performance evaluation")
# Original single-threaded function (slower). Simultaneously read DTM elevations
# dtm_elev <- create_DTMs(ref_ktlg, ref_GPS$lon, ref_GPS$lat,
#                         margin = USERCONF$loc_dtm_margin, ids = save_ids,
#                         folder=ref_dtm_dir, clean = USERCONF$luo_kaikki_5p_dtm,
#                         reso = USERCONF$DEV$dtm_luonti_solukoko,
#                         elev_only = !USERCONF$ajo_las_alustalla)
} # End section performed only on LAS platform

# Read elevations of the created DTMs (parallel)
future::plan("multisession", workers = future::availableCores())
las_avg_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"las_avg"), save_ids,
                              ref_GPS$lon, ref_GPS$lat, c_name = "las_avg_elev") %seed% T
tin_elev     %<-% read_DTM_elev(file.path(ref_dtm_dir,"tin"), save_ids,
                              ref_GPS$lon, ref_GPS$lat, c_name = "tin_elev") %seed% T
krig15_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig15"), save_ids,
                              ref_GPS$lon, ref_GPS$lat, c_name = "krig15_elev") %seed% T
krig30_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig30"), save_ids,
                                ref_GPS$lon, ref_GPS$lat, c_name = "krig30_elev") %seed% T
krig50_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig50"), save_ids,
                              ref_GPS$lon, ref_GPS$lat, c_name = "krig50_elev") %seed% T
krig100_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig100"), save_ids,
                              ref_GPS$lon, ref_GPS$lat, c_name = "krig100_elev") %seed% T
knnidw15_2_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw15_2"), save_ids,
                        ref_GPS$lon, ref_GPS$lat, c_name = "knnidw15_2_elev") %seed% T
knnidw15_3_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw15_3"), save_ids,
                        ref_GPS$lon, ref_GPS$lat, c_name = "knnidw15_3_elev") %seed% T
knnidw50_2_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw50_2"), save_ids,
                        ref_GPS$lon, ref_GPS$lat, c_name = "knnidw50_2_elev") %seed% T
knnidw50_3_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw50_3"), save_ids,
                        ref_GPS$lon, ref_GPS$lat, c_name = "knnidw50_3_elev") %seed% T

dtm_elev <- merge(las_avg_elev, tin_elev, by = "id") |>
  merge(krig15_elev, by = "id") |>
  merge(krig30_elev, by = "id") |>
  merge(krig50_elev, by = "id") |>
  merge(krig100_elev, by = "id") |>
  merge(knnidw15_2_elev, by = "id") |>
  merge(knnidw15_3_elev, by = "id") |>
  merge(knnidw50_2_elev, by = "id") |>
  merge(knnidw50_3_elev, by = "id")
future::plan("sequential")
rm(las_avg_elev, tin_elev, krig15_elev, krig30_elev, krig50_elev, krig100_elev,
   knnidw15_2_elev, knnidw15_3_elev, knnidw50_2_elev, knnidw50_3_elev) # Clean
message("Reference point DTM elevations read")

# Transform IDs to be only paikka_ids
dtm_elev[,"id"] <- gsub("^.*_", "", dtm_elev[,"id"])
dtm_elev <- dplyr::rename(dtm_elev, paikka_id = id)
# Join to main df
ref_GPS <- merge(ref_GPS, dtm_elev, by = "paikka_id")
rm(dtm_elev, save_ids, ref_dtm_dir)

# Calculate differences in DTM reported elevations to the GPS measurement
ref_GPS[["km2_error"]]     <- ref_GPS$Elev - ref_GPS$km2_elev
ref_GPS[["las_error"]]     <- ref_GPS$Elev - ref_GPS$las_avg_elev
ref_GPS[["tin_error"]]     <- ref_GPS$Elev - ref_GPS$tin_elev
ref_GPS[["krig15_error"]]  <- ref_GPS$Elev - ref_GPS$krig15_elev
ref_GPS[["krig30_error"]]  <- ref_GPS$Elev - ref_GPS$krig15_elev
ref_GPS[["krig50_error"]]  <- ref_GPS$Elev - ref_GPS$krig50_elev
ref_GPS[["krig100_error"]] <- ref_GPS$Elev - ref_GPS$krig100_elev
ref_GPS[["knnidw15_2_error"]] <- ref_GPS$Elev - ref_GPS$knnidw15_2_elev
ref_GPS[["knnidw15_3_error"]] <- ref_GPS$Elev - ref_GPS$knnidw15_3_elev
ref_GPS[["knnidw50_2_error"]] <- ref_GPS$Elev - ref_GPS$knnidw50_2_elev
ref_GPS[["knnidw50_3_error"]] <- ref_GPS$Elev - ref_GPS$knnidw50_3_elev



# Get goodness-of-fit indicators on errors of all DTMs
#(How performance progresses when only more and more accurate ref. points are left)

dtm_MAE_prog <- data.frame() # Init result dfs
dtm_RMSE_prog <- data.frame()
dtm_maxerror_prog <- data.frame()

# Points with any NA elevations must be dropped, as it could skew model
# performance if some location is easier or harder for accuracy
ref_GPS <- na.omit(ref_GPS)

for (lim in sort(unique(ref_GPS$Vt_Prec), decreasing = T)) {
  # Drop all points with vt. prec above limit
  refgps2 <- ref_GPS[ref_GPS$Vt_Prec <= lim, ]
  count <- length(refgps2$paikka_id) # How many points are remaining
  
  # Calculate how MAE changes
  dtm_MAE <- lapply(refgps2[,grepl("_error$", names(refgps2))],DescTools::MAE,ref=0)
  newrow <- cbind(lim, count, do.call(cbind, dtm_MAE))
  dtm_MAE_prog <- rbind(dtm_MAE_prog, newrow)
  # Calculate how RMSE changes
  dtm_RMSE <- lapply(refgps2[,grepl("_error$", names(refgps2))],DescTools::RMSE,ref=0)
  newrow <- cbind(lim, count, do.call(cbind, dtm_RMSE))
  dtm_RMSE_prog <- rbind(dtm_RMSE_prog, newrow)
  # Calculate how maximum absolute error changes
  dtm_maxerror <- lapply(refgps2[, grepl("_error$",names(refgps2))], function(x)max(abs(x)))
  newrow <- cbind(lim, count, do.call(cbind, dtm_maxerror))
  dtm_maxerror_prog <- rbind(dtm_maxerror_prog, newrow)
}
rm(lim, refgps2, newrow, dtm_MAE, dtm_RMSE, dtm_maxerror)



#####################======== PLOTTING ========#################################
### Plot the progressions of model performance
# Define labels for legend annotation
labs <- c(
  km2_error = "KM2", las_error = "LAS avg.", tin_error = "TIN",
  krig15_error = "Kriging 15", krig30_error = "Kriging 30",
  krig50_error = "Kriging 50", krig100_error = "Kriging 100",
  knnidw15_2_error = "KNN IDW 15.2",knnidw15_3_error = "KNN IDW 15.3",
  knnidw50_2_error = "KNN IDW 50.2",knnidw50_3_error = "KNN IDW 50.3")

# MAE
MAE_plotdata <- tidyr::pivot_longer(
  data = dtm_MAE_prog,
  tidyr::ends_with("_error"), # Select only error cols to be pivoted
  names_to = "korkeusmalli", values_to = "error")
MAE_plot <- ggplot(data = MAE_plotdata,
                   aes(x = lim, y = error, color = korkeusmalli)) +
  geom_line() +
  # Add count on how many points the values are based on
  geom_vline(xintercept = MAE_plotdata$lim, alpha = 0.3, linetype = 2) +
  ggrepel::geom_label_repel(data = dtm_maxerror_prog,
                               aes(x = lim, y = km2_error,
                                   label = paste("n =", count)),
                               nudge_x = -0.02, nudge_y = 0.35,
                               fontface = "bold", alpha = 0.4,
                               inherit.aes = F, max.time = 2) +
  # Set visual scale to log10
  scale_x_log10() + scale_y_log10() +
  # Adjust legend and visuals
  labs(y = "MAE [m]",
       x = "Upper vt. precision limit of included ref. points [m]",
       color = "Elevation model") +
  scale_color_hue(labels = labs) + theme_classic() +
  theme(legend.position = c(0.55, 0.1), legend.background = element_blank(),
        legend.direction = "horizontal")
# RMSE
RMSE_plotdata <- tidyr::pivot_longer(
  data = dtm_RMSE_prog,
  tidyr::ends_with("_error"),
  names_to = "korkeusmalli", values_to = "error")
RMSE_plot <- ggplot(data = RMSE_plotdata,
                    aes(x = lim, y = error, color = korkeusmalli)) +
  geom_line() +
  # Add count on how many points the values are based on
  geom_vline(xintercept = RMSE_plotdata$lim, alpha = 0.3, linetype = 2) +
  ggrepel::geom_label_repel(data = dtm_maxerror_prog,
                              aes(x = lim, y = km2_error,
                                  label = paste("n =", count)),
                              nudge_x = -0.02, nudge_y = 0.35,
                              fontface = "bold", alpha = 0.4,
                              inherit.aes = F, max.time = 2) +
  # Set visual scale to log10
  scale_x_log10() + scale_y_log10() +
  labs(y = "RMSE [m]",
       x = "Upper vt. precision limit of included ref. points [m]",
       color = "Elevation model") +
  scale_color_hue(labels = labs) + theme_classic() +
  theme(legend.position = c(0.55, 0.1), legend.background = element_blank(),
        legend.direction = "horizontal")
# Maximum error
maxerror_plotdata <- tidyr::pivot_longer(
  data = dtm_maxerror_prog,
  tidyr::ends_with("_error"),
  names_to = "korkeusmalli", values_to = "error")
maxerror_plot <- ggplot(data = maxerror_plotdata,
                        aes(x = lim, y = error, color = korkeusmalli)) +
  geom_line() +
  # Add count on how many points the values are based on
  geom_vline(xintercept = maxerror_plotdata$lim, alpha = 0.3, linetype = 2) +
  ggrepel::geom_label_repel(data = dtm_maxerror_prog,
                             aes(x = lim, y = km2_error + 0.25,
                                 label = paste("n =", count)),
                             nudge_x = -0.02, nudge_y = 0.5,
                             fontface = "bold", alpha = 0.4,
                             inherit.aes = F, max.time = 2) +
  # Set visual scale to log10
  scale_x_log10() + scale_y_log10() +
  # Adjust legend and visuals
  labs(y = "Maximum absolute error [m]",
       x = "Upper vt. precision limit of included ref. points [m]",
       color = "Elevation model") +
  scale_color_hue(labels = labs) + theme_classic() +
  theme(legend.position = c(0.55, 0.1), legend.background = element_blank(),
        legend.direction = "horizontal")
rm(MAE_plotdata, RMSE_plotdata, maxerror_plotdata)

# Save plots on disk (png and rds data file)
save2(MAE_plot, file.path(D$tulokset,"vertaa_dtm","MAE_progression"))
save2(RMSE_plot, file.path(D$tulokset,"vertaa_dtm","RMSE_progression"))
save2(maxerror_plot, file.path(D$tulokset,"vertaa_dtm","maxerror_progression"))
rm(MAE_plot, RMSE_plot, maxerror_plot)


# Plot only with points of acceptable accuracy
ok_refpoints <- na.omit(ref_GPS[ref_GPS$Vt_Prec < 0.09, ])
plotdata <- tidyr::pivot_longer(
  data = ok_refpoints,
  tidyr::ends_with("_error"),
  names_to = "korkeusmalli", values_to = "error")

okpoints_pntplot <- ggplot(data = plotdata, aes(y = error, x = Vt_Prec)) +
  geom_point(aes(col = korkeusmalli)) +
  geom_smooth(aes(col = korkeusmalli), method = "lm", se = F) +
  labs(y = "Error [m]", x = "Vertical precision of the reference point [m]") +
  scale_fill_hue(labels = labs) + theme_classic()

okpoints_densplot <- ggplot(
  data = plotdata,
  aes(x = error, y = korkeusmalli, fill = korkeusmalli)) +
  geom_density_ridges(stat = "binline", bins = 50,
                      scale = 0.9) +
  geom_density_ridges( # Include comments to add carpetplot
    # jittered_points=T,
    # position = position_points_jitter(width=0.01,height=0),
    # point_shape="|", point_size=3, point_alpha=1,
                      alpha = 0.5) +
  geom_vline(xintercept = 0, linewidth = 1.1) +
  labs(x = "DTM error to reference points [m]", y = "Elevation model") +
  scale_y_discrete(expand=expand_scale(mult = c(0.05,0.2)), labels=labs) +
  theme_ridges() +
  theme(legend.position="none", plot.background=element_rect(fill="white"))

okpoints_violplot <- ggplot(data = plotdata,
                            aes(x=korkeusmalli, y=error)) +
  geom_violin(aes(fill=korkeusmalli), adjust=0.9) +
  geom_point(aes(size = 1/(Vt_Prec^2)), alpha=0.32) +
  scale_size_area(name = "Point weight", max_size=6.1) +
  stat_summary(aes(shape="95% CI", color = "95% CI"),
               fun.data=function(x) {
                 DescTools::MedianCI(x,sides="two",method="exact") |>
                   broom::tidy() |>
                   tidyr::pivot_wider(names_from=names, values_from=x) |>
                   dplyr::rename(ymin=lwr.ci, y=median, ymax=upr.ci)},
               size = 0.6,linewidth=1.1, colour = "red2") +
  scale_shape_discrete("Median") + 
  coord_trans(y = scales::pseudo_log_trans(base = 2, sigma = 0.025)) +
  geom_hline(yintercept = 0, linewidth = 0.9) +
  labs(y="Error [m]",x=NULL, shape=NULL) +
  scale_fill_discrete(labels=labs,"Elevation model") + 
  scale_x_discrete(labels = labs, guide = guide_axis(angle = 90)) +
  guides(fill = "none", size = guide_legend(override.aes=list(linetype=0)),
         shape = guide_legend(order=1)) +
  theme_classic()

save2(okpoints_pntplot, path=f.path(D$tulokset,"vertaa_dtm","okpoints_error_v_prec"))
save2(okpoints_densplot, path=f.path(D$tulokset,"vertaa_dtm","okpoints_error_freq"))
save2(okpoints_violplot, path=f.path(D$tulokset,"vertaa_dtm","okpoints_violplot"))
rm(okpoints_pntplot, okpoints_densplot, okpoints_violplot) # Clean plot objs


# Test for normality to justify using median
# 1st LillieTest
model_error_normality <- lapply(
  ok_refpoints[, grepl("_error$", names(ok_refpoints))],
  FUN = function(x) DescTools::LillieTest(x) |> generics::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli")
# Join also results of CramerVonMises normality test
model_error_normality <- rbind(
  model_error_normality,
  lapply(
    ok_refpoints[, grepl("_error$",names(ok_refpoints))],
    FUN = function(x) DescTools::CramerVonMisesTest(x) |> generics::tidy(x)) |>
    dplyr::bind_rows(.id = "korkeusmalli"))
# Print which distributions are normal (p > 0.05)
dplyr::filter(model_error_normality, p.value > 0.05)


# Show indicators for this accepted group of points
# MAE
c_mae <- lapply(ok_refpoints[, grepl("_error$", names(ok_refpoints))],
                function(x) DescTools::MAE(x, ref = DescTools::Quantile(
                  x, probs=.5, weights = 1/(ok_refpoints$Vt_Prec^2))) |> broom::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(x)
# RMSE
c_rmse <- lapply(ok_refpoints[, grepl("_error$", names(ok_refpoints))],
                 function(x) DescTools::RMSE(x, ref = DescTools::Quantile(
                   x, probs=.5, weights = 1/(ok_refpoints$Vt_Prec^2))) |> broom::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(x)
# Maximum error
c_maxerr <- lapply(ok_refpoints[, grepl("_error$", names(ok_refpoints))],
                   function(x) max(abs(x)) |> broom::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(abs(x))
# Median error
c_median <- lapply(ok_refpoints[, grepl("_error$", names(ok_refpoints))],
                   function(x) median(x) |> broom::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(abs(x))
# Weighted median error
c_wgt_median <- lapply(ok_refpoints[, grepl("_error$", names(ok_refpoints))],
                   function(x) {DescTools::Quantile(
                     x, probs=.5, weights = 1/(ok_refpoints$Vt_Prec^2)) |>
                       broom::tidy(x)}) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(abs(x))
# Weighted MAD of error
c_wgt_mad <- lapply(ok_refpoints[, grepl("_error$",names(ok_refpoints))],
                function(x)DescTools::MAD(
                  x, weights = 1/(ok_refpoints$Vt_Prec^2)) |>
                  broom::tidy(x)) |>
  dplyr::bind_rows(.id = "korkeusmalli") |> dplyr::arrange(x)

# (Not ultimately included) Normalized Median Absolute Deviation (NMAD):
# The NMAD is proportional to the median of the absolute differences between
# errors and the median error. It can be considered as an estimate for the
# standard deviation more resilient to outliers.
# (Höhle & Höhle, 2009; Hoaglin et al., 1983)
NMAD <- function(data) 1/qnorm(0.75) * median(abs( data - median(data) ))
c_wgt_nmad <- lapply(ok_refpoints[, grepl("_error$",names(ok_refpoints))], NMAD) |>
  dplyr::bind_rows(.id = "korkeusmalli") |>
  tidyr::pivot_longer(cols=tidyr::everything(),
                      names_to="korkeusmalli", values_to="x") |> dplyr::arrange(x)

# Collect the sorted results of model comparisons
model_comparison <-
  cbind(c_mae,c_rmse,c_maxerr,c_median,c_wgt_median[,c("korkeusmalli","x")],c_wgt_mad)
# Set names of columns
nms <-
  as.vector(
    t(cbind("korkeusmalli",
            matrix(c("MAE","RMSE","Max err.","Median","Wgt. median","Wgt. MAD"),
                   6, byrow = T))))
names(model_comparison) <- nms
# Set pretty names for models
for (n in unique(model_comparison[,1])) {
  model_comparison <-
    replace(model_comparison, model_comparison==n, labs[[n]])
}
# Save results to csv
data.table::fwrite(model_comparison,
                   f.path(D$tulokset, "vertaa_dtm", "model_comparison.csv"))

rm(nms, c_mae, c_rmse, c_maxerr,c_median, c_wgt_median, c_wgt_mad)
# Krig50 is selected as the best
# (many top options very close, Krig50 best with least spread (small MAD))


# Use the weighted median error as a constant correction for all future DTM elevations
krig50shift <- DescTools::Quantile(ok_refpoints$krig50_error, probs=.5,
                                   weights = 1/(ok_refpoints$Vt_Prec^2))
# KM2 not shifted, as the error has larger values and larger SD (too wide distribution)


rm(ref_ktlg, labs)
#rm(plotdata)
