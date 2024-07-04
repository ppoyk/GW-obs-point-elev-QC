# Tässä koodissa on tarkoitus käsitellä alustavat Suomen alueen
# koordinaattikiintopisteisiin liittyvät asiat.
# 
# - kiintopisteiden korkeudet pitää kääntää manuaalisesti N2000-tasoon
# Paikkatietoikkunan koordinaattimuunnospalvelun kautta, koska täydet korkeudet
# vain EUREF-FIN-GRS80h (EPSG:4937) -muodossa, TODO
# - kiintopisteiden korkeuksiin korjataan vielä ilmoitettu ero maanpintaan, TODO
# - pisteiden alueilta ladataan KM2 korkeustiedot,
# - laserkatalogista rajataan pisteiden alueiden tiedot, TODO
# - laser rajauksista muodostetaan DTM:t, TODO
# - molemmista aineistoista luetaan korkeudet, ja niitä verrataan kiintopisteiden
# ilmoitettuihin korkeuksiin
# kiintopisteiden korkeudet pitää kääntää manuaalisesti Paikkatietoikkunan
# koordinaattimuunnospalvelun kautta, koska täydet korkeudet vain
# EUREF-FIN-GRS80h (EPSG:4937) -muodossa)
# 
################################################################################
kpcheck <- kp[kp$N2000_tarkkuusluokka %in% c("6N01"), ]
# Check that at least 1 N2000 elevation available
kpcheck <- kpcheck[
  !(is.na(kpcheck$N2000_korkeus) &
      is.na(kpcheck$N2000_korkeus_muunnettu_N60_korkeudesta)), ]
# Select only points in bedrock, not on structure or building
kpcheck <- kpcheck[kpcheck$alusta %in% c("3KAL"), ]
# Select only minimal point markers located on ground level
kpcheck <- kpcheck[kpcheck$keskusmerkki %in%
                     c("2PUL","2PPU","2TAP","2PUT","2ALN","2RUU","2NAU","2YMP"), ]
# Filter to points which likely are on the ground surface, not elevated or recessed
kpcheck <- dplyr::filter(kpcheck, 
                         korkeus_maanpinnasta_m == 0 | is.na(korkeus_maanpinnasta_m))

# Location data in the correct "precise" fields very incomplete.
# Must use coarser location fields (0.1m representation)
# Points where both of these coordinates fall on exact meter are filtered, as 
# possibly too coarse stored location for our purposes.
kpcheck <- kpcheck[
  as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_E) !=
    round(as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_E)) &
    as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_N) !=
    round(as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_N)), ]
# Same filtering based on the old KKJ coords. (Some points are old and likely have
# only automatic conversion to ETRS-TM35FIN done to them for the locating coords
kpcheck <- kpcheck[
  as.numeric(kpcheck$KKJ_paikannus_E) !=
    round(as.numeric(kpcheck$KKJ_paikannus_E)) &
    as.numeric(kpcheck$KKJ_paikannus_N) !=
    round(as.numeric(kpcheck$KKJ_paikannus_N)), ]

# Get reference point location, either true or coarser locating coordinates
kpcheck[["lon"]] <- ifelse(!is.na(kpcheck$ETRS_TM35FIN_E),
                           as.numeric(kpcheck$ETRS_TM35FIN_E),
                           as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_E))
kpcheck[["lat"]] <- ifelse(!is.na(kpcheck$ETRS_TM35FIN_N),
                           as.numeric(kpcheck$ETRS_TM35FIN_N),
                           as.numeric(kpcheck$ETRS_TM35FIN_paikannuskoordinaatti_N))
kpcheck <- sf::st_drop_geometry(kpcheck) # Drop unnecessary geometry

################################################################################
# Ladataan KM2:n tiedot MML:n palvelimelta kiintopisteiden alueilta



# Tarkistetaan tallennuskansio
kp_km2_dir <- check_dir(D$data,"kiintopisteet","KM2", ensure = T)

# Download KM2 elevation model rasters only if access to internet
if (curl::has_internet()) {
  # Alustetaan lista latausten osoitteille
  latauslista <- kpcheck["kiintopistetunnus"]
  
  # Käydään läpi kaikki rajatun luokan kiintopisteet
  for (i in 1:nrow(kpcheck)) {
    
    # Hyödynnetään aikaisempaa aineiston valmistelufunktiota
    vastaus <- valmistele_2m_aineisto(
      lon = as.numeric(kpcheck[[i,"lon"]]),
      lat = as.numeric(kpcheck[[i,"lat"]]),
      marginaali = USERCONF$loc_dtm_margin)
    # Tallennetaan lataus-URL
    latauslista[i, "url"] <- vastaus$links$href
  }
  
  # Odotetaan että lataustyö on valmistunut palvelimella ennen kuin ladataan
  i <- 1 # Ladattavan indeksin alustaminen
  while (i <= nrow(latauslista)) {
    # Jos työn URL on NA, huomautetaan virheestä
    if (is.na(latauslista[i, "url"])) {
      stop(paste("Kiintopisteen lataustyö ei onnistunut",latauslista[i,]))
    } else { # Muuten jatketaan normaalisti aineiston lataukseen
      
      # Haetaan työn ajantasainen tieto palvelimelta
      req <- httr2::request(latauslista[i, "url"])
      req <- httr2::req_url_query(req,
                                  `api-key` = readLines(file.path(D$secrets, "MML_avain.txt")))
      vastaus <- httr2::req_perform(req)
      vastaus <- jsonlite::fromJSON(rawToChar(vastaus$body))
      
      # Tarkistetaan onko työ epäonnistunut palvelimella
      if (vastaus$status %in% c("failed", "dismissed")) {
        stop(paste("VIRHE: Palvelimen vastaus:", vastaus$message))
      }
      
      # Jos työ ei ole valmis palvelimella, odotetaan 2 s
      if (vastaus$progress != 100 || vastaus$status %in% c("running", "accepted")) {
        message(paste("Lataustyön valmistelu palvelimella kesken..."))
        message(paste0("Progress ", vastaus$progress, "/100"))
        Sys.sleep(2)
      } else {
        # Ladataan yhden kiintopisteen alue
        tallennuspolku <- file.path(kp_km2_dir, 
                                    paste0(latauslista[[i,"kiintopistetunnus"]],"_.tif"))
        
        # Latauslinkkiin päästään seuraamalla aikaisempaa vastausta
        req <- httr2::request(vastaus$links$href)
        req <- httr2::req_url_query(req,
                                    `api-key` = readLines(file.path(D$secrets, "MML_avain.txt")))
        vastaus <- httr2::req_perform(req)
        vastaus <- jsonlite::fromJSON(rawToChar(vastaus$body))
        
        # Jos ladattavan tiedoston koko on 0, siirrytään seuraavaan kp:seen.
        if (vastaus$results$length[1] == 0) {
          warning(paste("Kiintopiste",latauslista[i,"kiintopistetunnus"],"ohitettu. KM2 ulkopuolella"))
          i <- i + 1
        } else {
          # Muuten ladataan tiedosto
          file_url <- vastaus$results$path[1]
          # Lataa tiedosto (pitää olla binarynä)
          download.file(file_url, tallennuspolku, mode = "wb", timeout = 120)
          
          # Tarkistetaan että löytyyhän tiedosto levyltä
          if (file.exists(file.path(tallennuspolku))) {
            message("Kiintopisteen",latauslista[i,"kiintopistetunnus"],"aineisto ladattu")
          } else stop(paste("VIRHE: Kiintopisteen aineistoa  ei ladattu:",tallennuspolku))
          
          i <- i + 1 # Siirrytään seuraavaan indeksiin vasta kun progress = 100.
        }
      }
    }
  }
  
  #rm(latauslista, file_url, tallennuspolku, vastaus, req)
}

# Read elevations from KM2 data
kpcheck[["km2_elev"]] <- NA # Prepare col
km2_elev <- read_KM2_elev(kp_km2_dir,
                          kpcheck$kiintopistetunnus,
                          kpcheck$lon, kpcheck$lat)
kpcheck[["km2_elev"]] <- km2_elev
rm(km2_elev, kp_km2_dir) # Siivous



if (USERCONF$ajo_las_alustalla == T) {
  kp_las_files <- shell(paste0(
    'find -L ',D$las,
    ' -regex ".*\\(',paste0(unique(kpcheck$UTM_karttalehti), collapse = "\\|"), 
    '\\).*\\.laz$"', # Files w/ prod area in path, and ending in .laz
    ' -type f'), # Files only
    intern = T)
  
  # Drop duplicated files of the same location. Leave only on in newest year folder  
  if(anyDuplicated(basename(kp_las_files))) {
    for (fi in unique(basename(kp_las_files))) {
      if (sum(grepl(fi, kp_las_files)) > 1) { # If file occurs more than once
        dupfiles <- kp_las_files[grepl(fi, kp_las_files)]
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
          kp_las_files <- kp_las_files[!kp_las_files %in% dropped_files]
      }
    }
  }
  rm(fi, dupfiles,fileyears,maxyear,dropped_files)
  
  kp_ktlg <- lidR::readLAScatalog(folder = kp_las_files,
                                  filter = "-drop_class 12 16 7 15 17 5 -drop_overlap",
                                  progress = F) 
  lidR::opt_independent_files(kp_ktlg) <- TRUE
  message("Kiintopisteiden katalogi muodostettu")
  rm(kp_las_files)
  
  
  
  # Check/create folders for DTM storage / loading
  dtm_trunkpath <- check_dir(D$data,"kiintopisteet","kp_LAS_DTM", ensure = T)
  
  fut_create_DTMs(kp_ktlg, kpcheck$lon, kpcheck$lat,
                  ids = kpcheck$kiintopistetunnus,
                  folder = dtm_trunkpath,
                  margin = USERCONF$loc_dtm_margin,
                  clean = USERCONF$luo_kaikki_5p_dtm,
                  reso = USERCONF$DEV$dtm_luonti_solukoko)
  
  future::plan("multisession", workers = future::availableCores())
  kp_las_avg_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"las_avg"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "las_avg_elev") %seed% T
  kp_tin_elev     %<-% read_DTM_elev(file.path(ref_dtm_dir,"tin"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "tin_elev") %seed% T
  kp_krig15_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig15"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "krig15_elev") %seed% T
  kp_krig30_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig30"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "krig30_elev") %seed% T
  kp_krig50_elev  %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig50"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "krig50_elev") %seed% T
  kp_krig100_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"krig100"), save_ids,
                                  ref_GPS$lon, ref_GPS$lat, c_name = "krig100_elev") %seed% T
  kp_knnidw15_2_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw15_2"), save_ids,
                                     ref_GPS$lon, ref_GPS$lat, c_name = "knnidw15_2_elev") %seed% T
  kp_knnidw15_3_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw15_3"), save_ids,
                                     ref_GPS$lon, ref_GPS$lat, c_name = "knnidw15_3_elev") %seed% T
  kp_knnidw50_2_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw50_2"), save_ids,
                                     ref_GPS$lon, ref_GPS$lat, c_name = "knnidw50_2_elev") %seed% T
  kp_knnidw50_3_elev %<-% read_DTM_elev(file.path(ref_dtm_dir,"knnidw50_3"), save_ids,
                                     ref_GPS$lon, ref_GPS$lat, c_name = "knnidw50_3_elev") %seed% T
  
  
kp_dtm_elev <- Reduce(
  function(x,y) merge(x, y, by="id"),
  list(kp_las_avg_elev, kp_tin_elev, kp_krig15_elev, kp_krig30_elev, kp_krig50_elev,
       kp_krig100_elev, kp_knnidw15_2_elev, kp_knnidw15_3_elev, kp_knnidw50_2_elev, kp_knnidw50_3_elev))
  rm(kp_las_avg_elev, kp_tin_elev, kp_krig15_elev, kp_krig30_elev, kp_krig50_elev,
     kp_krig100_elev, kp_knnidw15_2_elev, kp_knnidw15_3_elev, kp_knnidw50_2_elev, kp_knnidw50_3_elev)
  rm(dtm_trunkpath, dtm_dir, dtm_filepaths, kp_ktlg)
  
  
  # Calculate differences in DTM reported elevations to the GPS measurement
  kpcheck[["km2_error"]]     <- kpcheck$Elev - kpcheck$km2_elev
  kpcheck[["las_error"]]     <- kpcheck$Elev - kpcheck$las_elev
  kpcheck[["tin_error"]]     <- kpcheck$Elev - kpcheck$tin_elev
  kpcheck[["krig10_error"]]  <- kpcheck$Elev - kpcheck$krig10_elev
  kpcheck[["krig50_error"]]  <- kpcheck$Elev - kpcheck$krig50_elev
  kpcheck[["krig100_error"]] <- kpcheck$Elev - kpcheck$krig100_elev
  
}
