# Lataa 2x2m maankorkeusaineisto kaikilta havaintopaikoilta
# Joudutaan lataamaan aineisto pienissä paloissa, koska osa asemista ylettyy liian
# laajalle alueelle, jotta kaikki putket mahtuisivat suurimmalle sallitulle
# yhtenäiselle aineistolle, max 100km2.


# Prepare and execute TIFF downloads:
if (curl::has_internet()) {
  message("Aloitettu kaikkien paikkojen KM2 latausten valmistelu.")
  lataustyot <- kaikki_paikat[, c("paikka_id","tunnus")]
  # Aloitetaan joka putkelle lataustyö, ja tallennetaan sen URL tauluun
  for (i in 1:nrow(kaikki_paikat)) {
    # Syötetään putkitaulun rivi valmistelufunktioon
    pisteen_vastaus <- valmistele_2m_aineisto(lon = kaikki_paikat[[i, "lon"]],
                                              lat = kaikki_paikat[[i, "lat"]],
                                              marginaali = USERCONF$loc_dtm_margin)
    # Haetaan työn URL valmistelun vastauksesta
    lataustyot[i, "url"] <- pisteen_vastaus$links$href
    # Haetaan työn aloituksen status
    lataustyot[i, "status"] <- pisteen_vastaus$status
    if (i %% 100) message("100:n paikan aineisto valmisteltu palvelimelle")
  }
  rm(i, pisteen_vastaus)
  message("Kaikkien paikkojen KM2 lataukset valmisteltu.")
  
  
  # Odotetaan että lataustyö on valmistunut palvelimella ennen kuin ladataan
  i <- 1 # Ladattavan indeksin alustaminen
  while (i <= nrow(lataustyot)) {
    # Jos työn URL on NA, huomautetaan virheestä
    if (is.na(lataustyot[i, "url"])) {
      stop(paste("Havaintopaikan KM2 lataus ei onnistunut",lataustyot[i,]))
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
        tallennuspolku <- file.path(km2_dir,
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
          message(
            p("Havaintopaikka",lataustyot[i,"tunnus"],"ohitettu. KM2:n ulkopuolella?"))
          i <- i + 1
        } else {
          # Muuten ladataan tiedosto
          file_url <- vastaus$results$path[1]
          # Lataa tiedosto (pitää olla binarynä)
          download.file(file_url, tallennuspolku, mode="wb", timeout=120, quiet=T)
          
          # Tarkistetaan että löytyyhän tiedosto levyltä
          if (file.exists(file.path(tallennuspolku))) {
            message(paste("-Havaintopaikan",lataustyot[i,"tunnus"],"aineisto ladattu"))
            if (i %% 50 == 0) Sys.sleep(5) # Take breaks to not reach req limit
          } else stop(paste("Havaintopaikan aineistoa ei ladattu:",tallennuspolku))
          
          i <- i + 1 # Siirrytään seuraavaan vasta kun progress=100 ja ladattu.
        }
      }
    }
  }
  message("KM2 elevation model data downloaded for all points")
} else warning("Forced KM2 downloading skipped. No internet!") # End download section

rm(tallennuspolku, i, file_url, vastaus, req)
