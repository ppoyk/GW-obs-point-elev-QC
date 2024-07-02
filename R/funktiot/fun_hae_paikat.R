# Hakee kaikkien havaintopaikkatyyppien tiedot tietokannasta asemilta, joiden ID on annettu listassa. (Pitää ehkä olla data.frame rivi/sarake)

# Jos savepath annettu, tallentaa hakutuloksen tähän sijaintiin

hae_paikat <- function(asema_id_lista = NULL, paikka_id_lista = NULL,
                       savepath = NULL, tstamp = FALSE) {
  
  # Check input (either or)
  if (!xor(is.null(asema_id_lista), is.null(paikka_id_lista))) {
    stop("Anna joko aseman TAI paikan ID(t)!", call. = F) }
  
  # Yhdistetään tietokantaan
  yhteys <- .connect_db(D$secrets)
  on.exit(DBI::dbDisconnect(yhteys))
  
  if (!is.null(asema_id_lista)) { # Check if asema_id supplied
    asema_id_lista <- paste(asema_id_lista, collapse = ",") # Muutetaan kirjain vektoriksi
    # Haetaan ensin vain asemaan linkitettyjen paikkojen id:t.
    haetut_paikat <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#asema_id_1.txt")),#Query from file
          collapse = " "),
        asema_id_lista = asema_id_lista))
    
    # Muutetaan saatujen paikkojen id:t kirjain vektoriksi omaan muuttujaan
    paikka_idt <- paste(haetut_paikat$paikka_id, collapse = ",")
    
    # Haetaan paikkojen ID:n perusteella Paikka-taulun relevantit tiedot
    # ja yhdistetään samaan muuttujaan "paikka_id":n perustella
    haetut_paikat <- merge(
      haetut_paikat,
      DBI::dbGetQuery(
        yhteys,
        stringr::str_glue(
          p(readLines(f.path(D$secrets,"SQL","hae_paikat#asema_id_2.txt")),#Query from file
            collapse = " "),
          paikka_idt = paikka_idt)),
      by = "paikka_id")
    
  # Get paikka_id directly from expected supplied list if asema_id not provided
  } else if (!is.null(paikka_id_lista)) { # Check if paikka_id supplied
    paikka_id_lista <- paste(paikka_id_lista, collapse = ",") # Muutetaan kirjain vektoriksi
    # Haetaan paikkojen ID:n perusteella Paikka-taulun relevantit tiedot
    # ja yhdistetään samaan muuttujaan "paikka_id":n perustella
    haetut_paikat <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#paikka_id.txt")),#Query from file
          collapse = " "),
        paikka_id_lista = paikka_id_lista))
    
    paikka_idt <- paste(haetut_paikat$paikka_id, collapse = ",")
    
  } else stop("Anna joko aseman TAI paikan ID(t)!",call.=F) # 2ndary input check
  
  
  # Muutetaan PaikkaTyyppi factor-muuttujaksi.
  # Id<->Selite tiedot tietokannasta
  PaikkaTyyppi <- DBI::dbGetQuery(
    yhteys,
    p(readLines(f.path(D$secrets,"SQL","hae_paikat#PaikkaTyyppi.txt")),#Query from file
      collapse = " "))
  haetut_paikat$PaikkaTyyppi_Id <- factor(
                    haetut_paikat$PaikkaTyyppi_Id,
                    levels = trimws(PaikkaTyyppi$PaikkaTyyppi_Id),
                    labels = trimws(PaikkaTyyppi$Selite))
  
  # Eri paikkatyyppien tiedot pitää hakea erikseen omista tauluistaan, ja
  # liittää osaksi palautettavaa taulua siten että tyyppien keskenään jakamat
  # tiedot (esim maanpinnan korkeus) menevät samaan sarakkeeseen.
  
  # Suoritetaan sql-haut vain jos tiettyä tyypppiä esiintyy.
  
  #################### Haetaan putkien tiedot
  #################### 
  if (any(haetut_paikat$PaikkaTyyppi_Id == "Havaintoputki")) {
    # Valitaan vain putkien ID:t haetuista paikoista, koska vain ne ovat PaikkaPutki-taulussa
    putki_idt <- haetut_paikat[haetut_paikat$PaikkaTyyppi_Id == "Havaintoputki", "paikka_id"]
    putki_idt <- paste(putki_idt, collapse = ",") # Kirjainjonoksi
    paikka_putket <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#Havaintoputki.txt")),#Query from file
          collapse = " "),
        putki_idt = putki_idt))
    
    # Yhdistetään putkien tiedot paikkojen tauluun (uudet sarakkeet)
    # Vain tässä käytetään merge(), myöhemmin muiden tyyppien tiedot halutaan
    # samoihin soveltuviin sarakkeisiin, joten käytetään dplyr::rows_patch()
    # Uudet paikkatyypille uniikit sarakkeet luodaan ja täytetään 
    # kunkin paikkatyypin käsittelyssä
    haetut_paikat <- merge(haetut_paikat, paikka_putket,
                           all.x = TRUE,
                           by = "paikka_id")
  }
  
  #################### Haetaan kaivojen tiedot
  #################### 
  if (any(haetut_paikat$PaikkaTyyppi_Id == "Kaivo")) {
    # Valitaan vain kaivojen ID:t haetuista paikoista, koska vain ne ovat PaikkaKaivo-taulussa
    kaivo_idt <- haetut_paikat[haetut_paikat$PaikkaTyyppi_Id == "Kaivo", "paikka_id"]
    kaivo_idt <- paste(kaivo_idt, collapse = ",")
    paikka_kaivot <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#Kaivo.txt")),#Query from file
          collapse = " "),
        kaivo_idt = kaivo_idt))
    
    # Yhdistetään kaivojen PutkiYla ja KorkeusKansi, sekä
    # PutkiAla ja KorkeusPohja sarakkeet
    # (tietokannassa näyttää olevan lähes kopioita)
    # Priorisoidaan Korkeus* sarakkeita, koska kuvaavat kaivoja paremmin
    # Nimetään sarake myös uudelleen, jotta sopii kaikkien paikkojen listaan
    paikka_kaivot$Yla <- ifelse(is.na(paikka_kaivot$KorkeusKansi),
                                paikka_kaivot$PutkiYla,
                                paikka_kaivot$KorkeusKansi)
    paikka_kaivot$Ala <- ifelse(is.na(paikka_kaivot$KorkeusPohja),
                                paikka_kaivot$PutkiAla,
                                paikka_kaivot$KorkeusPohja)
    # Jätetään vain yhdistetyt sarakkeet, poistetaan yhdistämisessä käytetyt
    paikka_kaivot[ , c("PutkiYla", "KorkeusKansi", "PutkiAla", "KorkeusPohja")] <- NULL
    
    # Yhdistetään kaivojen tiedot paikkojen tauluun
    # Luodaan tyhjät sarakkeet paikkatyypin uniikeille sarakkeille
    for (s in setdiff(names(paikka_kaivot), names(haetut_paikat))) {
      haetut_paikat[[s]] <- NA
    }
    # Siirretään olemassa oleviin paikkatyyppien yhteisiin, sekä paikkatyypin uniikkeihin sarakkeisiin paikkatyypin haetut tiedot
    haetut_paikat <- dplyr::rows_patch(haetut_paikat, paikka_kaivot, by = "paikka_id")
  }
  
  #################### Routaputkien käsittely
  #################### 
  if (any(haetut_paikat$PaikkaTyyppi_Id == "Routaputki")) {
    # Valitaan vain routaputkien ID:t haetuista paikoista
    routa_idt <- haetut_paikat[haetut_paikat$PaikkaTyyppi_Id == "Routaputki", "paikka_id"]
    routa_idt <- paste(routa_idt, collapse = ",")
    paikka_routa <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#Routaputki.txt")),#Query from file
          collapse = " "),
        routa_idt = routa_idt))
    # Liitetään palautettavaan tauluun (ei mukaan otettuja uniikkeja sarakkeita luotavaksi)
    haetut_paikat <- dplyr::rows_patch(haetut_paikat, paikka_routa, by = "paikka_id")
  }
  
  #################### Maankosteusputkien käsittely
  ####################
  if (any(haetut_paikat$PaikkaTyyppi_Id == "Maankosteusmittari")) {
    # Valitaan vain routaputkien ID:t haetuista paikoista
    mkost_idt <- haetut_paikat[haetut_paikat$PaikkaTyyppi_Id=="Maankosteusmittari", "paikka_id"]
    mkost_idt <- paste(mkost_idt, collapse = ",")
    paikka_mkost <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#Maankosteusmittari.txt")),#Query from file
          collapse = " "),
        mkost_idt = mkost_idt))
    # Liitetään palautettavaan tauluun (sarakkeet nimetty yhtsopiviksi hakulauseessa)
    haetut_paikat <- dplyr::rows_patch(haetut_paikat, paikka_mkost, by = "paikka_id")
  }
  
  #################### Lysimetrien käsittely
  ####################
  if (any(haetut_paikat$PaikkaTyyppi_Id == "Lysimetri")) {
    # Valitaan vain routaputkien ID:t haetuista paikoista
    lysi_idt <- haetut_paikat[haetut_paikat$PaikkaTyyppi_Id=="Lysimetri", "paikka_id"]
    lysi_idt <- paste(lysi_idt, collapse = ",")
    paikka_lysi <- DBI::dbGetQuery(
      yhteys,
      stringr::str_glue(
        p(readLines(f.path(D$secrets,"SQL","hae_paikat#Lysimetri.txt")),#Query from file
          collapse = " "),
        lysi_idt = lysi_idt))
    # Calculate the sieve bottom (to concur with other types)
    paikka_lysi[["SiivilaAla"]] <- paikka_lysi$SiivilaYla-paikka_lysi$SiivilaPituus
    paikka_lysi$SiivilaPituus <- NULL
    # Liitetään palautettavaan tauluun (sarakkeet nimetty yhtsopiviksi hakulauseessa)
    haetut_paikat <- dplyr::rows_patch(haetut_paikat, paikka_lysi, by = "paikka_id")
  }
  
  
  # TÄHÄN VOI LISÄTÄ KÄSITTELYT LOPUILLEKIN PAIKKATYYPEILLE
  # Lumikeppien paikoilla ei tyyppikohtaisia tietoja
  # Hanoja ei haluta mukaan tarkasteluun
  
  
  # Korvataan palautettavan taulun kautta-merkit tekstistä viivalla,
  # jotta tallennuspolut eivät sekoa
  for (sarake in names(haetut_paikat)) {
    if (is.character(haetut_paikat[[sarake]])) {
      haetut_paikat[[sarake]] <- sub("/", "-", haetut_paikat[[sarake]])
      # Trimmataan turhat välilyönnit tms. joita on taulussa.
      # Varoitus! trimws() Muuttaa luvut "tekstiksi" jos ajetaan lukusarakkeeseen
      haetut_paikat[[sarake]] <- trimws(haetut_paikat[[sarake]])
    }
  }
  
  # Järjestetään aseman id:n ja paikan nimen mukaan
  haetut_paikat <- haetut_paikat[with(haetut_paikat, order(asema_id, tunnus)),]

  # Tallennetaan levylle jos halutaan
  if (!is.null(savepath)) {
    data.table::fwrite(haetut_paikat,
                       file = file.path(savepath),
                       na = NA, bom = T)
    if (file.exists(savepath)) {
      message("Haetut paikkojen tiedot tallennettu:", savepath)
    } else stop("Paikkojen tietojen tallennusyritys epäonnistui:", savepath, call. = F)
  }
    
  # Dokumentoidaan myös tieto, milloin aineisto ladattiin tietokannasta
  if (tstamp == TRUE) {
  writeLines(
    c("Tämä tiedosto päivitetään automaattisesti, kun havaintopaikkojen tiedot tallennetaan tietokannasta levylle", "",
      paste("Analyysissä käytetyt havaintopaikkojen tiedot ladattu Ympäristöhallinnon pohjavesitietokannasta:",Sys.time()),"",
      paste("Hakulauseen muotoutuminen dokumentoitu funktiossa",
            gsub(root,"",this.path::this.path()))),
    con = file.path(D$doc, "havaintopaikkojen_tietokantahaun_tiedot.txt"))
  }
  

  
  return(haetut_paikat)
  
}
  
