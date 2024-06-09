# Linkitetään tallennetut 5p-aineistosta erotetut klipit varalta paikka_id:isiin

# (Klippien tallennusnimiä voi hallita vain rajallisesti, joten niille on
# muodostunut uudet idt, jotka ovat riippumattomia muista tiedoista)
# (Linkitys täytyy siis tehdä koordinaattien kautta)
# (Ei täsmällinen, mutta alueiden pitäisi osua kohdalleen)

# Alustetaan taulu kaikille paikka_id:ille yhdistetyllä koordinaattisarakkeella
indeksi_5p <- kaikki_paikat[, c("paikka_id", "lon","lat")]
indeksi_5p[["lonlat"]] <- paste0(indeksi_5p$lon,"_",indeksi_5p$lat)
indeksi_5p[ , c("lon","lat")] <- NULL

# Luetaan tallennettujen 5p-klippien tiedostot, ja muokataan tiedostonimistä
# koordinaatit ja 5p-tallennuksessa luodut ID:t uuteen tauluun sopivassa muodossa
putkittaiset_tied <- list.files(file.path(D$data,"5p_putkittainen"),
                                pattern = ".*\\.las$")
# Tiedostonimen hajotus osiin
putkittaiset_tied <- unlist(strsplit(putkittaiset_tied, "_"))
# Osien muodostaminen tauluksi
putkittaiset_tied <- data.frame(matrix(putkittaiset_tied, ncol = 3, byrow = T))
names(putkittaiset_tied) <- c("lon","lat","id5p")
# Koordinaattien yhdistäminen tekstimuotoon (melkein uniikkeja ja yhdessä sarakkeessa, joten toimii "avaimena")
putkittaiset_tied[["lonlat"]] <- paste0(putkittaiset_tied$lon,"_", putkittaiset_tied$lat)
putkittaiset_tied[ , c("lon","lat")] <- NULL
# 5p-aineiston tallentamisessa muodostuneiden ID:iden siivous
putkittaiset_tied[["id5p"]] <- gsub("\\.las$","", putkittaiset_tied$id5p)

# Yhdistetään paikka_id:t ja 5p-aineiston luonnin id:t
indeksi_5p <- merge(indeksi_5p, putkittaiset_tied, by = "lonlat")

rm(putkittaiset_tied)  # Siivous
