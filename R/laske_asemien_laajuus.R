# Tarkastellaan havaintoaseman putkien etäisyyksiä aseman ilmoitetuista koordinaateista

# "laajuus" ilmoittaa aseman koon säteen, eli neliötä rajattaessa puolet sivun pituudesta
# (Tätä aseman laajuutta käytetään myöhemmin aineiston latauksen rajaamiseen)

# Laajuudessa överitulokset (n. yli 10km) ovat virheellisiä putken linkityksiä
# VOIDAAN MYÖHEMMIN MUUTTAA OTTAMAAN HUOMIOON KAIKKI PAIKAT, EI VAIN PUTKIA

# Funktio, joka laskee kauimmaisen putken etäisyyden aseman koordinaateista
aseman_maxdist <- function(as_id) {
  
  # Ottaa aseman rivin asemataulusta
  asema <- asemataulu[asemataulu$asema_id == as_id, ]
  
  # Hakee asemaan linkitetyt PUTKET ja tarkistetaan onko yhtään
  # (koska halutaan myöhemmin suodattaa pois asemat joissa on vain muita kuin putkia)
  putket <- kaikki_putket[kaikki_putket$asema_id == as_id, ]
  
  if (length(putket[,1]) == 0) {
    message(paste("Ei havaintoputkia asemalla", asema[["tunnus"]], asema[["nimi"]]))
    max <- NA  # aseman laajuus nollaksi, koska sen avulla alempana pudotetaan asema pois
  } else {
# Jos putkia löytyy, halutaan kuitenkin kaikkien aseman havpaikkojen koord ja 
# etäisyydet ottaa huomioon, jotta aineiston latauksen rajaus sis. nämäkin kohteet
    paikat <- kaikki_paikat[kaikki_paikat$asema_id == as_id, ]
  # Tallennetaan vain suurin matka aseman pisteestä havaintopaikoille
  max <- max(terra::distance(
    as.matrix(asema[c("lon", "lat")]),
    as.matrix(paikat[c("lon", "lat")]),
    lonlat = FALSE))
  return(max)
  }
}

# Ajetaan kauimmaisen putken laskenta kaikille asema ID:ille
cat("Aloitetaan asemien laajuuden laskenta...\n")
laajuus <- sapply(asemataulu$asema_id, aseman_maxdist)
# Toimii myös, mutta todnäk raskaampi
# maxit <- mapply(aseman_maxdist, asemataulu$asema_id)

# Pyöristetään laajuus ja liitetään asematauluun
laajuus <- ceiling(laajuus)
asemataulu[["laajuus"]] <- laajuus

rm(laajuus) # Siivous

# Pudotetaan kaikki asemat joissa voidaan päätellä että yhtään putkea ei ole linkitetty
# (Tällaisten kohteiden laajuus merkittiin NA'ksi)
asemataulu <- asemataulu[complete.cases(asemataulu[["laajuus"]]), ]

# Tarkistetaan, eihän laajuuksissa ole NA arvoja.
if (anyNA(asemataulu[["laajuus"]])) {
  stop("VIRHE: Tyhjiä arvoja asemien laajuuksissa")
} else {
  cat(paste0("=============\n",
             "Asemien laajuudet laskettu ja putkettomat asemat poistettu\n",
             "============="))
}

