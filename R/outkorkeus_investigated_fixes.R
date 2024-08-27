# Script to run before the output of new elev values of places.

# Hard-codes corrections to the automatic logic of selecting the most suitable 
# elevation values. Based on manual investigations.
# Required, as the monit.network has too diverse cases to account for with simple rules
# (May break indicated sources)


# Function to set the maa with current pipetop minus the pipe vis.len 
# Option to set the pipe len manually.
set_maa_w_putki <- function(id, putki = NULL) {
  if (is.null(pipe)) {
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa"] <- 
      putki_yhd_temp[putki_yhd_temp$paikka_id == id,"yla"] -
      putki_yhd_temp[putki_yhd_temp$paikka_id == id,"putki"]
  } else {
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa"] <- 
      putki_yhd_temp[putki_yhd_temp$paikka_id == id,"yla"] - putki
  }
  # Attempt to set appropriate sources
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa_source"] <-
    paste0(putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"],"-",
           putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki_source"])
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa_verif"] <- "man. chg."
}

# Function to set the top elev from db. Sources accordingly.
set_yla_from_db <- function(id) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_db"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <- "DB"
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}
# Function to set the top elev from DTM elev + pipe. Sources accordingly.
set_yla_from_dtm <- function(id) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    kaikki_paikat[kaikki_paikat$paikka_id == id, "dtm_elev"] +
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <-
    paste0("DTM+", putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki_source"])
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}
# Function to set the top elev from KM2 elev + pipe. Sources accordingly.
set_yla_from_km2 <- function(id) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    kaikki_paikat[kaikki_paikat$paikka_id == id, "km2_elev"] +
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <-
    paste0("KM2+", putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki_source"])
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}

# Set top elev or ground elev from another pipe. Copy also elev sources.
set_yla_w_id <- function(id, from) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, "yla"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, c("yla_source","yla_verif")] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, c("yla_source","yla_verif")]
}
set_maa_w_id <- function(id, from) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, "maa"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, c("maa_source","maa_verif")] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, c("maa_source","maa_verif")]
}

# Function to copy all relevant sources from one place to another
copy_sources <- function(id, from) {
  putki_yhd_temp[putki_yhd_temp$paikka_id == id,
                 c("maa_source","maa_verif","yla_source","yla_verif")] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, 
                   c("maa_source","maa_verif","yla_source","yla_verif")]
}


##### MANUAL FIXES ######

# Siuntio 0101p10 yläpää 10cm alempana kuin 10b, maanpinta putken pituuden mukaan.
putki_yhd_temp[putki_yhd_temp$paikka_id == 9935, "yla"] <-
  putki_yhd_temp[putki_yhd_temp$paikka_id == 43689, "yla"] - 0.10
set_maa_w_putki(9935); copy_sources(9935, from = 43689)

# Karkkila 0102 p8 ja p8b tietokannasta, parilla parempi yhteneväisyys.
set_yla_from_db(50438); set_maa_w_putki(50438)
set_yla_from_db(10148); set_maa_w_putki(10148)

# Perniö 0202 p10 ja p10AM yläpää tietokannasta. Maanpinta putken mukaan.
set_yla_from_db(9730); set_maa_w_putki(9730)
set_yla_w_id(100207, from = 9730); set_maa_w_id(100207, from = 9730)

# Oripää 0203 p3 ja p26 tietokannasta (DTM tukee hyvin, ELY hieman pielessä)
set_yla_from_db(9613); set_maa_w_putki(9613)
set_yla_from_db(9615); set_maa_w_putki(9615)

# Kuuminainen 0204 p8 ja p8AM tietokannasta (DTM tukee hyvin, ELY hieman pielessä)
set_yla_from_db(9739); set_maa_w_putki(9739)
set_yla_w_id(86005, from = 9739); set_maa_w_id(86005, from = 9739)

# Orivesi 0301 p10b ja p10bAM yläpäät DTMstä+putket.
set_yla_from_dtm(43837); set_maa_w_putki(43837) # p10b
set_yla_w_id(59765, from = 43837); set_maa_w_id(59765, from = 43837) # p10bAM
# Orivesi 0301 p10 yläpää DTM+putki, mutta putkesta 46cm. Maanpinta suoraan DTM.
putki_yhd_temp[putki_yhd_temp$paikka_id == 9673, "putki"] <- 0.46
putki_yhd_temp[putki_yhd_temp$paikka_id == 9673, "putki_source"] <- "man. chg."
set_yla_from_dtm(9673) # DTM + yllä asetettu putken pituus 46cm
set_maa_w_putki(9673) # (DTM + 46cm) - 46cm
# Orivesi p3, p3b, p3bAM DTM+putket yläpäiksi. Maanpinta yläpäiden mukaan.
set_yla_from_dtm(9666); set_maa_w_putki(9666) # p3
set_yla_from_dtm(43831); set_maa_w_putki(43831) # p3b
set_yla_w_id(59764, from = 43831); set_maa_w_id(59764, from = 43831) # p3bAM

# Jämijärvi 0302 p9/p9AM, p4, p5 yläpäät tietokannasta. Maanpinnat putkien mukaan.
set_yla_from_db(9848); set_maa_w_putki(9848) # p9
set_yla_w_id(65021, from = 9848); set_maa_w_id(65021, from = 9848) # p9AM
set_yla_from_db(9845); set_maa_w_putki(9845) # p4
set_yla_from_db(9842); set_maa_w_putki(9842) # p5

# Siikainen 0303 p3, p9, p10, p10AM yläpää tietokannasta
set_yla_from_db(9675); set_maa_w_putki(9675) # p3
set_yla_from_db(9680); set_maa_w_putki(9680) # p9
set_yla_from_db(9681); set_maa_w_putki(9681) # p10
set_yla_w_id(85999, from = 9681); set_maa_w_id(85999, from = 9681) # p10AM
# Siikainen 0303 p8, p4b, p5b, p7b, p10b yläpää DTM:stä
set_yla_from_dtm(9679); set_maa_w_putki(9679) # p8
set_yla_from_dtm(73607); set_maa_w_putki(73607) # p4b
set_yla_from_dtm(73608); set_maa_w_putki(73608) # p5b
set_yla_from_dtm(73609); set_maa_w_putki(73609) # p7b
set_yla_from_dtm(73610); set_maa_w_putki(73610) # p10b

# Seitseminen 0306 p2,p2AM, p3,p3AM, p4,p4AM, p5, p6,p6AM, p8,p8AM yläp db:stä
set_yla_from_db(45943); set_maa_w_putki(45943) # p2
set_yla_w_id(68210, from = 45943); set_maa_w_id(68210, from = 45943) # p2AM
set_yla_from_db(45944); set_maa_w_putki(45944) # p3
set_yla_w_id(51476, from = 45944); set_maa_w_id(51476, from = 45944) # p3AM
set_yla_from_db(45945); set_maa_w_putki(45945) # p4
set_yla_w_id(51477, from = 45945); set_maa_w_id(51477, from = 45945) # p4AM
set_yla_from_db(45946); set_maa_w_putki(45946) # p5
set_yla_from_db(45947); set_maa_w_putki(45947) # p6
set_yla_w_id(82014, from = 45947); set_maa_w_id(82014, from = 45947) # p6AM
set_yla_from_db(45949); set_maa_w_putki(45949) # p8
set_yla_w_id(51478, from = 45949); set_maa_w_id(51478, from = 45949) # p8AM

# Elimäki 0401 p4 yläpää tietokannasta
set_yla_from_db(10109); set_maa_w_putki(10109)

# Kotaniemi 0403 p1 ja limni yläpäät KM2:sta
set_yla_from_km2(9451); set_maa_w_putki(9451) # p1
set_yla_from_km2(75346); set_maa_w_putki(75346) # limni

# Parikkala 0404 p2, p8 yläpäät tietokannasta
set_yla_from_db(10097); set_maa_w_putki(10097) # p2
set_yla_from_db(10099); set_maa_w_putki(10099) # p8

# Pertunmaa 0501 p4 yläpää tietokannasta
set_yla_from_db(10126); set_maa_w_putki(10126) # p4
# Pertunmaa 0501 p7, p7AM, p9b yläpäät DTM:stä
set_yla_from_dtm(10132); set_maa_w_putki(10132) # p7
set_yla_w_id(88330, from = 10132); set_maa_w_id(88330, from = 10132) # p7AM
set_yla_from_dtm(45779); set_maa_w_putki(45779) # p9b

# ASEMIA VÄLIIN


# Hietajärvi Patvinsuo 0705 p1AM yläp tietokannasta, p1 identtiseksi p1AM kanssa
set_yla_from_db(63979); set_maa_w_putki(63979) # p1AM
set_yla_w_id(14363, from = 63979); set_maa_w_id(14363, from = 63979) # p1



