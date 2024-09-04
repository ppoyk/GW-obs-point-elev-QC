# Script to run before the output of new elev values of places.

# Hard-codes corrections to the automatic logic of selecting the most suitable 
# elevation values. Based on manual investigations.
# Required, as the monit.network has too diverse cases to account for with simple rules
# (May break indicated sources)


# Function to set the maa with current pipetop minus the pipe vis.len 
# Option to set the pipe len manually.
set_maa_w_putki <- function(id, putki = NULL) {
  if (!(id %in% putki_yhd_temp$paikka_id)) stop("Korjattavaa paikkaId:tä ei ole")
  if (is.null(putki)) {
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
  if (!(id %in% putki_yhd_temp$paikka_id)) stop("Korjattavaa paikkaId:tä ei ole")
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_db"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <- "DB"
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}
# Function to set the top elev from DTM elev + pipe. Sources accordingly.
set_yla_from_dtm <- function(id) {
  if (!(id %in% putki_yhd_temp$paikka_id)) stop("Korjattavaa paikkaId:tä ei ole")
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    kaikki_paikat[kaikki_paikat$paikka_id == id, "dtm_elev"] +
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <-
    paste0("DTM+", putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki_source"])
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}
# Function to set the top elev from KM2 elev + pipe. Sources accordingly.
set_yla_from_km2 <- function(id) {
  if (!(id %in% putki_yhd_temp$paikka_id)) stop("Korjattavaa paikkaId:tä ei ole")
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    kaikki_paikat[kaikki_paikat$paikka_id == id, "km2_elev"] +
    putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_source"] <-
    paste0("KM2+", putki_yhd_temp[putki_yhd_temp$paikka_id == id, "putki_source"])
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla_verif"] <- "man. chg."
}

# Set top elev or ground elev from another pipe. Copy also elev sources.
set_yla_w_id <- function(id, from) {
  if (!all(c(id,from) %in% putki_yhd_temp$paikka_id))stop("PaikkaId:tä ei löydy")
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "yla"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, "yla"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, c("yla_source","yla_verif")] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, c("yla_source","yla_verif")]
}
set_maa_w_id <- function(id, from) {
  if (!all(c(id,from) %in% putki_yhd_temp$paikka_id))stop("PaikkaId:tä ei löydy")
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, "maa"] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, "maa"]
  putki_yhd_temp[putki_yhd_temp$paikka_id == id, c("maa_source","maa_verif")] <-
    putki_yhd_temp[putki_yhd_temp$paikka_id == from, c("maa_source","maa_verif")]
}

# Function to copy all relevant sources from one place to another
copy_sources <- function(id, from) {
  if (!all(c(id,from) %in% putki_yhd_temp$paikka_id))stop("PaikkaId:tä ei löydy")
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
set_yla_from_db(10148); set_maa_w_putki(10148) # p8
set_yla_from_db(50438); set_maa_w_putki(50438) # p8b

# Perniö 0202 p10 ja p10AM yläpää tietokannasta. Maanpinta putken mukaan.
set_yla_from_db(9730); set_maa_w_putki(9730) # p10
set_yla_w_id(100207, from = 9730); set_maa_w_id(100207, from = 9730) # p10AM

# Oripää 0203 p3 ja p26 tietokannasta (DTM tukee hyvin, ELY hieman pielessä)
set_yla_from_db(9613); set_maa_w_putki(9613) # p3
set_yla_from_db(9615); set_maa_w_putki(9615) # p26

# Kuuminainen 0204 p8 ja p8AM tietokannasta (DTM tukee hyvin, ELY hieman pielessä)
set_yla_from_db(9739); set_maa_w_putki(9739) # p8
set_yla_w_id(86005, from = 9739); set_maa_w_id(86005, from = 9739) # p8AM


# Orivesi 0301 p10 yläpää DTM+putki, mutta putkesta 46cm. Maanpinta suoraan DTM.
putki_yhd_temp[putki_yhd_temp$paikka_id == 9673, "putki"] <- 0.46
putki_yhd_temp[putki_yhd_temp$paikka_id == 9673, "putki_source"] <- "man. chg."
set_yla_from_dtm(9673) # DTM + yllä asetettu putken pituus 46cm
set_maa_w_putki(9673) # (DTM + 46cm) - 46cm
# Orivesi 0301 p10b ja p10bAM, p3, p3b, p3bAM yläpäät DTMstä+putket.
set_yla_from_dtm(43837); set_maa_w_putki(43837) # p10b
set_yla_w_id(59765, from = 43837); set_maa_w_id(59765, from = 43837) # p10bAM
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


# Kangaslahti Rautavaara 0603 p6b, p7b/p7bAM yläp vanh putkista miinus b- ja ei-b-putkien vanh ero (jotka valokuv mukaiset)
putki_yhd_temp[putki_yhd_temp$paikka_id == 41917, "yla"] <- #p6b:n yläksi...
  putki_yhd_temp[putki_yhd_temp$paikka_id == 10018, "yla"] - #p6:n valittu ylä, miin
  (putki_yhd_temp[putki_yhd_temp$paikka_id == 10018, "yla_db"] - #6sten vanh ero
     putki_yhd_temp[putki_yhd_temp$paikka_id == 41917, "yla_db"])
set_maa_w_putki(41917)
putki_yhd_temp[putki_yhd_temp$paikka_id == 41918, "yla"] <- #p7b:n yläksi...
  putki_yhd_temp[putki_yhd_temp$paikka_id == 10019, "yla"] - #p7:n valittu ylä, miin
  (putki_yhd_temp[putki_yhd_temp$paikka_id == 10019, "yla_db"] - #7jen vanh ero
     putki_yhd_temp[putki_yhd_temp$paikka_id == 41918, "yla_db"])
set_maa_w_putki(41918)
set_yla_w_id(54425, from = 41918); set_maa_w_id(54425, from = 41918) # p7bAM
# Kangaslahti0603 p9b:lle p9:n maanpinta(-74 vaait) +p9b putk pituus (valokuv arv)
set_maa_w_id(41920, from = 10021) # Aseta aluksi p9b maa p9:n mukaan
putki_yhd_temp[putki_yhd_temp$paikka_id == 41920, "yla"] <- #p9b ylä putk pit muk
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41920, "maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41920, "putki"]


# Kuuksenvaara Ilomantsi 0701 p4 ja p4b yläpää DTM:stä
set_yla_from_dtm(10557); set_maa_w_putki(10557) # p4
set_yla_from_dtm(41040); set_maa_w_putki(41040) # p4b
# Kuuksenvaara Ilomantsi 0701 p2 yläpksi p2b:n yläp miin 15 cm 
putki_yhd_temp[putki_yhd_temp$paikka_id == 9476, "yla"] <-
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41037, "yla"] - 0.15
set_maa_w_putki(9476)
# Kuuksenvaara Ilomantsi 0701 p10 yläpääksi p10b:n yläpää plus 10 cm 
putki_yhd_temp[putki_yhd_temp$paikka_id == 10563, "yla"] <-
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41045, "yla"] + 0.10
set_maa_w_putki(10563)
# Kuuksenvaara Ilomantsi 0701 p3b yläpää KM2:sta
set_yla_from_km2(41039); set_maa_w_putki(41039)


# Jaamankangas 0702 p2b yläpää KM2:sta
set_yla_from_km2(40881); set_maa_w_putki(40881)


# Jakokoski Kontiolahti 0703 p7 ja p10 yläpää tietokannasta
set_yla_from_db(10081); set_maa_w_putki(10081) # p7
set_yla_from_db(10084); set_maa_w_putki(10084) # p10
# Jakokoski Kontiolahti 0703 p2:lle p2b:n yhd_maa plus p2:n putken pituus
putki_yhd_temp[putki_yhd_temp$paikka_id == 10076, "yla"] <-
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41750, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 10076, "putki"]
set_maa_w_putki(10076)


# Hietajärvi Patvinsuo 0705 p1AM yläp tietokannasta, p1 identtiseksi p1AM kanssa
set_yla_from_db(63979); set_maa_w_putki(63979) # p1AM
set_yla_w_id(14363, from = 63979); set_maa_w_id(14363, from = 63979) # p1

# Rajamäki Karijoki 0801 p11b yläpää tietokannasta
set_yla_from_db(42048); set_maa_w_putki(42048) # p11b


# Kylänpää Laihia 0803 p2,p5,p10 yläpää tietokannasta
set_yla_from_db(9755); set_maa_w_putki(9755) # p2
set_yla_from_db(9758); set_maa_w_putki(9758) # p5
set_yla_from_db(9763); set_maa_w_putki(9763) # p10
# Kylänpää Laihia 0803 p1b, p1bAM, p6b, p6bAM, p8b, p8bAM yläpää yhd_yla
putki_yhd_temp[putki_yhd_temp$paikka_id == 41995, "yla"] <- # p1b
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41995, "yhd_yla"]
putki_yhd_temp[putki_yhd_temp$paikka_id == 41997, "yla"] <- # p6b
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41997, "yhd_yla"]
putki_yhd_temp[putki_yhd_temp$paikka_id == 42002, "yla"] <- # p8b
  putki_yhd_temp[putki_yhd_temp$paikka_id == 42002, "yhd_yla"]
set_maa_w_putki(41995); set_maa_w_putki(41997); set_maa_w_putki(42002) # Grounds
set_yla_w_id(88738, from = 41995); set_maa_w_id(88738, from = 41995) # p1bAM
set_yla_w_id(80921, from = 41997); set_maa_w_id(80921, from = 41997) # p6bAM
set_yla_w_id(80922, from = 42002); set_maa_w_id(80922, from = 42002) # p8bAM
# Kylänpää Laihia 0803 p1,p6,p8,p3,p7,p9 yläpksi uuden putken yhd_maa+vanh putk pituus
putki_yhd_temp[putki_yhd_temp$paikka_id == 9754, "yla"] <- # p1
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41995, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9754, "putki"]
set_maa_w_putki(9754)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9759, "yla"] <- # p6
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41997, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9759, "putki"]
set_maa_w_putki(9759)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9761, "yla"] <- # p8
  putki_yhd_temp[putki_yhd_temp$paikka_id == 42002, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9761, "putki"]
set_maa_w_putki(9761)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9756, "yla"] <- # p3
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41996, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9756, "putki"]
set_maa_w_putki(9756)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9760, "yla"] <- # p7
  putki_yhd_temp[putki_yhd_temp$paikka_id == 41999, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9760, "putki"]
set_maa_w_putki(9760)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9762, "yla"] <- # p9
  putki_yhd_temp[putki_yhd_temp$paikka_id == 42003, "yhd_yla"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9762, "putki"]
set_maa_w_putki(9762)


# Mutkala Joutsa 0901p1,p2,p3,p3AM,p4,p5,p5AM,p7,p8,p8AM,p9,p9AM,p10 yläp dbstä 
set_yla_from_db(9647);  set_maa_w_putki(9647)  # p1
set_yla_from_db(9648);  set_maa_w_putki(9648)  # p2
set_yla_from_db(9649);  set_maa_w_putki(9649)  # p3
set_yla_w_id(88351, from = 9649); set_maa_w_id(88351, from = 9649) # p3AM
set_yla_from_db(9650);  set_maa_w_putki(9650)  # p4
set_yla_from_db(9651);  set_maa_w_putki(9651)  # p5
set_yla_w_id(88352, from = 9651); set_maa_w_id(88352, from = 9651) # p5AM
set_yla_from_db(9653);  set_maa_w_putki(9653)  # p7
set_yla_from_db(9654);  set_maa_w_putki(9654)  # p8
set_yla_w_id(88353, from = 9654); set_maa_w_id(88353, from = 9654) # p8AM
set_yla_from_db(9655);  set_maa_w_putki(9655)  # p9
set_yla_w_id(88354, from = 9655); set_maa_w_id(88354, from = 9655) # p9AM
set_yla_from_db(9656);  set_maa_w_putki(9656)  # p10

# Vehkoo Multia 0902 p6b ja p6bAM yläpää putkikortista (163,09)
putki_yhd_temp[putki_yhd_temp$paikka_id == 43289, "yla"] <- 163.09 # p6b
set_maa_w_putki(43289)
set_yla_w_id(88295, from = 43289); set_maa_w_id(88295, from = 43289) # p6bAM

# Äijälä Laukaa 0903p11/p11AM,p12/p12AM,p14/p14AM,p15/p15AM,p3,p9,p6,p13 ylä dbstä
set_yla_from_db(10005); set_maa_w_putki(10005) # p11
set_yla_w_id(82246, from = 10005); set_maa_w_id(82246, from = 10005) # p11AM
set_yla_from_db(10006); set_maa_w_putki(10006) # p12
set_yla_w_id(82247, from = 10006); set_maa_w_id(82247, from = 10006) # p12AM
set_yla_from_db(10008); set_maa_w_putki(10008) # p14
set_yla_w_id(82248, from = 10008); set_maa_w_id(82248, from = 10008) # p14AM
set_yla_from_db(10009); set_maa_w_putki(10009) # p15
set_yla_w_id(82249, from = 10009); set_maa_w_id(82249, from = 10009) # p15AM
set_yla_from_db(10159); set_maa_w_putki(10159) # p3
set_yla_from_db(9537);  set_maa_w_putki(9537)  # p9
set_yla_from_db(9535);  set_maa_w_putki(9535)  # p6
set_yla_from_db(10007); set_maa_w_putki(10007) # p13

# Taikkomäki Karstula 0904 p10b ja p10bAM tietokannan mukaisiksi. ELY sus.
set_yla_from_db(44421); set_maa_w_putki(44421) # p10b
set_yla_w_id(82236, from = 44421); set_maa_w_id(82236, from = 44421) # p10bAM

# Kälviä 1003 p6, p10 yläpää tietokannasta
set_yla_from_db(9746); set_maa_w_putki(9746) # p6
set_yla_from_db(9750); set_maa_w_putki(9750) # p10

# Kolmisoppi 1201 p9b, p9bAM yläpää tietokannasta; p2 yläpää p2b:n yhd_maa+p2:n putki
set_yla_from_db(44336); set_maa_w_putki(44336) # p9b
set_yla_w_id(75654, from = 44336); set_maa_w_id(75654, from = 44336) # p9bAM
putki_yhd_temp[putki_yhd_temp$paikka_id == 9785, "yla"] <- # p2
  putki_yhd_temp[putki_yhd_temp$paikka_id == 44330, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9785, "putki"]
set_maa_w_putki(9785)


# Lumiaho 1202 p1b yläpää DTM:stä
set_yla_from_dtm(42113); set_maa_w_putki(42113)
# Lumiaho 1202limni,limAM putket 106 cm kuten p11:ssa, p11 koord korjattiin limnin koordksi, jolloin yläp tulee samaksi kuin limnillä/limAM:llä
putki_yhd_temp[putki_yhd_temp$paikka_id == 37921, "putki"] <- 1.06 
putki_yhd_temp[putki_yhd_temp$paikka_id == 37921, "putki_source"] <- "DB(1202p11)"
set_yla_from_dtm(37921); set_maa_w_putki(37921) # 1202limni (DTM was selected by def)
set_yla_w_id(64028, from = 37921); set_maa_w_id(64028, from = 37921) #1202limAM
set_yla_w_id(9961, from = 37921); set_maa_w_id(9961, from = 37921) #p11 (just in case)


# Alakangas 1203 p10 ja p10b yläpäät tietokannasta
set_yla_from_db(9570); set_maa_w_putki(9570) # p10
set_yla_from_db(43427); set_maa_w_putki(43427) # p10b


# Kullisuo 1204 p11 yläpää limnin yläpään mukaiseksi + koord korjattu vastaaviksi
set_yla_w_id(9959, from = 37922); set_maa_w_putki(9959) # p11
# limni / limAM putkipituus p11:n db perustuv putken mukaiseksi (ei vain arvioida 1m)
putki_yhd_temp[putki_yhd_temp$paikka_id == 37922, "putki"] <- # limni
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9959, "putki"]
putki_yhd_temp[putki_yhd_temp$paikka_id == 82270, "putki"] <- #limAM
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9959, "putki"]
putki_yhd_temp[putki_yhd_temp$paikka_id == 37922, "putki_source"] <- "DB(1204p11)"
putki_yhd_temp[putki_yhd_temp$paikka_id == 82270, "putki_source"] <- "DB(1204p11)"
set_maa_w_putki(37922); set_maa_w_putki(82270) # limni/limAM ground


# Jokiniemi Pesiö 1209p1/p1AM, p10/limni/limAM yläp 2000-luv vaait mukaan (p10=limni)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9945, "yla"] <- 218.7677 # p1
set_maa_w_putki(9945)
set_yla_w_id(86020, from = 9945); set_maa_w_id(86020, from = 9945)  # p1AM
putki_yhd_temp[putki_yhd_temp$paikka_id == 9944, "yla"] <- 219.3402 # p10
set_maa_w_putki(9944)
set_yla_w_id(37891, from = 9944); set_maa_w_id(37891, from = 9944) # limni
set_yla_w_id(86023, from = 9944); set_maa_w_id(86023, from = 9944) # limAM 
# Jokiniemi Pesiö 1209 p2/p2AM, p4, p8/p8AM, p10 yläpäät tietokannasta
set_yla_from_db(9939); set_maa_w_putki(9939) # p2
set_yla_w_id(86021, from = 9939); set_maa_w_id(86021, from = 9939) # p2AM
set_yla_from_db(9940); set_maa_w_putki(9940) # p4
set_yla_from_db(9942); set_maa_w_putki(9942) # p8
set_yla_w_id(86022, from = 9942); set_maa_w_id(86022, from = 9942) # p8AM
set_yla_from_db(9944); set_maa_w_putki(9944) # p10


# Könölä Tornio 1301 p3, p4,limni,p5/p5b/p5bAM,p7/p7b/p7bAM,p8/p8AM yläpää db:stä
set_yla_from_db(9810); set_maa_w_putki(9810) # p3
set_yla_from_db(9716); set_maa_w_putki(9716) # p4
set_yla_from_db(78408); set_maa_w_putki(78408) # limni
set_yla_from_db(9717); set_maa_w_putki(9717) # p5
set_yla_from_db(65120); set_maa_w_putki(65120) # p5b
set_yla_w_id(82228, from = 65120); set_maa_w_id(82228, from = 65120) # p5bAM
set_yla_from_db(9811); set_maa_w_putki(9811) # p7
set_yla_from_db(65117); set_maa_w_putki(65117) # p7b
set_yla_w_id(82229, from = 65117); set_maa_w_id(82229, from = 65117) # p7bAM
set_yla_from_db(9812); set_maa_w_putki(9812) # p8
set_yla_w_id(82230, from = 9812); set_maa_w_id(82230, from = 9812) # p8AM


# Lautavaara 1302 p1, p3, p4, p10, p11 yläpää yhd_maa + putki
putki_yhd_temp[putki_yhd_temp$paikka_id == 9705, "yla"] <- # p1
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9705, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9705, "putki"]
set_maa_w_putki(9705)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9707, "yla"] <- # p3
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9707, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9707, "putki"]
set_maa_w_putki(9707)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9708, "yla"] <- # p4
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9708, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9708, "putki"]
set_maa_w_putki(9708)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9714, "yla"] <- # p10
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9714, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9714, "putki"]
set_maa_w_putki(9714)
putki_yhd_temp[putki_yhd_temp$paikka_id == 9715, "yla"] <- # p11
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9715, "yhd_maa"] +
  putki_yhd_temp[putki_yhd_temp$paikka_id == 9715, "putki"]
set_maa_w_putki(9715)
# Lautavaara 1302 limni & limAM:lle p9:n yläpää (ja p9 putken pituus)
set_yla_w_id(37846, from = 9713); set_maa_w_id(37846, from = 9713) # limni
set_yla_w_id(64999, from = 9713); set_maa_w_id(64999, from = 9713) # limAM
# Lautavaara 1302 p6b, p6bAM yläpää tietokannasta
set_yla_from_db(46139); set_maa_w_putki(46139) # p6b
set_yla_w_id(64996, from = 46139); set_maa_w_id(64996, from = 46139) # p6bAM
# Lautavaara 1302 p8, p8b, p8bAM yläpää KM2:sta
set_yla_from_km2(9712); set_maa_w_putki(9712) # p8
set_yla_from_km2(46141); set_maa_w_putki(46141) # p8b
set_yla_w_id(64998, from = 46141); set_maa_w_id(64998, from = 46141) # p8bAM


# Nellim Inari 1306 p2 yläpää DTMstä
set_yla_from_dtm(9827); set_maa_w_putki(9827) # p2


# Pallas 1313 p5, p5AM, p6, p6AM yläpää tietokannasta
set_yla_from_db(87140); set_maa_w_putki(87140) # p5
set_yla_w_id(88402, from = 87140); set_maa_w_id(88402, from = 87140) # p5AM
set_yla_from_db(87141); set_maa_w_putki(87141) # p6
set_yla_w_id(88403, from = 87141); set_maa_w_id(88403, from = 87141) # p6AM

