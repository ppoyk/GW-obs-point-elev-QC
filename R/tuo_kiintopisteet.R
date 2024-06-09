# Load national reference point data from a file, loaded from MML database

# Tuodaan Suomen alueen kiintopisteet tiedostosta
# (Saatavilla: https://www.maanmittauslaitos.fi/kartat-ja-paikkatieto/aineistot-ja-rajapinnat/tuotekuvaukset/kiintopisteet)

kp <- sf::st_read(file.path(D$data,
                            "kiintopisteet","kiintopisterekisteri_koko_suomi.gpkg"),
                  layer = "kiintopisteet")

# Pudotetaan muutamia turhia sarakkeita
kp <- dplyr::select(kp, !grep("organisaatio", names(kp)))
kp <- dplyr::select(
  kp, !c("kuntakoodi","maakuntakoodi","valtakunnan_rajapiste_tosi_epatosi",
         "varamerkit","havinnyt_pvm","maastotarkistus_pvm","N60_havaintopvm"))
kp <- dplyr::select(
  kp, !c("varamerkkimittaukset","pisteen_nimi","UTM_karttalehti",
         "KKJ_karttalehti","KKJ_havaintopvm","KKJ_paikannus_E","KKJ_paikannus_N",
         "KKJ_viitetunnus","KKJ_tarkkuusluokka","KKJ_E","KKJ_N",
         "N60_viitetunnus","N60_tarkkuusluokka"))

# Possible to select a subset of control points based on the accuracy classification

# Mahdollisia tarkkuusluokkia N2000-korkeudelle 7kpl: 6N01-06 + 6N0X (ei luokkaa)
kp1lk <- kp[kp$N2000_tarkkuusluokka %in% c("6N01"), ]
kp1lk <- kp[kp$N2000_tarkkuusluokka %in% c("6N01") |
              kp$EUREF_FIN_tarkkuusluokka %in% c("6EF1","6EFb"), ]

# Suodatetaan käytettäviksi kiintopisteiksi vain E1 ja E1b luokan pisteet.
# (Nämä määrittelevät EUREF-FIN koordinaatiston)
kp1lk <- kp[kp$EUREF_FIN_tarkkuusluokka %in% c("6EF1","6EFb"), ]


