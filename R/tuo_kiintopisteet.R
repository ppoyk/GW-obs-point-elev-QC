# Load national reference point data from a file, loaded from MML database

# Tuodaan Suomen alueen kiintopisteet tiedostosta
# (Saatavilla: https://www.maanmittauslaitos.fi/kartat-ja-paikkatieto/aineistot-ja-rajapinnat/tuotekuvaukset/kiintopisteet)


# Define a function
lue_kiintopisteet_ <- function(kp_path, layer) {
  
  kp <- sf::st_read(kp_path, layer = layer)
  
  # Pudotetaan muutamia turhia sarakkeita
  kp <- dplyr::select(kp, !grep("organisaatio", names(kp))) |>
    dplyr::select(
      !c("kuntakoodi","maakuntakoodi","valtakunnan_rajapiste_tosi_epatosi",
         "varamerkit","havinnyt_pvm","maastotarkistus_pvm","N60_havaintopvm")) |>
    dplyr::select(
      !c("varamerkkimittaukset","pisteen_nimi","UTM_karttalehti",
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
  
  return(kp1lk)
}

# Memoise the function
lue_kiintopisteet <- memoise::memoise(
  lue_kiintopisteet_,
  # Set disk cache to remember results during consecutive sourcing of this file
  cache = cachem::cache_disk(file.path(D$c,"lue_kiintopisteet"), max_size = 5*1024^2),
  hash = function(x) rlang::hash(x)) # Explicitly define the default hash fun

# Execute memoised fun
kp1lk <- lue_kiintopisteet(
  file.path(D$data, "kiintopisteet", "kiintopisterekisteri_koko_suomi.gpkg"),
  layer = "kiintopisteet")

