# Koodi kirjaa havaintopaikkojen kenttämittauksiin paikkaa vastaavan paikka_id:n
# hyödyntäen paikan tunnusta ja havaintoaseman tunnusta

# Tuodaan ELYjen tekemien kenttäkartoitusten tiedot, jotka manuaalisesti yhdistetty ja trimmattu
# (nimetty uudelleen ne automaattikohteet manuaaleiksi joilla ei vastaavaa manuaaliriviä, poistettu muut AM ja kaivot)

kenttamittaukset <- readxl::read_xlsx(
  file.path(D$data, "kenttamittaukset_raw.xlsx"),
  skip = 1, # Skipataan kommenttikenttä
  col_names = TRUE, # Headers
  na = c("NA","") # Detect NAs
  ) |> data.table::as.data.table()

# Tehdään tarvittavat muotoilut taululle. 
# (excelissä tyhjiä rivejä jaotteluun, ja cm-lukemia kopioitu ELYjen exceleistä)

# Drop cols with all NA (unknown reason for why these appear)
for (col in names(kenttamittaukset)) {
  if (all(is.na(kenttamittaukset[[col]]))) {
    kenttamittaukset[ , col] <- NULL
  }
}
rm(col)

# Do not import the col with bad values (related to some comments within the file)
kenttamittaukset[["BAD"]] <- NULL

# Suodatetaan pois tyhjät rivit
kenttamittaukset <- kenttamittaukset[!is.na(asema_nimi), ]

# Muutetaan senttimetrit metreiksi, ja poistetaan vanhat sarakkeet
putki_cmkorj <- kenttamittaukset$putki_ely / 100
kenttamittaukset[["putki_ely"]] <- putki_cmkorj
liejuun_cmkorj <- kenttamittaukset$liejuun_ely / 100
kenttamittaukset[["liejuun_ely"]] <- liejuun_cmkorj
rm(liejuun_cmkorj, putki_cmkorj)
# Format rest of the appropriate columns as numeric
kenttamittaukset[,"vanha_lat"] <- as.numeric(kenttamittaukset$vanha_lat)
kenttamittaukset[,"vanha_lon"] <- as.numeric(kenttamittaukset$vanha_lon)
kenttamittaukset[,"uusi_lat"] <- as.numeric(kenttamittaukset$uusi_lat)
kenttamittaukset[,"uusi_lon"] <- as.numeric(kenttamittaukset$uusi_lon)
kenttamittaukset[,"kor_ely"] <- as.numeric(kenttamittaukset$kor_ely)


# Korvataan palautettavan taulun kautta-merkit tekstistä viivalla,
# jotta mahd tallennuspolut eivät sekoa, ja samat tunnukset kuin kaikki_paikat-taul
for (sarake in names(kenttamittaukset)) {
  if (is.character(kenttamittaukset[[sarake]])) {
    kenttamittaukset[[sarake]] <- sub("/", "-", kenttamittaukset[[sarake]])
    # Trimmataan turhat välilyönnit tms. joita on taulussa.
    # Varoitus! trimws() Muuttaa luvut "tekstiksi" jos ajetaan lukusarakkeeseen
    kenttamittaukset[[sarake]] <- trimws(kenttamittaukset[[sarake]])
  }
}
rm(sarake)

########## ID kohdentaminen

# Alustetaan id vektori
idt <- rep(NA, nrow(kenttamittaukset))
# Iteroidaan kenttämittausten taulu riveittäin, koska monimutkainen operaatio
for (i in 1:nrow(kenttamittaukset)) {
  # Ohitetaan, jos paikan id on jo syötetty lähtötaulukkoon manuaalisesti
  if (!is.na(kenttamittaukset[i, paikka_id])) next
    
  # Luetaan KAIKISTA havaintopaikoista ne rivit joiden sanallinen PAIKAN tunnus
  # täsmää tarkasteltavan kenttämittausten rivin tunnukseen
  sama_tunnus <- kaikki_paikat[
    grep(
      # (välilyönnit poistettu kaikesta vertailusta)
      paste0(gsub(" ", "", kenttamittaukset[i, tunnus]),"$"),
      gsub(" ", "", kaikki_paikat[ , "tunnus"]),
      ignore.case = T)
    , ] # Kaikki sarakkeet
  
  # Luetaan näistä rajatuista paikoista ne joilla ASEMAN tunnus täsmää, ja palautetaan paikka_id
  # Tehdään tämä vain, jos ensimmäisellä ei rajautunut yhteen vaihtoehtoon
  if (nrow(sama_tunnus) == 1L) {
    id_match <- sama_tunnus[ , "paikka_id"]
  } else {
  id_match <- sama_tunnus[
    grep(
      gsub(" ", "", kenttamittaukset[i, asema_tunnus]),
      gsub(" ", "", sama_tunnus[ , "asema_tunnus"]),
      ignore.case = T),
    "paikka_id"]
    # Jos aseman tunnus rajaa nolliin, yritetään vielä etsiä aseman nimen perusteella
    # (varmempi tulevaisuudessa, kun VHSP-asemien tunnukset saattavat vaihtua pitkistä 4-numeroisiksi)
    if (length(id_match) == 0L || rlang::is_empty(id_match)) {
      id_match <- sama_tunnus[
        # Etsitään rivit, joilla aseman nimi on kenttämittaustaulun kohteen aseman nimen kaltainen
        stringr::str_like(sama_tunnus$asema_nimi,
                          # Otetaan 4 ensimmäistä kirjainta ja joustava haku
                          pattern = paste0(
                            strtrim(kenttamittaukset[i, asema_nimi], 4),"%")) 
        , "paikka_id"] # Palautetaan paikka_id
    }
  }

  
  # Jos vielä on monta vaihtoehtoa jäljellä, tai ei yhtään, pyydetään käyttäjää selvittämään tarkempi paikka id.
  if (length(id_match)>1L || rlang::is_empty(id_match)) {
      idt[i] <- readline(
        cat(paste(
          c("Syötä id paikalle:",
          kenttamittaukset[i,c("asema_nimi","asema_tunnus","tunnus","uusi_lat","uusi_lon") ]))))
  } else { # Muuten kirjataan yhtä kohdetta täsmäävä paikka_id vektoriin indeksin mukaan
    idt[i] <- id_match
  }
}
rm(i, id_match, sama_tunnus)


# Kirjataan täsmäävät tulokset kenttämittauskohteiden omaan sarakkeeseen
kenttamittaukset[["paikka_id"]] <- ifelse(is.na(kenttamittaukset[["paikka_id"]]),
                                          idt,
                                          kenttamittaukset[["paikka_id"]])
# Tarkistetaan, ettei jäänyt tyhjiä id paikkoja
if (any(is.na(kenttamittaukset$paikka_id))) 
  stop("Tyhjiä arvoja kenttämittausten paikkojen id määrityksessä!")
# Tarkistetaan ettei duplikaatti ID:itä
if (any(duplicated(kenttamittaukset$paikka_id))) {
  stop("Kenttämittauksissa kohdistetuissa ID:issä duplikaatti")
}
rm(idt)


# Haetaan vielä tietokannasta asemien ID:t, jotta valmista taulukkoa voi
# käyttää vaikka aseman nimi tietokannassa muuttuisi.
# Osaa ei ole linkitetty asemaan. Niillä as. id tulee NA
as_idt <- rep(NA, nrow(kenttamittaukset))

for (i in 1:nrow(kenttamittaukset)) {
  # Jos ei kaikki_paikat taulussa, paikkaa ei ole linkitetty asemaan, ->NA
  if (nrow(kaikki_paikat[
    kaikki_paikat$paikka_id == kenttamittaukset$paikka_id[i], ]) == 0) {
    as_idt[i] <- NA
    next
  }
  if (nrow(kaikki_paikat[
    kaikki_paikat$paikka_id == kenttamittaukset$paikka_id[i], ]) > 1) {
    stop("1111") }
  as_idt[i] <- kaikki_paikat[
    kaikki_paikat$paikka_id == kenttamittaukset$paikka_id[i],
    "asema_id"]
}
# Kirjataan asema_id:t omaan sarakkeeseen
kenttamittaukset[["asema_id"]] <- as_idt
# EI tarkisteta NA-arvojen varalta, koska NA on niillä joita ei ole link. asemaan

rm(i, as_idt)


# Tallennetaan id:illä täydennetty taulu omaksi csv:ksi, jota sktripti yrittää
# lähtökohtaisesti käyttää seuraavilla ajokerroilla
data.table::fwrite(kenttamittaukset,
                   file.path(D$data, "kenttamittaukset+idt.csv"),
                   na = NA,
                   bom = TRUE)
# BOM säilyttää erikoismerkit, eikä tässä tapauksessa haittaa,
# ks. https://stackoverflow.com/a/13398447/22207980


