# Tiedosto jossa määritellään ohjelman käyttäytymistä ohjaavat, käyttäjälle
# tarkoitetut muuttujat
# Tämä ajetaan ohjelmassa ensimmäiseksi sisään

# Tallennetaan käyttäjän säätämien muuttujien arvot yhteen muuttujaan
USERCONF <- list(
  ### Tietokanta:
  # Halutaanko ladata uudet tiedot pohjavesitietokannasta, riippumatta siitä onko
  # tiedosto olemassa levyllä?
  pakota_lataus_asematiedot  = F, # Pohjavesiasemien tiedoille
  pakota_lataus_putkitiedot  = F, # Pelkille putkille
  pakota_lataus_paikkatiedot = F, # Tiedot kaikille havaintopaikkatyypeille
  
  ### MML:n 2x2m tarkkuuden maankorkeusaineisto (KM2):
  # Halutaanko ladata maanpintadata Maanmittauslaitoksen sivuilta, riippumatta
  # siitä onko aineisto jo olemassa levyllä?
  pakota_lataus_KM2 = F,
  
  ### Aineiston rajaamisen marginaalit latauksessa 
  # Asetetaan marginaali ladattavalle 2x2m maankorkeusaineistolle metreinä.
  # Käytetään aineiston latauksen bounding box määrittelyssä
  loc_dtm_margin = 75, # Putkikohtaiselle aineistolle (laajat asemat)
  marg_2m_asemille = 75, # Asemakohtaiselle aineistolle
  
  ajo_las_alustalla = F, # Is program executed on the remote LAS processing platform?
  
  ### 5p/m2 laserkeilausaineiston käsittely
  # Halutaanko kaikille havainopaikoille uudestaan luoda ja tallentaa levylle
  # eri menetelmillä 5p-aineistosta luodut digital terrain model -tiedostot?
  luo_kaikki_5p_dtm = T,
  
  # Kehityskäytön alalista
  # ÄLÄ MUOKKAA ARVOJA ennen tarkkaa harkintaa (voi vaatia muunkin koodin muuttamista)
  DEV = list(
    # 5p/m2 lidarista luotavien ja tallennettavien digital terrain modelien tarkkuus
    dtm_luonti_solukoko = 0.5, # metriä x metriä
    # LAS-tiedostojen indeksoinnissa käytettävien LAX-tiedostojen luonti
    pakota_lax_luonti = FALSE, # Luo uudestaan tiedostot kaikille laz-tiedostoille
    # ELYjen maastomittausten linkittäminen paikka_id:isiin. Linkitetäänkö uudelleen?
    pakota_kenttamittausten_id_kohdennus = T, 
    experimental = F # CAUTION! Toggle experiments. This is not core functionality.
  )
)
