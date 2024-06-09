
# Kaikilla asemien havaintopaikoilla ei ole karttalehden tietoa, eikä
# karttalehteä ole asemakohtaisissa tiedoissa.
# Myöskin laserkeilausaineiston tuotantoalueita ei saa dataksi ulos MML:n
# sivulta https://tilannekartta.maanmittauslaitos.fi/laserkeilaus

# # Joudutaan muodostamaan tuotantoaluetietojen matriisi käsin referoimalla
# asemien sijainteja. Apuna mm. https://hkp.maanmittauslaitos.fi/hkp/published/fi/4343c1b4-7d8f-4473-896a-70f930f36be1 

# Matriisi perustuu haettujen asemien ID:iden vakiojärjestykseen tietokannassa.
# Otetaan lisäksi karttalehdet ylös, jotta saadaan selville mitä alueita 
# pitää aineistosta ladata, koska muuten tallennustila levyllä ei riitä.
# (5p aineiston lataaminen pitää tehdä manuaalisesti, eikä aluetta voi rajata kuin karttalehdittäin)

# Alueiden kuvaustilanne 03/2024. Seuraavilta aineiston tuotantoalueilta ei ole aineistoa saatavilla.
# Aluejako ei vielä kata koko Suomea, joten niiden ulkopuolella olevat on merkitty "ei_aluetta".
ei_ole_5p <- c("Kilpisjärvi","Ala-Kilpisjarvi","Kaaresuvanto",
               "Ivalo","Raja-Jooseppi","Lisma","Vuotso",
               "Kittilä","Jeesiö","Martti","Tuntsa","Saija","Meltaus",
               "Rovajarvi","Salla","Muurola","Narkaus","Kemi","Kuusamo",
               "Soivio","Taivalkoski","Ylikiiminki","Suomussalmi","Pyhäjoki",
               "Muhos","Hyrynsalmi","Kuhmo","Vieremä","Veteli",
               "Närpiö","Seinäjoki","Saarijarvi","Joensuu","Varkaus",
               "Rääkkylä","Kitee","Ruovesi","Jämsä","Sastamala","Kouvola",
               "Uusikaupunki","Kumlinge","Karkkila","ei_aluetta")
# Lisäksi Loviisa ja Hanko vain osittain saatavilla.

# Asemien linkitys karttalehtiin ja 5p-aineiston tuotantoalueisiin
linkitykset_5p <- data.frame(
  matrix(ncol = 3, byrow = TRUE,
         data = c(
           8,   "L4113E", "Inkoo",
           26,  "L4213E", "Karkkila",
           25,  "L4414D", "Lahti",
           38,  "M4312B, M4134H", "Padasjoki",
           58,  "L4122H", "Karkkila",
           59,  "L4324F", "Loviisa",
           697, "L4414C", "Lahti",
           861, "L4143C", "Helsinki",
           1010,"K3434B", "Hanko",
           1113,"L4413B, L4413A", "Lahti", 
           1120,"L4132E, L4132F", "Helsinki",
           1129,"L4111E, L4111F, L4111H, L4111G", "Inkoo",
           1128,"L4314F, L4314E", "Loviisa",
           18,  "L2343E", "Ahvenanmaa",
           49,  "L3332E", "Hanko",
           37,  "L3442D, M3331C", "Loimaa",
           35,  "M3234H", "Pori",
           874, "M3324G", "Sastamala",
           1044,"L3344B", "Salo",
           172, "L2343E, L2343C", "Ahvenanmaa",
           1124,"L2341G", "Ahvenanmaa",
           9,   "M4241A, M4241B", "Tampere",
           48,  "M3442F, M3442H", "Parkano",
           7,   "N3312E, N3312G", "Merikarvia",
           173, "L4223A, L4223C, L4214B, L4214D", "Karkkila",
           698, "N4112A", "Ruovesi",
           1119,"N3343B, N3344A, N3341H, N3342G", "Parkano",
           1078,"N4113E", "Ruovesi",
           1122,"N3323H, N3323G", "Parkano",
           19,  "L4432H", "Kouvola",
           39,  "L5222A", "Kouvola",
           4,   "M5144H, M5144F", "Parikkala",
           20,  "M5414D, M5423C", "Parikkala",
           1126,"M5123G, M5141A, M5123H, M5141B", "Puumala",
           1127,"M5324B, M5322H", "Parikkala",
           24,  "M4433B", "Mäntyharju",
           40,  "M5232A, M5232C", "Puumala",
           50,  "N5122C, N5122A", "Pieksamaki",
           50,  "N5122C, N5122A", "Mikkeli",
           1,   "N5412C, N5412A", "Heinävesi",
           3,   "P4432E", "Viitasaari",
           3,   "P4432E", "Iisalmi",
           43,  "P5241D, P5241B, P5241A, P5241C", "Nilsiä",
           21,  "Q5121G, Q5123A", "Vieremä",
           1121,"P5144G", "Juuka",
           22,  "N6222F, P6111E", "Ilomantsi",
           41,  "N5424H", "Joensuu",
           5,   "P5331C, P5331D", "Joensuu",
           42,  "Q5311A, Q5133G", "Nurmes",
           1008,"P5433H, P5433F, P5433G", "Koitere",
           1117,"Q5323C, Q5314D", "Nurmes",
           1118,"P5342C, P5342A", "Koitere",
           1118,"P5341D", "Joensuu",
           52,  "N3411H, N3413B, N3412G, N3414A", "Närpiö",
           11,  "N4222A", "Alavus",
           10,  "P3323E", "Seinäjoki",
           873, "P4114B, P4114D", "Alajärvi",
           1004,"Q4112B", "Kokkola",
           1114,"P4232F", "Perho",
           1115,"N4224E, N4224F, N4224G, N4224H", "Alavus",
           1116,"Q4111H, Q4112G", "Kokkola",
           6,   "M4441B, M4441D", "Joutsa",
           44,  "N4243A, N4241G", "Saarijarvi",
           51,  "N4423G", "Jyväskylä",
           2,   "P4134E", "Saarijarvi",
           114, "N4422E", "Äänekoski",
           1018,"N4321G", "Jyväskylä",
           45,  "P4241D, P4241C, P4241B", "Perho",
           46,  "Q4312D", "Toholampi",
           46,  "Q4321C", "Ylivieska",
           36,  "Q4122E", "Kokkola",
           12,  "Q4241A", "Pyhäjoki",
           47,  "Q4433D, Q4433B, Q4433C", "Pyhäntä",
           55,  "R4134F", "Pyhäjoki",
           32,  "S5113B, S5114A", "Taivalkoski",
           34,  "S5412B, S5412D", "Kuusamo",
           723, "Q4311H, Q4313B", "Pyhäjärvi",
           1125,"T5321C, T5321E, T5312F", "Hautajärvi",
           725, "R5212D, R5221C, R5212F, R5212B, R5221A", "Puolanka",
           726, "S4332G, S4334A, S4334C", "Pudasjärvi",
           1005,"Q4441A", "Siikalatva",
           13,  "Q5233A", "Sotkamo",
           14,  "R5314C, R5313D, R5314A", "Lentiira",
           54,  "R5121E, R5121G", "Vaala",
           54,  "R5121E, R5121G", "Ristijärvi",
           31,  "R5144H", "Hyrynsalmi",
           28,  "R5233B", "Näljänkä",
           115, "R5233D", "Näljänkä",
           30,  "R5233B", "Näljänkä",
           29,  "R5233D", "Näljänkä",
           27,  "R5233D, R5233B", "Näljänkä",
           1009,"R5233G", "Suomussalmi",
           1112,"S5312H, S5314B, S5314A, S5312F, S5321E, S5321G", "Hossa",
           15,  "S4243D, S4243B", "Kemi",
           33,  "T4431G", "Rovajarvi",
           56,  "T5241F, T5241E", "Salla",
           57,  "U4343D", "Pelkosenniemi",
           16,  "V4123F", "ei_aluetta",
           17,  "W5132A, W5131B, W5131D, W5132C", "Nellim",
           893, "W4333A", "ei_aluetta",
           907, "V4134A", "ei_aluetta",
           718, "M3442B, N3331A, N3331C, M3442D", "Parkano",
           719, "L3441F, L3441D, L3441E, L3441C", "Loimaa",
           639, "M3314G, M3314E, M3314F, M3313H, M3332A, M3331B, M3331A", "Loimaa",
           251, "L4142F, L4233A, L4144B, L4144C, L4142H", "Helsinki",
           825, "L4142B, L4142D", "Karkkila",
           713, "Q4211G", "Kokkola",
           316, "R4332G, R4334A, R4334C, R4333B, R4333D, R4334B, R4332H", "Vaala",
           824, "M3323C, M3323E, M3323D", "Uusikaupunki",
           635, "L3411H", "Turku",
           759, "L3411H, L3412G", "Turku",
           905, "L3413A, L3413B", "Turku",
           282, "M5111B, M5112A", "Kouvola",
           702, "L5124F", "Kotka",
           226, "N5323C, N5314D, N5323A", "Savonlinna",
           703, "M5412E, M5412F", "Parikkala",
           860, "M4322G, M4322H, M4322F", "Heinola",
           862, "N4444D", "Tervo",
           609, "P3434F, P3443E", "Veteli",
           715, "N3441B, N3442A, N3441A", "Kurikka",
           714, "P3411G, P3411H", "Vaasa",
           792, "R5232C, R5232A, R5231D", "Näljänkä",
           1137, "U5232G, U5234A, U5231H", "Lattuna",
           1136, "U4131H, U4131F, U4131G, U4132E, U4132G", "Kolari",
           1138, "M3134C, M3134E, M3133F", "Uusikaupunki",
           1135, "S5321E", "Hossa",
           1134, "M4113D, M4113B", "Akaa",
           1132, "N4141C, N4141E, N4141D, N4141F", "Keuruu",
           1133, "L4212G", "Karkkila",
           1131, "M4234C, M4234E", "Jämsä"
         )))
# Nimetään sarakkeet oikein
colnames(linkitykset_5p) <- c("asema_id", "karttalehti", "tuotantoalue")

# Kuuluisiko alueelle löytyä 5p LAS aineistoa:
linkitykset_5p[["on_5p"]] <- ifelse(linkitykset_5p$tuotantoalue %in% ei_ole_5p,
                                    FALSE, TRUE)
# Linkityksen perusteella muodostetaan vektori asemista, joiden alueelta
# kuuluisi olla 5p-aineistoa saatavilla
as_on_5p <- unique(linkitykset_5p[linkitykset_5p$on_5p, "asema_id"])
# Some stations are over many prod areas, where only some might not have data.
# These are also noted as having data missing in the variable below
as_ei_5p <- unique(linkitykset_5p[!linkitykset_5p$on_5p, "asema_id"])


# Listataan kaikki tuotantoalueet
tuotantoalueet <- sort(unique(linkitykset_5p[!(linkitykset_5p[["tuotantoalue"]] %in% ei_ole_5p), "tuotantoalue"]))

# Tarkastellaan, mitä karttalehtiä pitää ladata miltäkin tuotantoalueelta
lkm <- 0
for (alue in tuotantoalueet) {
  lkm <- lkm + 1
  print(paste(
    c(alue,
      sort(unique(
        linkitykset_5p[linkitykset_5p[["tuotantoalue"]] == alue, "karttalehti"])))),
    collapse = " ")
}
writeLines(paste("======= Tuotantoalueiden lukumäärä:", lkm))


rm(lkm, alue, ei_ole_5p) # Siivous

