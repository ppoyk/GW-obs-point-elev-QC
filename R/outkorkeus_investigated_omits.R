# Script to run before the output of new elev values of places.

# Hard-codes which places must be dropped before uploading new elevs to the database.
# Affects only the putki_yhd_trim variable. The table without drops is retained.

# In the future, after investigations with ELYs etc., many IDs will likely
# be removed from here and moved to a subset to be uploaded to the db later.

# Function to drop a place from the table based on id.
drop_id <- function(ids) {
  putki_yhd_trim <-
    putki_yhd_trim[!putki_yhd_trim$paikka_id %in% ids, ]
}
# Function to drop places from the table based on the station identifier
drop_station <- function(as_t) {
  putki_yhd_trim <-
    putki_yhd_trim[!putki_yhd_trim$asema_tunnus %in% as_t, ]
}

# koko Keräkankare 0106, linkitysten oikeellisuus epäselvää. UUDELYyn yhteys
drop_station("0106")

# koko Pernaja-Koskenkylä 0110, linkitykset epäselvät. UUDELYyn yhteyttä
drop_station("0110")

# koko Nurmijärvi_Kiljava 0119, VO, SeSu, linkitykset. UUDELYyn yhteyttä
drop_station("0119")

# koko Jomala 0201, varmistus Ahv.maakunt.hall. & UUDELY ennen kuin päivitetään.
drop_station("0201")

# Perniö 0202p11 masinotekin laite (autom. vaikkei AM tunnuksessa)
drop_id(52191)

# koko Munittula 0210. EI AJETA MYÖHEMMINKÄÄN, ASEMA POISTUMASSA SEURANNASTA
drop_station("0210")

# PERTUNMAA Kolunoja 0501 P7a SeSu paikka
drop_id(40143)

# Särkelä Heinävesi 0504 koko asema. Huonoja maastomittauksia, vaikea maasto.
drop_station("0504")

# Raikuunkangas 0508 koko asema. Ei as.dokumentoitu, epäselvät linkitykset
drop_station("0508")

# Akonjoki Sonkajärvi 0604 koko asema. Huonoja maastomittauksia, vaikea maasto.
drop_station("0604")

# Kruunupyy 0806 koko asema. EI AJETA MYÖHEMMINKÄÄN, AS. POISTUMASSA SEURANNASTA
drop_station("0806")



