# Koodin tarkoitus on muodostaa kaiken LAS-aineiston katalogi
# https://r-lidar.github.io/lidRbook/engine.html#engine-dtm
# Tähän sidotaan kaikki käytettävä LAS-aineisto, josta voidaan
# nopeasti ladata muistiin vain halutut alueet, koska datamäärä on suuri

# Katalogin rakentaminen parantaa myös tiedostojen saumojen alueille osuvien
# kohteiden tarkkuutta, koska tiedostot voidaan tällä tavoin kohdistaa paremmin


# Too slow to search for all files, even with 'find'
# full_aineisto_5p <- shell(paste0('find -L ',D$las,'/ -name "*.laz"'), intern = T)
# Alternative with no full file paths:
# test <- shell(paste("ls -R -f -1",D$las,"| grep .laz$"), intern = T)
# Even slower:
# aineisto_5p <- list.files(file.path(D$las), pattern = "\\.laz$",
#                           recursive = T, full.names = T)

# List all map sheets which have been manually listed as being required.
karttalehdet <- unique(trimws(unlist(strsplit(linkitykset_5p$karttalehti, ","))))


# LAS-data production areas are split at map sheet scale A0000A, same scale
# our manually stored information on which map sheets are needed. Thus searching
# based on "karttalehdet" is adequate, and overlapping areas should get avoided. 

# Loop yearly folders starting from newest until data from all map sheets is found
# (5p/m2 data only available starting from 2019, warning issued at the end of loop)
lasfiles <- NULL # Init looping var
# Init starting year (latest data available only from previous year)
year <- as.numeric(format(Sys.Date(),"%Y")) - 1
# Start looping through yearly folders
while (year > 2018) {
  
  yeardir <- file.path(D$las, year)
  
  if (dir.exists(yeardir)) {
    # Search files in year-folder which are within listed map sheets
    newfiles <- shell(paste0(
      'find -L ',yeardir,' -regex ".*\\(',
      paste(karttalehdet, collapse = '\\|'), '\\)',
      '.*\\.laz$" -type f'), # Only .laz files
      intern = T) # for shell() to return back to R
    
    # Append files found from the processed year folder
    lasfiles <- c(lasfiles, newfiles)
    message(p("Year",year,"of LAS files scanned"))
  } else {
    warning("Directory of LAS data for year ",year," not found.")
    year <- year - 1
  }
  
  # Check if there are still missing map sheets
  nfiles_found <- sapply(karttalehdet, FUN = function(s) {
    sum(grepl(s, basename(lasfiles))) }) #How many files of the map sheet are found
  # Note sheets missing if they don't have >= 9 out of 9 files found
  puuttuv_kartlehd <- names(nfiles_found[nfiles_found < 9])
  
  # If all required map sheets have not been found, move index to prev year
  if (rlang::is_empty(puuttuv_kartlehd)) {
    message("\nNo missing map sheets of LAS data ✅")
    break
  } else {
    year <- year - 1 # Set to earlier year for next loop
    # If precedes LAS-data prod start year, warn sheets that are still missing
    if (year < 2019) {
      warning("Not all map sheets found in full LAS data: ",
              paste(puuttuv_kartlehd, collapse = ", "))
      break
    }
  }
}
# Leave only the sheets which should have LAS data (according to mapping)
puuttuv_kartlehd <- puuttuv_kartlehd[unlist(
  lapply(puuttuv_kartlehd,
         function(x) linkitykset_5p[grepl(x,linkitykset_5p$karttalehti), "on_5p"]))]
# If there are sheets which are indicated that they should have data, warn
if (rlang::is_empty(puuttuv_kartlehd)) {
  message("\nAll missing mapsheets of LAS data are known to have data missing✅")
} else warning("LAS data expected from following map sheets: ",puuttuv_kartlehd)

rm(year, yeardir, newfiles, nfiles_found, puuttuv_kartlehd)

# Drop files from test-flights (would-be duplicates)
lasfiles <- lasfiles[!grepl(".*test.*", lasfiles, ignore.case = T)]
# Drop "old" (redundant) files
lasfiles <- lasfiles[!grepl("/OLD/", lasfiles)]


# Get files stored in limited quality folders, with density of 0.5p/m2, and not wanted
raj_tied <- lasfiles[grepl("rajoitusalue", lasfiles, ignore.case = T)]
# Loop through limited quality files. If same 1x1km area exists in full qual., drop
# poor file. (1x1km data should be split between the two files.)
for (f in raj_tied) {
  if (sum(grepl(basename(f), lasfiles)) > 1) {
    lasfiles <- lasfiles[!lasfiles %in% f]
  }
}
rm(f, raj_tied)


# Drop duplicated files of the same mapsheet. Leave only 1 in newest year folder  
if(anyDuplicated(basename(lasfiles))) {
  for (fi in unique(basename(lasfiles))) {
    if (sum(grepl(fi, lasfiles)) > 1) { # If file occurs more than once
      dupfiles <- lasfiles[grepl(fi, lasfiles)] # Get dupfile full paths
      # Get file years from the path (root year folder)
      fileyears <- as.numeric(sapply(dupfiles, substr,
                                     start = nchar(D$las)+2, stop = nchar(D$las)+5))
      maxyear <- max(fileyears)
      # Drop files which are older than newest of the duplicate files
      dropped_files <- dupfiles[which(fileyears < maxyear)]
      # If more than 1 file remains, halt. More checks needed on what to scan
      if (length(dropped_files) < length(dupfiles) - 1) 
        stop("Too many files with same filename from the newest year: ",dupfiles)
      else
        lasfiles <- lasfiles[!lasfiles %in% dropped_files] # Drop files
    }
  }
  rm(fi, dupfiles,fileyears,maxyear,dropped_files)
}
# Check if duplicate removal successful
if (anyDuplicated(basename(lasfiles)))
  stop(paste("Duplicate LAS-files:",lasfiles[duplicated(basename(lasfiles))]))

message("Havaintopaikkojen LAS-aineiston tiedostot listattu.")

# Muodostetaan katalogi listatusta aineistosta
message("Muodostetaan LAS-tiedostojen katalogi...")
ktlg <- lidR::readLAScatalog(folder = lasfiles,
# Pudotetaan jo alusta luokat "overlap","isolated", "low point", "air points",
# "fault points", "high vegetation", jotta kevennetään datakuormaa
# Lisätietoa https://www.maanmittauslaitos.fi/kartat-ja-paikkatieto/asiantuntevalle-kayttajalle/tuotekuvaukset/laser-scanning-data-5-p 
                             filter = "-drop_class 12 16 7 15 17 5 -drop_overlap",
                             progress = F) 
# Catalog consists of separate files, not forming unified area.
lidR::opt_independent_files(ktlg) <- TRUE
message("Kaikkien havaintopaikkojen LAS-aineiston katalogi muodostettu")

# Luodaan lax-tiedostot jokaiselle katalogin laz-tiedostolle, joilla sitä ei vielä ole
# (https://rapidlasso.de/lasindex-spatial-indexing-of-lidar-data/)
# Pitäisi nopeuttaa tiedon rajaamista tiedostoista
#luo_katalogin_lax(ktlg, force = USERCONF$DEV$pakota_lax_luonti)
# EI KÄYTETÄ koska täyden aineiston laseralustalla ei ole kirjoitusoikeuksia kansioihin,
# eikä ole selvää tapaa tallentaa indeksointitiedostoja erilliseen kansioon,
# siten että tiedostojen tarkoitus ja yhteys alkuperäisiin säilyisi.



# Säädetään katalogin prosessointiosioita pienemmäksi, koska käytetään paljon
# pieniä alueita datasta. (vaatii pienemmän työmuistin, mutta voi olla hitaampi)
#lidR::opt_chunk_size(ktlg) <- 300
# Ei pitäisikään olla tarvetta säätää, koska aineistolle ei tässä tapauksessa
# tehdä mitään laajoja yhtenäisiä toimenpiteitä

