# Funktio, jolla voidaan antaa käyttäjälle mahdollisuus valita tiedostokansio
# Ottaa huomioon riippuvuudet ajoympäristöstä ja käyttöjärjestelmästä
# Adapted from https://stackoverflow.com/a/48243694/22207980 on 2023-11-10

select_directory_method = function() {
  # Tries out a sequence of potential methods for selecting a directory to find one that works 
  # The fallback default method if nothing else works is to get user input from the console
  if (!exists('.dir.method')) {  # if we already established the best method, just use that
    # otherwise lets try out some options to find the best one that works here
    if (exists('utils::choose.dir')) {
      .dir.method = 'choose.dir'
    } else if (requireNamespace('rstudioapi')) {
      if (rstudioapi::isAvailable() & rstudioapi::getVersion() > '1.1.287') {
        .dir.method = 'RStudioAPI'
    }} else if (requireNamespace('tcltk') & 
              class(try({tt  <- tcltk::tktoplevel(); tcltk::tkdestroy(tt)}, silent = TRUE)) != "try-error") {
      .dir.method = 'tcltk'
    } else if (requireNamespace('gWidgets2') & requireNamespace('RGtk2')) {
      .dir.method = 'gWidgets2RGtk2'
    } else if (requireNamespace('rJava') & requireNamespace('rChoiceDialogs')) {
      .dir.method = 'rChoiceDialogs'
    } else {
      .dir.method = 'console'
    }
    assign('.dir.method', .dir.method, envir = .GlobalEnv) # remember the chosen method for later
  }
  return(.dir.method)
}

choose_directory = function(method = select_directory_method(), title = 'Select data directory') {
  switch (method,
          'choose.dir' = utils::choose.dir(caption = title),
          'RStudioAPI' = rstudioapi::selectDirectory(caption = title),
          'tcltk' = tcltk::tk_choose.dir(caption = title),
          'rChoiceDialogs' = rChoiceDialogs::rchoose.dir(caption = title),
          'gWidgets2RGtk2' = gWidgets2::gfile(type = 'selectdir', text = title),
          readline('Please enter directory path: ')
  )
}
