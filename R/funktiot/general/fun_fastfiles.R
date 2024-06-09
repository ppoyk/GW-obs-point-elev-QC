# Function to return paths of all recursive items inside a folder.
# Faster than list.files() with recursive for large number of items.
# Use list.files if non-recursive search is wanted

fastfiles <- function(startdir, 
                      pattern = NULL, filename_pattern = NULL,
                      filesonly = NULL) {
  
  if (!is.null(pattern) && !is.null(filename_pattern))
    stop("Give a maximum of one pattern")
  
  # Append slash if supplied folder path not ending in slash
  if (stringr::str_sub(startdir, -1) != "/") startdir <- paste0(startdir,"/")
  
  
  if (!is.null(pattern))
    args <- paste0('-regex "',pattern,'"') # Match entire file path string w/ regex
  if (!is.null(filename_pattern))
    args <- paste0('-name "',pattern,'"') # Match file name (not regex)(needs quotes)
  if (is.null(c(pattern,filename_pattern)))
    args <- NULL  # Leave empty if no pattern argument given (lists all from dir)
  
  if (isTRUE(filesonly)) 
    args <- paste(args, '-type f') # Append arg to match only files
  
  
  files <- shell(paste('find -L',startdir, args), intern = T)
  
  return(files)
}
