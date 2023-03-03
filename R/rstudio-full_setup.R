#' Title
#'
#' @return
#' @export
#'
#' @examples
printRSShortcuts <- function() {
  fl <- system.file("setup_rstudio/keybindings/addins.json", package = "RSAddins")
  text <- readLines(fl)
  text <- text[-c(1, length(text))]
  text <- gsub('"',"", text)
  text <- gsub(',',"", text)
  text <- gsub('RSAddins::',"", text)
  text <- trimws(text)
  text <- strsplit(text, " : ", T)
  text <- lapply(text, function(x) data.table(Function = x[1], Shortcut = x[2]))
  text <- rbindlist(text)
  outputMdTable(text)
}





#' Install keybindings
#'
#' @export
#' @importFrom stats setNames
installRSkeybindings <- function(FLAGoverwrite = FALSE){
  keybindings_paths <- c("~/.R/rstudio/keybindings", "~/.config/rstudio/keybindings")
  for (keybindings_path in keybindings_paths) {
    if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
    keybindings_files <- list.files(system.file("setup_rstudio/keybindings", package = "RSAddins"), "json$", F, T)
    wup <- vapply(stats::setNames(nm = keybindings_files), file.copy, to = keybindings_path, overwrite = FLAGoverwrite, FUN.VALUE = TRUE)
  }
  if (any(wup)) cat(paste0(names(wup)[wup], collapse = " .... \n"),  "\nkeybindings installed\n")
  NULL
}

