#' Title
#'
#' @return
#' @export
#'
#' @examples
printRSShortcuts <- function(FLAGdefensive = TRUE) {
  fl <- ifelse(FLAGdefensive, 
                 system.file("setup_rstudio/keybindings_defensive/addins.json", package = "RSAddins"),
                 system.file("setup_rstudio/keybindings/addins.json", package = "RSAddins"))
  
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
installRSkeybindings <- function(FLAGoverwrite = FALSE, FLAGdefensive = TRUE){
  keybindings_paths <- c("~/.R/rstudio/keybindings", "~/.config/rstudio/keybindings")
  for (keybindings_path in keybindings_paths) {
    if (!dir.exists(keybindings_path)) dir.create(keybindings_path, FALSE, TRUE)
    path <- ifelse(FLAGdefensive, 
                   system.file("setup_rstudio/keybindings_defensive", package = "RSAddins"),
                   system.file("setup_rstudio/keybindings", package = "RSAddins"))
    keybindings_files <- list.files(path, "json$", F, T)
    wup <- vapply(stats::setNames(nm = keybindings_files), file.copy, to = keybindings_path, overwrite = FLAGoverwrite, FUN.VALUE = TRUE)
  }
  if (!any(wup)) cat("No keybindings were installed. Do you need to set FLAGoverwrite to TRUE?")
  if (any(wup)) cat(paste0(names(wup)[wup], collapse = " .... \n"),  "\nkeybindings installed\n")
  invisible()
}

