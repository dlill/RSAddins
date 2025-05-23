

#' Output table as markdown
#'
#' @param dt data.table
#' @param split_by Charactor of colnames to split the table by inserting separator lines
#' @param filename 
#'
#' @return object of class knitr::kable
#' 
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#' @importFrom knitr kable
#' @importFrom data.table data.table copy as.data.table
#' @importFrom tibble tribble
#' @importFrom stringr str_pad
#'
#' @examples
#' tableName <- "bla"
#' dt <- data.table(iris)[1:5]
#' outputMdTable(dt)
#' outputMdTable(data.table(iris), split_by = "Species")
#' split_by <- NULL
#' filename <- NULL
#' format <- c("markdown", "pandoc")
#' caption <- NULL
#' na.strings <- "NA"
#' FLAGsummaryRow <- TRUE
#' NFLAGtribble <- 2
outputMdTable <- function(dt, split_by = NULL, filename = NULL, format = c("markdown", "pandoc"), 
                          caption = NULL, na.strings = ifelse(NFLAGtribble == 2, "NA", "-"), FLAGsummaryRow = TRUE, 
                          NFLAGtribble = 0,
                          ...) {
  tableName <- as.character(substitute(dt))
  
  options(knitr.kable.NA = na.strings)
  
  # kt <- knitr::kable(dt,format = format[1], caption = caption)
  kt <- knitr::kable(dt,format = format[1], caption = caption, ...)
  
  seprow <- gsub(":","-",kt[3 + 2*(!is.null(caption))])
  widths <- nchar(strsplit(seprow, "|", fixed = TRUE)[[1]][-1])
  
  if (NFLAGtribble) {
    
    # Add quotes for strings and factos
    types <- vapply(dt, class, "double")
    for (fact in names(types)[types=="factor"]) dt[[fact]] <- as.character(dt[[fact]])
    hasDoubleQuote <- vapply(names(types)[types%in%c("character","factor")], function(nm) any(grepl('"', dt[[nm]])), FUN.VALUE = TRUE)
    hasSingleQuote <- vapply(names(types)[types%in%c("character","factor")], function(nm) any(grepl("'", dt[[nm]])), FUN.VALUE = TRUE)
    if (any(hasDoubleQuote & hasSingleQuote)) 
      warning("This table has double quotes (\") AND single quotes ('). Using (\"), but the output must be checked manually.")
    quoteSymbol <- ifelse(any(hasDoubleQuote), "'", '"')
    for (fact in names(types)[types%in%c("character","factor")]) dt[[fact]] <- paste0(quoteSymbol, dt[[fact]], quoteSymbol)
    
    # Handle widths: Account for quotes
    widths[types%in%c("character","factor")] <- widths[types%in%c("character","factor")] + 2 # increase widths by the quote symbols that were added manually
    # Handle widths: Account for whitespace padding in the first column which will be removed upon autoindent
    hasWhiteSpacePaddingRow1Cell1 <- sub("|","", kt[3], fixed = TRUE)
    hasWhiteSpacePaddingRow1Cell1 <- hasWhiteSpacePaddingRow1Cell1 != trimws(hasWhiteSpacePaddingRow1Cell1)
    if (hasWhiteSpacePaddingRow1Cell1) {
      npad <- sub("|","", kt[3], fixed = TRUE)
      npad <- gsub("( *)\\w.*", "\\1", npad)
      npad <- nchar(npad)
      widths[1] <- widths[1] - npad
    }
    
    kt <- knitr::kable(dt,format = "markdown")
    kt <- kt[-(1:2)]
    row0 <- "data.table::data.table("
    row0 <- paste0(row0, "tibble::tribble(")
    row1 <- paste0(paste0(stringr::str_pad(paste0("~", names(dt)), width = widths, side = "right"), " , "), collapse = "")
    kt <- substr(kt, 2, nchar(kt))
    kt <- gsub("\\|", " , ", kt)
    rowN <- kt[length(kt)]
    rowN <- substr(rowN, 1, nchar(rowN)-3)
    rowN <- paste0(rowN, "))")
    kt <- kt[-length(kt)]
    kt <- c(row0, row1, kt, rowN, "")
    
    # if (NFLAGtribble == 1) cat(kt, sep = "\n")
    if (NFLAGtribble == 2) {
      e <- rstudioapi::getSourceEditorContext()
      rstudioapi::documentSave(e$id)
      rstudioapi::insertText(e$selection[[1]]$range$start, paste0(kt, collapse = "\n"))
    }
    return(invisible(kt))
  }
  
  if (!is.null(split_by)){
    dt <- data.table::copy(data.table::as.data.table(dt))
    dn <- dt[,list(nlines = .N), by = split_by]
    dn[,`:=`(rowid = cumsum(nlines))]
    dn[,`:=`(rowid = rowid + 2 + 2*(!is.null(caption)))]
    atr <- attributes(kt)
    for (i in rev(dn$rowid)[-1]) {
      kt <- c(kt[1:i], seprow, kt[(i+1):length(kt)])
    }
    attributes(kt) <- atr
  }
  
  if (FLAGsummaryRow) {
    summaryrow <- vapply(dt, function(x) {
      if (is.numeric(x))   return(paste0("Sum=", sum(x)))
      if (is.character(x)) return(paste0("Lvl=", length(unique(x))))
    }, FUN.VALUE = "N=nunique")
    summaryrow <- vapply(seq_along(summaryrow), function(i) sprintf(paste0("%",widths[i],"s"), summaryrow[i]), "bla")
    summaryrow <- paste0("|", paste0(summaryrow, collapse = "|"), "|")
    kt <- c(kt, seprow, summaryrow)
  }
  
  if(!is.null(filename))
    writeLines(kt, filename)
  
  cat(kt, sep = "\n")
  invisible(kt)
}


#' Title
#'
#' @param dt 
#'
#' @returns
#' @export
#'
#' @examples
outputMdTable2 <- function(dt) {
  
  dt <- lapply(dt, function(x) {
    if (!is.character(x) && !is.factor(x)) {
      x <- signif(x, digits = 6)
    }
    x
  })
  
  deparsedTable <- lapply(setNames(nm = names(dt)), function(nm) {
    # content  
    x <-     deparse2(dt[[nm]])
    x <- x[-c(1,length(x))]
    if (nm != tail(names(dt),1)) {
      x[length(x)] <- paste0(x[length(x)], " ,")
    } else {
      x[length(x)] <- paste0(x[length(x)], "  ")
    }
    # header
    y <- paste0("~",nm)
    if (nm == head(nm,1)) {
      y <- paste0("  ", y)
    }
    
    # Align header and content
    widthx <- max(nchar(x))
    widthy <- max(nchar(y)) + 2
    width <- max(c(widthx, widthy))
    x <- stringr::str_pad(x, width, "left")
    y <- paste0(stringr::str_pad(y, width - 2, "right"), " ,")
    out <- c(y, x)
    
    # trim some whitespace if NA_real_ -> NA creates whitespace
    out <- gsub("NA_real_", "NA      ", out)
    nc <- max(nchar(out))
    while(all(substr(out, nc-2,nc) == "  ,")) {
      out <- paste0(substr(out, 1, nc-2), ",")
      nc <- max(nchar(out))
    }
    # Whitespace at the left end of the string
    while(all(substr(out, 1,2) == "  ")) {
      out <- substr(out, 2, max(nchar(out)))
    }
    out
  })
  
  textToInsert <- paste0(c("data.table::data.table(tibble::tribble(",
                           do.call(paste, deparsedTable),
                           "))"), collapse = "\n")
  textToInsert
}

