# -------------------------------------------------------------------------#
# 0 Subsection ----
# -------------------------------------------------------------------------#

#' @rdname toggle
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
toggle_off <- function(line, text, editor) {
  d1 <- rstudioapi::document_position(line, nchar(text[line]) + 1)
  rstudioapi::modifyRange(d1, " #", editor$id)
  NULL
}

#' @rdname toggle
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
toggle_on <- function(line, text, editor) {
  d1 <- rstudioapi::document_position(line, nchar(text[line]) - 1)
  d2 <- rstudioapi::document_position(line, nchar(text[line]) + 1)
  r <- rstudioapi::document_range(d1,d2)
  rstudioapi::modifyRange(r, "", editor$id)
  NULL
}

#' Toggle sections
#'
#' In the document outline, toggle a section on or off by adding or removing a #
#'
#' @return called for side-effect
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
toggle <- function(dashes = "----") {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(e$id)
  e <- rstudioapi::getSourceEditorContext()
  text <- readLines(e$path)
  
  sec_on    <- grep(paste0("# (.*) ", dashes, "$"), text)
  sec_off   <- grep(paste0("# (.*) ", dashes, " #$"), text)
  
  if (length(sec_off)){
    lapply(sec_off, toggle_on, text = text, editor = e)
    return(NULL)
  } else {
    lapply(sec_on, toggle_off, text = text, editor = e)
    return(NULL)
  }
}

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
toggle_section <- function() toggle("----")

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
toggle_subsection <- function() toggle("-----")

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @rdname toggle
#' @export
toggle_subsubsection <- function() toggle("------")


#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
transform_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  ws <- nchar(regmatches(linetext, regexpr(" *", linetext)))
  if (grepl(" -----$", linetext)) {
    rstudioapi::insertText(c(line, Inf), "-")
    rstudioapi::insertText(c(line, ws+3), "..")
  } else if (grepl(" ------$", linetext)) {
    rstudioapi::modifyRange(c(line, nchar(linetext), line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+7), "# ..")
  }
  rstudioapi::documentSave(e$id)
  NULL
}

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#' @rdname toggle
initiate_or_delete_subsection <- function(line, text, editor) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  line <- e$selection[[1]]$range$start[[1]]
  text <- readLines(e$path)
  linetext <- text[line]
  ws <- nchar(regmatches(linetext, regexpr(" *", linetext)))
  if (grepl(" -{6}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-6, line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+8), "")
  } else if (grepl(" -{5}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-5, line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+6), "")
  } else if (grepl(" -{4}$", linetext)) {
    # Remove
    rstudioapi::modifyRange(c(line, nchar(linetext)-4, line, Inf), "")
    rstudioapi::modifyRange(c(line, ws+1, line, ws+3), "")
  } else if (grepl("^ *# ", linetext)) {
    # Turn comment into subsection
    rstudioapi::insertText(c(line, ws+3), ".. ")
    rstudioapi::insertText(c(line, Inf), " -----")
    rstudioapi::setCursorPosition(rstudioapi::document_position(line, 6), e$id)
  } else {
    # Turn code into subsection
    rstudioapi::insertText(c(line, ws+1), "# .. ")
    rstudioapi::insertText(c(line, Inf), " -----")
    rstudioapi::setCursorPosition(rstudioapi::document_position(line, 6), e$id)
  }
  rstudioapi::documentSave(e$id)
  NULL
}

#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @param FLAGfunctionAsSection Start subsections within functions with 1. Switch off if the function is just a small part of a bigger logic
#' @export
#' @rdname toggle
#' @importFrom data.table data.table
renumber_sections <- function(FLAGfunctionAsSection = FALSE) {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  
  # .. 1 Get lines -----
  s1all <- s1 <- grep("(?<!Exit) -{4}$", text, perl = TRUE)
  if (FLAGfunctionAsSection){
    functions <- grep("function(", text, fixed = TRUE)
    s1all <- c(s1all, functions)
  }
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  # .. 2  # Associate subsubs to subs -----
  if (length(s3)){
    ds3 <- data.table::data.table(s = s3)
    ds3[,`:=`(s2associated = which.min(s > s2) ), by = (1:nrow(ds3))]
    ds3[,`:=`(number = 1:.N), by = s2associated]
    for (i in 1:nrow(ds3)) {
      line <- ds3[i,s]
      nsec <- ds3[i,number]
      reg <- regexpr("# .... \\d* ?", text[line])
      rstudioapi::modifyRange(location = c(line, reg, line, reg + attr(reg, "match.length")), 
                              text = paste0("# .... ", nsec, " "), 
                              id = e$id)
    }
  }
  # .. 3  # Associate subs to s -----
  if (length(s2)){
    ds2 <- data.table::data.table(s = s2)
    ds2[,`:=`(s1associated = which.min(s > s1all) ), by = (1:nrow(ds2))]
    ds2[,`:=`(number = 1:.N), by = s1associated]
    for (i in 1:nrow(ds2)) {
      line <- ds2[i,s]
      nsec <- ds2[i,number]
      reg <- regexpr("# .. \\d* ?", text[line])
      rstudioapi::modifyRange(location = c(line, reg, line, reg + attr(reg, "match.length")),
                              text = paste0("# .. ", nsec, " "),
                              id = e$id)
    }
  }
  
  # .. 4 Number sections -----
  if (length(s1)){
    ds1 <- data.table::data.table(s = s1)
    ds1[,`:=`(number = 1:.N - 1)]  
    for (i in 1:nrow(ds1)) {
      line <- ds1[i,s]
      nsec <- ds1[i,number]
      reg <- regexpr("# (?!# )\\d* ?", text[line], perl = TRUE)
      rstudioapi::modifyRange(location = c(line, reg, line, reg + attr(reg, "match.length")), 
                              text = paste0("# ", nsec, " "),
                              id = e$id)
    }
  }
  rstudioapi::documentSave(id = e$id)
  NULL
}



#' Same as renumber_sections but with filename, not as addin
#' 
#' @export
#' 
renumber_sections_filename <- function(filename, FLAGfunctionAsSection = FALSE) {
  text <- readLines(filename)
  
  # .. 5 Get lines -----
  s1all <- s1 <- grep("(?<!Exit) -{4}$", text, perl = TRUE)
  if (FLAGfunctionAsSection){
    functions <- grep("function(", text, fixed = TRUE)
    s1all <- c(s1all, functions)
  }
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  # .. 6  # Associate subsubs to subs -----
  if (length(s3)){
    ds3 <- data.table::data.table(s = s3)
    ds3[,`:=`(s2associated = which.min(s > s2) ), by = (1:nrow(ds3))]
    ds3[,`:=`(number = 1:.N), by = s2associated]
    for (i in 1:nrow(ds3)) {
      line <- ds3[i,s]
      nsec <- ds3[i,number]
      text[line] <- gsub("# .... \\d* ?", paste0("# .... ", nsec, " "), text[line])
    }
  }
  # .. 7  # Associate subs to s -----
  if (length(s2)){
    ds2 <- data.table::data.table(s = s2)
    ds2[,`:=`(s1associated = which.min(s > s1all) ), by = (1:nrow(ds2))]
    ds2[,`:=`(number = 1:.N), by = s1associated]
    for (i in 1:nrow(ds2)) {
      line <- ds2[i,s]
      nsec <- ds2[i,number]
      text[line] <- gsub("# .. \\d* ?", paste0("# .. ", nsec, " "), text[line])
    }
  }
  
  # .. 8 Number sections -----
  if (length(s1)){
    ds1 <- data.table::data.table(s = s1)
    ds1[,`:=`(number = 1:.N - 1)]  
    for (i in 1:nrow(ds1)) {
      line <- ds1[i,s]
      nsec <- ds1[i,number]
      text[line] <- gsub("# \\d* ?", paste0("# ", nsec, " "), text[line])
    }
  }
  writeLines(text, filename)
}


# -------------------------------------------------------------------------#
# 1 Loopdebugger ----
# -------------------------------------------------------------------------#

#' @rdname extract_loopargs
#' @export
extract_for <- function(textline) {
  loopvar <- gsub(".*for ?\\((.+) in.*", "\\1", textline)
  loopval <- gsub(".*for ?\\(.+ in (.+)\\) ?\\{?.*", "\\1", textline)
  list(loopvar = loopvar, loopval = loopval)
}

#' @rdname extract_loopargs
#' @export
#' @examples
#' textline <- "simR <- do.call(rbind,lapply(seq_along(Tested9001Models),function(i_m){"
extract_apply <- function(textline) {
  loopval <- trimws(gsub(".*apply\\((.+), *(mc.cores.*)? *function.*", "\\1", textline))
  loopvar <- gsub(".*apply\\(.+, *(mc.cores.*)? *function\\( *(\\w+) *\\).*", "\\2", textline)
  list(loopvar = loopvar, loopval = loopval)
}

#' Get the arguments of a for loop or of lapply
#'
#' @param textline Line of code
#'
#' @return list(loopvar = "loopingvariable", loopval = "list:ofValues")
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
#'
#' @examples
#' extract_loopargs("lapply(names(alpha), function(x) 1)")
#' textline <- "simR <- do.call(rbind,lapply(seq_along(Tested9001Models),function(i_m){"
#' extract_loopargs(textline)
extract_loopargs <- function(textline) {
  if (grepl("apply\\(", textline)) return(extract_apply(textline))
  if (grepl("for ?\\(", textline)) return(extract_for(textline))
}


#' Insert the arguments of a loop into the script
#' 
#' for (a in 1:3) gets turned into
#' 
#' a <- (1:3)[[1]]
#' for (a in 1:3)
#' 
#' This is handy for developing and debugging a loop
#' 
#' 
#' @return NULL. Modifies the document
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
insert_loopdebugger <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  loopargs <- extract_loopargs(textline)
  
  newline <-   paste0(loopargs$loopvar, " <- (", loopargs$loopval, ")[[1]]\n")
  
  rstudioapi::insertText(location = rstudioapi::document_position(current_row, 1), newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
  
}



# -------------------------------------------------------------------------#
# 2 Function arguments ----
# -------------------------------------------------------------------------#

#' Fix commas in rearranged lists
#' 
#' E.g. turns code from 
#' 
#' list(a
#' b,
#' )
#' 
#' into 
#' 
#' list(a,
#' b
#' )
#' 
#' @export
fixCommas <- function() {
  
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row1 <- e$selection[[1]]$range$start[1]
  row2 <- e$selection[[1]]$range$end[1]
  text <- readLines(e$path)
  text <- text[row1:row2]
  if (trimws(text[length(text)]) != ")") stop("Please select text with the last closing bracket in its own line")
  text <- trimws(text, "right")
  text <- gsub(",$","", text)
  text[1:(length(text)-2)] <- paste0(text[1:(length(text)-2)], ",")
  
  newRange <- rstudioapi::document_range(rstudioapi::document_position(row1,1), rstudioapi::document_position(row2, Inf))
  rstudioapi::modifyRange(newRange, text = "", e$id)
  rstudioapi::insertText(rstudioapi::document_position(row1,1), paste0(text, collapse = "\n"), e$id)
  
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
  
}

#' Like extract variable, but this writes it into the function argument section
#'
#' @export
exposeAsArgument <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row1 <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  rowfunction <- max(grep("function\\(", text[1:row1]))
  rowfunctionArgClose <- min(grep("\\) \\{", text[rowfunction:row1])) + rowfunction - 1
  if (!length(rowfunctionArgClose) || rowfunctionArgClose < rowfunction) {
    stop("Function brackets ') {' should have space in between and should be in same line")
  }
  
  argName <- rstudioapi::showPrompt("Argument name", "Enter the argument name")
  
  argText = paste0(", ", argName, " = ", e$selection[[1]]$text)
  
  rstudioapi::insertText(location = rstudioapi::document_position(rowfunctionArgClose, regexpr("\\) \\{", text[rowfunctionArgClose])),
                         text = argText, id = e$id)
  rstudioapi::modifyRange(location = e$selection[[1]]$range,
                          text = argName, id = e$id)
  
  rstudioapi::documentSave(id = e$id)
  sink <- NULL
  
}

#' Title
#'
#' @param functionText 
#'
#' @return
#' @export
#'
#' @examples
#' a <- 1 # won't be found
#' b <- 2 # won't be found
#' functionText <- c("fun1<- function(",
#'           "a,",
#'           "b = c(1,a = 2),",
#'           "d = 2) {",
#'           "a^2",
#'           "if (a ==2) {",
#'           "b}",
#'           "}")
#' functionText <- c("fun <- function(a, b = 1, x = c(\"option1\", \"option2\"), y = NULL) {", 
#' "  # do stuff", "}")
getFormalValues <- function(functionText) {
  
  bodyIdx <- grep("{", functionText, fixed = TRUE)[1]
  textWithoutBody <- functionText
  textWithoutBody <- functionText[1:bodyIdx]
  textWithoutBody <- gsub("\\{.*", "", textWithoutBody)
  if (sum(stringr::str_count(textWithoutBody, "function")) > 1) 
    stop("keyword 'function' was detected more than once. This is not yet supported.")
  textWithoutBody <- gsub(".*function", "function", textWithoutBody)
  
  # Evaluate formals, get their names and code
  formalValuesText <- c(".formalFun <- ",
                        textWithoutBody,
                        "{",
                        "  fx <- formals()",
                        "  l <- lapply(setNames(nm = names(fx)), function(xx_______) tryCatch(eval(parse(text = xx_______)), error = function(e) NULL))",
                        "}",
                        ".formalFun()")
  formalValues <- eval(parse(text = formalValuesText))
  formalValues
  
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
assignFormals <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row <- e$selection[[1]]$range$start[1]
  documentText <- readLines(e$path)
  
  text <- findFunctionCode(documentText, row)
  formalValues <- getFormalValues(text)
  
  formalNames <- names(formalValues)
  formalNULLs <- vapply(formalValues, is.null, FUN.VALUE = TRUE)
  
  for (i in which(!formalNULLs)) {
    cat("Assigned ", formalNames[[i]], "\n")
    assign(formalNames[[i]], formalValues[[i]], .GlobalEnv)
  }
  cat("Not assigned formals (no default value):\n  ", 
      paste0(formalNames[formalNULLs], " <- NULL", collapse = "  ,\n"))
  
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
insertFormals <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row <- e$selection[[1]]$range$start[1]
  documentText <- readLines(e$path)
  
  text <- findFunctionCode(documentText, row)
  formalValues <- getFormalValues(text)
  
  formalNames <- names(formalValues)
  formalCode <- lapply(formalValues, function(x) capture.output(dput(x)))
  
  codeToInsert <- vapply(seq_along(formalNames), function(i) {
    paste0(formalNames[[i]], " <- ", paste0(formalCode[[i]], collapse = "\n"))
  }, FUN.VALUE = "assignment<-value")
  codeToInsert <- paste0(codeToInsert, collapse = "\n")
  codeToInsert <- paste0(codeToInsert, "\n")
  
  rowStart <- findFunctionStartRow(documentText, row)
  
  rstudioapi::insertText(location = rstudioapi::document_position(rowStart, 1), 
                         text = codeToInsert, e$id)
  rstudioapi::documentSave(id = e$id)
}





# -------------------------------------------------------------------------#
# 3 Toggle mclapply/lapply ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @param textline 
#'
#' @return
#' @export
#'
#' @examples
#' textline <- "fuck <- lapply(1:n, function(x) {"
#' toggle_mclapply_on(textline)
#' textline <- "fuck <- lapply(motherfuck[1:3], function(x) bla::fn(x))"
#' toggle_mclapply_on(textline)
#' 
toggle_mclapply_on <- function(textline) {
  loopargs <- extract_apply(textline = textline)
  newline <- textline
  newline <- paste0("ncores <- 4\n", newline)
  newline <- gsub("lapply", "parallel::mclapply", newline)
  newline <- gsub(loopargs$loopval, paste0("X = ", loopargs$loopval, ", mc.cores = ncores"), newline, fixed = TRUE)
  newline <- gsub("function", "FUN = function", newline)
  newline
}

#' Title
#'
#' @param textline 
#'
#' @return
#' @export
#'
#' @examples
#' textline <- "fuck <- mclapply(X = 1:n, mc.cores = ncores, FUN = function(x) {"
#' toggle_mclapply_off(textline)
toggle_mclapply_off <- function(textline) {
  newline <- textline
  newline <- gsub("parallel::mclapply", "lapply", newline)
  newline <- gsub("X = ", "", newline)
  newline <- gsub(", mc.cores = ncores", "", newline)
  newline <- gsub("FUN = ", "", newline)
  newline
}

#' Insert the arguments of a loop into the script
#' 
#' for (a in 1:3) gets turned into
#' 
#' a <- (1:3)[[1]]
#' for (a in 1:3)
#' 
#' This is handy for developing and debugging a loop
#' 
#' @return NULL. Modifies the document
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
toggle_mclapply <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  rng <- newline <- NULL
  if (grepl("mclapply", textline)) {
    newline <- toggle_mclapply_off(textline)
    del_line_above <- 0
    if (grepl("^ncores *<-", text[current_row-1])) del_line_above <- 1
    rng <- rstudioapi::document_range(
      rstudioapi::document_position(current_row - del_line_above, 1), 
      rstudioapi::document_position(current_row, Inf))
  } else {
    newline <- toggle_mclapply_on(textline)
    rng <- rstudioapi::document_range(
      rstudioapi::document_position(current_row, 1), 
      rstudioapi::document_position(current_row, Inf))
  }
  
  rstudioapi::modifyRange(location = rng, text = newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
}



# -------------------------------------------------------------------------#
# 4 Debugonce ----
# -------------------------------------------------------------------------#

#' Guess function name of interest
#'
#' @param textline character(1L), line of script with buggy function
#'
#' @return extracted function name
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#' @export
#'
#' @examples
#' textline <- "rng <- rstudioapi::document_range("
#' textline <- "rstudioapi::document_range("
#' textline <- "document_range(kjshdf = bla)"
guess_function <- function(textline) {
  gsub("(.*<- *)?(\\w+:+)?(\\w+)\\(.*", "\\2\\3", textline)
}

#' Insert debugonce(function)
#' 
#' rng <- rstudioapi::document_range(...)
#' 
#' gets turned into
#' 
#' debugonce(rstudioapi::document_range)
#' rng <- rstudioapi::document_range(...)
#' 
#' @return NULL. Modifies the document
#' @author Daniel Lill (daniel.lill@intiquan.com)
#' @md
#' @export
insert_debugonce <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  newline <- paste0("debugonce(", guess_function(textline), ")\n")
  rstudioapi::insertText(location = rstudioapi::document_position(current_row, 1), newline, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
  sink <- NULL
}

# -------------------------------------------------------------------------#
# 5 History ----
# -------------------------------------------------------------------------#


#' Insert parts of the history
#'
#' @return
#' @export
#'
#' @examples
insertHistory <- function() {
  fl <- file.path(rstudioapi::getActiveProject(),".Rhistory")
  savehistory(fl)
  
  historyContent <- readLines(fl)
  nLines <- as.numeric(rstudioapi::showPrompt("NRow of history", "How man rows from End?", 10))
  historyContent <- historyContent[length(historyContent) - ((nLines-1):0)]
  historyContent <- c("","# .. HISTORY -----", historyContent,"# .. HISTORY -----","")  
  historyContent <- paste0(historyContent, collapse = "\n")
  
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  rstudioapi::insertText(location = rstudioapi::document_position(current_row, 1), historyContent, id = e$id)
  rstudioapi::documentSave(id = e$id)
  
}




# -------------------------------------------------------------------------#
# 6 BLABLA ----
# -------------------------------------------------------------------------#

#' Toggle BLABLA so the documentwalker can extract functions
#' 
#' Problem: the parser from Rstudio's extract function cannot handle code such as
#' foo <- bar[1,]
#' or 
#' foo <- bar[,1]
#' 
#' Thus wee first need to insert BLABLA into the specific places
#' foo <- bar[1,BLABLA]
#' or 
#' foo <- bar[BLABLA,1]
#' 
#' Then we can extract the function
#' 
#' @details rstudioapi getSourceEditorContext documentSave setDocumentContents 
#' 
#' @return NULL: Modifies document
#' @export
toggle_blabla <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  
  if (!any(grepl("\"BLABLA\"", text))){
    text <- gsub("[,", "[\"BLABLA\",", text, fixed = TRUE)
    text <- gsub(",]", ",\"BLABLA\"]", text, fixed = TRUE)
  } else {
    text <- gsub("\"BLABLA\"", "", text, fixed = TRUE)
  }
  
  text <- paste0(text, collapse = "\n")
  text <- paste0(text, "\n")
  
  rstudioapi::setDocumentContents(text, id = e$id)
  rstudioapi::documentSave(id = e$id)
}


# -------------------------------------------------------------------------#
# 7 importFrom ----
# -------------------------------------------------------------------------#

#' Go through code and extract all package::function calls into roxy tags
#'
#' @return
#' @export
#'
#' @details rstudioapi getSourceEditorContext documentSave
#' 
#' @importFrom stringr str_extract_all
#' @importFrom data.table as.data.table rbindlist setnames
#'
#' @examples
extract_importFrom <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  text <- e$selection[[1]]$text
  text <- strsplit(text, "\n", fixed = TRUE)[[1]]
  text <- stringr::str_extract_all(text, "[a-zA-Z0-9._]++::[a-zA-Z0-9._]+")
  text <- unlist(text)
  text <- strsplit(text, "::", TRUE)
  text <- lapply(text, function(x) data.table::as.data.table(as.list(x)))
  text <- data.table::rbindlist(text)
  if (length(text)){
    data.table::setnames(text, c("pkg", "fun"))
    text <- text[,list(text = paste0("#' @importFrom ", unique(pkg), " ", paste0(unique(fun), collapse = " "))), by = "pkg"]
    text <- paste0(paste0(text$text, "\n"), collapse = "")
  } else text <- NULL
  
  # Add other stuff as well (hacky but can be cleaned in a breeze, just remove this row)
  text <- paste0("#' @md\n", 
                 "#' @family \n", 
                 text)
  
  # Insert into beginning of selection (preferably select up to @export)
  position_toInsert <- rstudioapi::as.document_position(c(e$selection[[1]]$range$start[1], 1))
  rstudioapi::insertText(location = position_toInsert,text = text, id = e$id)
  
  invisible(text)
}





# -------------------------------------------------------------------------#
# 8 Function call ----
# -------------------------------------------------------------------------#

#' Title
#'
#' Turn this: "refactor_functionCall <- function() {"
#' into this: "refactor_functionCall()"
#' 
#' @return
#' @export
#' @author Daniel Lill (daniel.lill@physik.uni-freiburg.de)
#' @md
#'
#' @examples
refactor_functionCall <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  nchar_current_row <- nchar(textline)
  textline <- gsub(" ?<- ?function| ?\\{", "", textline)
  
  # Insert into beginning of selection (preferably select up to @export)
  rng <- rstudioapi::document_range(rstudioapi::document_position(current_row,1), 
                                    rstudioapi::document_position(current_row,nchar_current_row+1))
  rstudioapi::modifyRange(location = rng, text= textline, id = e$id)
  
  invisible(text)
}



#' Turn fun(a, b) into fun(a = a, b = b)
#'
#' @return
#' @export
#'
#' @examples
#' textline <- "wup <- fun(a, b)"
#' textline <- "wup <- fun(aa, a, b)"
#' textline <- "wup <- aaa(aa, a, b)"
duplicateArguments <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  args <- gsub(".*\\(|\\)|\\{","",textline)
  args <- strsplit(args, ",")[[1]]
  args <- trimws(args)
  codeToInsert <- textline
  for (x in args) codeToInsert <- gsub(paste0("\\b", x,"\\b"), paste0(x, " = ", x), codeToInsert)
  nwhite <- nchar(gsub("\\(.*","", textline))
  codeToInsert <- gsub(",", paste0(",\n", paste0(rep(" ",nwhite), collapse = "")), codeToInsert)
  
  # Insert into beginning of selection (preferably select up to @export)
  rng <- rstudioapi::document_range(rstudioapi::document_position(current_row,1), 
                                    rstudioapi::document_position(current_row,Inf))
  rstudioapi::modifyRange(location = rng, text= "", id = e$id)
  rstudioapi::insertText(location = rstudioapi::document_position(current_row,1),
                         text = codeToInsert, id = e$id)
}






#' Swap the first two arguments of a function call
#' e.g setdiff(x,y) becomes setdiff(y,x)
#' @param text 
#'
#' @return
#' @export
#'
#' @examples
#' text <- 'setdiff(1:5, unique(c(1,2,6,7)))'
#' swapArg1Arg2(text)
swapArg1Arg2InText <- function(text) {
  parsedText <- parse(text = text)
  fun <- deparse(parsedText[[1]][[1]])
  arg1 <- deparse(parsedText[[1]][[2]])
  arg2 <- deparse(parsedText[[1]][[3]])
  
  if ((parsedText[[1]][4]) != "NULL()") 
    stop("Third argument not empty - not yet supported.")
  
  paste0(fun, "(", arg2, ",", arg1, ")")
}



#' Title
#'
#' e.g setdiff(x,y) becomes setdiff(y,x)
#' @return
#' @export
#'
#' @examples
#' # Uncomment and try
#' # setdiff(1:5, unique(c(1,2,6,7)))
swapArg1Arg2 <- function() {
  
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row <- e$selection[[1]]$range$start[1]
  documentText <- readLines(e$path)
  text <- findConnectedCode(documentText, row)
  rowBelow <- row + length(text)
  
  modifiedText <- swapArg1Arg2InText(text)
  modifiedText <- paste0(modifiedText, "\n")
  
  rstudioapi::insertText(rstudioapi::document_position(rowBelow, 1), text = modifiedText, id = e$id)
}


# -------------------------------------------------------------------------#
# 9 Insert dput ----
# -------------------------------------------------------------------------#

#' Execute selected text and insert its result as source code into your script
#' 
#' data.frames are inserted as tibble::tribble
#' 
#' @return
#' @export
#' @md
#' @importFrom rstudioapi getSourceEditorContext documentSave insertText document_position setCursorPosition document_range setSelectionRanges executeCommand
#' @importFrom stringr str_pad
#'
#' @examples
#' # Uncomment and try out
#' 1+1
#' x <- 1:10 + 0.1
#' x <- setNames(1:3 + 0.1,letters[1:3])
#' a <- list(
#'   a= 1:3,
#'   b = list("a", "b", "c"),
#'   d = list("a", "b", "c")
#' )
#' wup <- data.frame(a = 1+1, b ="c")
#' function(x) {bla}
insertDput <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  # current_range <- e$selection[[1]]$range
  
  row <- e$selection[[1]]$range$start[1]
  rowEnd <- e$selection[[1]]$range$end[1]
  documentText <- readLines(e$path)
  
  text <- e$selection[[1]]$text
  if (text == "") text <- findConnectedCode(documentText, row)
  
  textPasted <- paste0(text, collapse = "\n")
  variable <- ifelse(grepl("<-", textPasted),gsub("(.*)<-.*", "\\1", textPasted), "x")
  x <- eval(parse(text = paste0("{", paste0(text, collapse = "\n"), "}")))
  
  codeToInsert <- deparse2(x)
  codeToInsert <- paste0(c(codeToInsert, ""), collapse = "\n")
  
  rstudioapi::insertText(location = rstudioapi::document_position(rowEnd + 1, 1), text = codeToInsert, e$id)
  # Too annoying with the loss of focus, but if I find a solution one day it would be cool to reindent automatically
  # ranges <- rstudioapi::document_range(c(1, 0), c(Inf, Inf))
  # rstudioapi::setSelectionRanges(ranges, id = e$id)
  # rstudioapi::executeCommand("reindent")
  # rstudioapi::setSelectionRanges(current_range, id = e$id)
  rstudioapi::documentSave(id = e$id)
}

#' Find rows which, taken together, are parseable code
#'
#' @param documentText 
#' @param row 
#' @param FLAGreturnIdxs return indices instead of text 
#'
#' @return
#' @export
#'
#' @examples
#' documentText <- c(
#'   "a <- letters[",
#'   "1:3",
#'   "]",
#'   "b <- 1",
#'   "c(a,",
#'   "b)"
#' )
#' row <- 6
#' findConnectedCode(documentText, row)
#' for (i in 1:6) print(findConnectedCode(documentText, i))
findConnectedCode <- function(documentText, row, FLAGreturnIdxs = FALSE) {
  for (rowUp in rev(seq_len(row))) {
    for (rowDown in seq(rowUp, length(documentText))) {
      isParsed <- suppressMessages(suppressWarnings(
        try(parse(text = paste0(documentText[rowUp:max(row,rowDown)], collapse = "\n")), silent = TRUE)))
      if (!inherits(isParsed, "try-error")) break
    }
    if (!inherits(isParsed, "try-error")) break
  }
  
  if (FLAGreturnIdxs) return(rowUp:max(row,rowDown))
  
  text <- documentText[rowUp:max(row,rowDown)]
  text
}


#' Title
#'
#' @param row 
#' @param documentText 
#'
#' @return
#' @export
#'
#' @examples
findFunctionStartRow <- function(documentText, row) {
  for (rowFunDef in seq(row,1,-1)) {
    if (grepl("function *\\(", documentText[rowFunDef])) break
  }
  rowFunDef
}


#' Find rows which, taken together, are a function
#'
#' @param documentText 
#' @param row 
#'
#' @return
#' @export
#'
#' @examples
#' documentText <- c(
#'   "fun1<- function(",
#'   "a,",
#'   "b = 1) {a^2}",
#'   "# blabla",
#'   "fun2 <- function(",
#'   "a,",
#'   "b = 1,",
#'   "d = c(a,",
#'   "b)) {
#'  a + b
#'  }",
#'   "1+2",
#'   "fun3 = function(",
#'   "a,",
#'   "b = 1) {a^2}",
#'   "# blabla"
#' )
#' row <- 12
#' lapply(1:length(documentText), function(i) findFunctionCode(documentText, i))
findFunctionCode <- function(documentText, row) {
  
  rowFunDef <- findFunctionStartRow(documentText, row)
  
  text <- findConnectedCode(documentText,rowFunDef)
  if ((length(text) + rowFunDef) <= row) warning("Cursor is below the complete function definition.")
  
  text
}


#' Title
#'
#' @param rowStart start of function code, result from findFunctionStartRow()
#' @param documentText 
#'
#' @return indices of the roxy skeleton
#' @export
findRoxyIdxs <- function(rowStart, documentText) {
  roxyActive <- TRUE
  roxyIdxs <- c()
  lastRoxyIdx <- rowStart
  while (roxyActive == TRUE) {
    newRoxyIdx <- lastRoxyIdx - 1
    roxyFound <- grepl("^#'", documentText[newRoxyIdx])
    if (!roxyFound) break
    roxyIdxs <- c(roxyIdxs, newRoxyIdx)
    lastRoxyIdx <- newRoxyIdx
  }
  roxyIdxs <- sort(setdiff(roxyIdxs, rowStart))
  roxyIdxs
}



#' Featureful deparse: 
#' 
#' * data.frames are deparsed into tibble::tribble() calls with aligned columns
#' * vectors are dputted with one entry per row and aligned equal signs and commas
#' * Other objects are simply deparsed, with the shortest possible width.cutoff
#' 
#' @param x an object to deparse
#'
#' @return deparsed code, collapsed into length 1
#' @export
#'
#' @examples
#' # Uncomment and try out
#' library(magrittr)
#' deparse2(1+1) %>% cat(sep = "\n")
#' deparse2(1:10 + 0.1) %>% cat(sep = "\n")
#' deparse2(setNames(1:26 + 0.1, letters)[1:10]) %>% cat(sep = "\n")
#' x <- c(wup = "sadfm", "sdflk" = "asldkfjlas", a = "asssdf", sldkfjlabs = "df")
#' deparse2(x) %>% cat(sep = "\n")
#' deparse2(list(
#'   a= 1:3,
#'   b = list("a", "b", "c"),
#'   d = list("a", "b", "c")
#' )) %>% cat(sep = "\n")
#' deparse2(data.frame(a = 1+1, b = c("c", "sdafksdfl"))) %>% cat(sep = "\n")
#' deparse2(function(x) {bla}) %>% cat(sep = "\n")
deparse2 <- function(x) {
  if (is.data.frame(x)) {
    deparsedCode <- outputMdTable(x, NFLAGtribble = 1)
  } else if (is.vector(x) && !is.list(x)) {
    # Deparse elementary vectors one by one and output one element per row
    deparsedValues <- lapply(x, deparse)
    deparsedValues <- do.call(c, deparsedValues)
    deparsedValues <- stringr::str_pad(deparsedValues, max(nchar(deparsedValues)), side = "right")
    if (!is.null(names(x))) {
      deparsedNames <- lapply(names(x), deparse)
      deparsedNames <- do.call(c, deparsedNames)
      deparsedNames <- stringr::str_pad(deparsedNames, max(nchar(deparsedNames)), side = "right")
      deparsedValues <- paste0(deparsedNames, " = ", deparsedValues)
    }
    # Commas in beteween
    if (length(deparsedValues) > 1) {
      deparsedValues[-length(deparsedValues)] <- paste0(deparsedValues[-length(deparsedValues)], " ,")
    }
    deparsedValues <- paste0("  ", deparsedValues)
    deparsedCode <- c("c(", deparsedValues, ")")
  } else {
    deparsedCode <- deparse(x, width.cutoff = 20)
  }
  deparsedCode
}


# -------------------------------------------------------------------------#
# 10 Compress Multiline ----
# -------------------------------------------------------------------------#


#' Title
#'
#' @return
#' @export
#'
#' @examples
collapseMultilineCode <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  text <- e$selection[[1]]$text
  text <- strsplit(text, "\n")[[1]]
  text <- collapseMultilineCodeWorkhorse(text)
  
  text <- paste0(c(
    "",
    "# --- Collapsed multiline code pure ---- ",
    text,
    "",
    "# --- Collapsed multiline code. ---- ",
    "# For snippets, the following things are important: ", 
    "#  1. Wrap the expression into `r {expr}` by using the surrounding lines.",
    "#  2. the last expression must not be assigned, but evaluate to a single-length character",
    "#  3. It is recommended to keep multiline versions of the snippets' raw code as well.",
    "#  4. Dollars are currently escaped - either rectify manually in the snippets file or don't use them.",
    "#  5. ` marks the end of the R code - so avoid constructs like `:=`.",
    "#  5.1 a:=b will be converted to `:=` by the parser, so avoid as well. use mutate instead",
    "# Note: The collapsed code has all comments removed.",
    "",
    paste0("`r {",text,"}`"),
    "",
    "",
    "# --- End collapsed multiline code. ---- ",
    ""
  ), collapse = "\n")
  
  location <- rstudioapi::document_position(row = e$selection[[1]]$range$end[1] + 1, column = 1)
  rstudioapi::insertText(location = location, text = text, id = e$id)
}

#' Compress multiline code into single-line code
#'
#' @param codeToCompress Code given as character string. The code must be without error!
#'
#' @return Character(1L) with the code being compressed
#' @export
#'
#' @examples
#' codeToCompress <- c(
#'   "f <- function(x) {",
#'   "  paste0(x, c('bla',",
#'   "  'bla'),",
#'   "  'bla'",
#'   "  )",
#'   "}",
#'   "f('asdf')"
#' )
#' collapseMultilineCodeWorkhorse(codeToCompress)
collapseMultilineCodeWorkhorse <- function(codeToCompress) {
  
  # Check if code can be parsed
  isParsed <- suppressMessages(suppressWarnings(
    try(parse(text = codeToCompress), silent = TRUE)))
  if (inherits(isParsed, "try-error")) {stop("Code could not be parsed")}
  
  # Remove hashes
  if (any(grepl("#", codeToCompress))) {
    warning("Selected text contains hashes (#). Trying to remove comments by parsing and outputting, but it is not guaranteed to always work.")
    codeToCompress <- as.character(parse(text = codeToCompress))
    codeToCompress <- strsplit(codeToCompress, "\n")
    codeToCompress <- do.call(c, codeToCompress)
  }
  
  # Crunch ...
  counter <- 0
  while (length(codeToCompress) > 1 & !all(grepl(";$", codeToCompress))) {
    counter <- counter+1
    if (counter >= 100) break
    
    connectedLines <- lapply(seq_along(codeToCompress), function(i) {
      findConnectedCode(codeToCompress, i)
    })
    
    connectedLineIdxs <- lapply(seq_along(codeToCompress), function(i) {
      findConnectedCode(codeToCompress, i, FLAGreturnIdxs = TRUE)
    })
    
    
    # Single lines which can be parsed on their own
    oneLinerCandidates <- which(sapply(connectedLines, function(x) length(x) == 1))
    
    isTrueOneLiner <- sapply(oneLinerCandidates, function(x) {
      testCode <- codeToCompress
      testCode[x] <- paste0(testCode[x],";")
      isParsed <- suppressMessages(suppressWarnings(
        try(parse(text = testCode), silent = TRUE)))
      success <- !inherits(isParsed, "try-error")
      success
    })
    falseOneLiners <- oneLinerCandidates[!isTrueOneLiner] # Part of multiline list which cannot have semicolon in the end
    trueOneLiners <- oneLinerCandidates[isTrueOneLiner] # Tre oneliner which is not part of multiline list
    
    # Deal with trueOneLiners: Append semicolons if not present
    for (idx in trueOneLiners) {
      if (!grepl(";$", codeToCompress[idx])) codeToCompress[idx] <- paste0(codeToCompress[idx], ";")
    }
    
    # Find subsets and supersets of remaining connected lines
    # True oneliners were dealt with (they can be crunched now, because they have their semicolon), false oneliners are not of interest
    uniqueConnectedLineIdxs <- unique(connectedLineIdxs[-oneLinerCandidates])
    # Where in the list of connectedLineIdxs are the uniqueConnecteLineIdxs located? index of index vectors ...
    uniqueConnectedLineIdxsLocations <- setdiff(which(!duplicated(connectedLineIdxs)), oneLinerCandidates)
    
    if (length(uniqueConnectedLineIdxs) == 0) break
    
    subsetMatrix <- sapply(seq_along(uniqueConnectedLineIdxs), function(i) {
      sapply(seq_along(uniqueConnectedLineIdxs), function(j) {
        all(uniqueConnectedLineIdxs[[i]] %in% uniqueConnectedLineIdxs[[j]])
      })
    })
    subsetMatrix <- t(subsetMatrix) # i,j == TRUE means: j is a subset of i
    diag(subsetMatrix) <- FALSE # Remove the trivial relations
    
    isNoSuperset <- which(!apply(subsetMatrix, 2, any))
    isNoSuperset <- uniqueConnectedLineIdxsLocations[isNoSuperset] # Go back from the unique list to the full list of connectedLines
    
    # The ones which are no superset can be shrunk without semicolon
    i <- (isNoSuperset)[[1]]
    for (i in rev(sort(isNoSuperset))) { # Classic case of where you need to traverse the array backwards
      idx <- connectedLineIdxs[[i]]
      compressedCode <- paste0(codeToCompress[idx], collapse = " ")
      codeToCompress <- codeToCompress[-idx]
      codeToCompress <- append(codeToCompress, compressedCode, after = min(idx) - 1)
    }
    
  }
  
  # Finally, paste all guys together, as they all have semicolons now
  compressedCode <- paste0(codeToCompress, collapse = " ")
  
  compressedCode <- gsub(" +", " ", compressedCode)
  compressedCode
  
}




# -------------------------------------------------------------------------#
# 11 Toggle roxy comments ----
# -------------------------------------------------------------------------#



#' Switch between # and #' comments
#'
#' @return
#' @export
#'
#' @examples
#' text <- c(
#' "# bla",
#' "# bla",
#' "  # bla",
#' "  # bla"
#' )
#' 
#' text <- c(
#'   "#' bla",
#'   "#' bla",
#'   "  #' bla",
#'   "  #' bla"
#' )
toggle_roxyComments <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  text <- e$contents[seq(e$selection[[1]]$range$start[1], e$selection[[1]]$range$end[1])]
  isRoxy <- any(grepl("#'", text))
  pattern     <- ifelse(isRoxy, "^( *)#'", "^( *)#")
  replacement <- ifelse(isRoxy, "\\1#"   , "\\1#'")
  textNew <- sub(pattern = pattern, replacement = replacement, x = text)
  textNew <- paste0(textNew, collapse = "\n")
  rstudioapi::modifyRange(
    rstudioapi::document_range(
      rstudioapi::document_position(e$selection[[1]]$range$start[1], 1),
      rstudioapi::document_position(e$selection[[1]]$range$end[1], Inf)
    ), 
    text = textNew, id = e$id)
  rstudioapi::setSelectionRanges(list(e$selection[[1]]$range), id = e$id)
}

# -------------------------------------------------------------------------#
# 12 Turn into factor ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' dwup <- data.table::data.table(bla = c("wupwup", "bla"))
#' dwup[,`:=`(bla)]
turnIntoFactor <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  # current_range <- e$selection[[1]]$range # for reindent, but too annoying. if a solution is found, this can be reactivated.
  current_row <- e$selection[[1]]$range$start[1]
  text <- readLines(e$path)
  textline <- text[current_row]
  
  start <- e$selection[[1]]$range$start[2]
  end <- e$selection[[1]]$range$end[2]
  
  word <- guess_word(textline, start, end)
  factorCall <- paste0(word, " = factor(",word, ", unique(",word,"))")
  newLine <- gsub(word, factorCall, textline)
  
  # If the call happens within a data.table, give the second option: evaluated levels as plain text dputted in the script
  if (grepl("\\w+\\[,`:=`\\(\\w+\\)\\]", textline)) {
    evaledLevels <- textline
    evaledLevels <- gsub("`:=`\\(","unique(", evaledLevels)
    evaledLevels <- deparse2(eval(parse(text = evaledLevels)))
    evaledLevels <- paste0(evaledLevels, collapse = "\n")
    factorCall2 <- paste0(word, " = factor(", word, ", levels = ", evaledLevels, ")")
    newLine2 <- gsub(word, factorCall2, textline)
    newLine <- paste0(newLine,"\n", newLine2)
  }
  
  rstudioapi::modifyRange(location = rstudioapi::document_range(start = rstudioapi::document_position(current_row, 1),
                                                                end = rstudioapi::document_position(current_row, Inf)),
                          text = newLine,
                          id = e$id)
  # Too annoying with the loss of focus, but if I find a solution one day it would be cool
  # ranges <- rstudioapi::document_range(c(1, 0), c(Inf, Inf))
  # rstudioapi::setSelectionRanges(ranges, id = e$id)
  # rstudioapi::executeCommand("reindent")
  # rstudioapi::setSelectionRanges(current_range, id = e$id)
  rstudioapi::documentSave(id = e$id)
  sink <- NULL
}


#' Title
#'
#' @param textline 
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
guess_word <- function(textline, start, end) {
  
  textlineSpaces <- gsub("\\W", " ", textline)
  textlineSpacesSplit <- strsplit(textlineSpaces, "")[[1]]
  
  # Construct a vector of word Ids
  wordStartStop = c(0,diff(textlineSpacesSplit != " "))
  # Capture edge cases - starting with a word. Stopping with a word happens *after* the vector, so this does not need to be captured
  if (min(which(wordStartStop == -1)) < min(which(wordStartStop == 1))) wordStartStop[1] <- 1
  wordActive = cumsum(wordStartStop)
  wordIds <- wordActive
  counter = 0
  for (i in seq_along(wordStartStop)) {
    counter <- counter + (wordStartStop[i] == 1)
    wordIds[i] <- wordActive[i] * counter
  }
  
  # Initiate search start
  wordIdStart <- wordIds[start]
  wordIdEnd <- wordIds[end]
  
  if (wordIdStart != 0 & wordIdEnd != 0 & wordIdStart != wordIdEnd) {
    message("Please select a single word only")
    return(NA)
  }
  
  # Heuristic to find word
  wordId <- wordIdStart
  if (wordId == 0) wordId <- wordIdEnd
  if (wordId == 0 & start != 1) wordId <- wordIds[start - 1]
  # if (wordId == 0 & end != length(wordIds)) wordId <- wordIds[end + 1]
  if (wordId == 0) {
    message("Word could not be found. Please place your cursor on a word")
    return(NA)
  }
  
  # Construct word
  word <- paste0(textlineSpacesSplit[wordIds == wordId], collapse = "")
  word
  
}


# -------------------------------------------------------------------------#
# # 13 Dtify script ----
# # -------------------------------------------------------------------------#
# 
# #' replace bare function calls to data.table functions by data.table::funCall
# #' 
# #' Works on whole script
# #' 
# #' @return
# #' @export
# #'
# #' @examples
# dtify <- function() {
#   e <- rstudioapi::getSourceEditorContext()
#   rstudioapi::documentSave(id = e$id)
#   text <- readLines(e$path)
#   
#   dtFunctions <- c("as.data.table", "copy", "data.table", "dcast", "fread", 
#                    "fwrite", "melt", "nafill", "rbindlist",
#                    "setcolorder", "setindex", "setindexv", "setkey", 
#                    "setkeyv", "setnafill", "setnames", "setorder", "setorderv", 
#                    "setreordervec", "transpose")
#   idxNoComment <- grep("^ *#", text, invert = TRUE)
#   regex <- paste0(
#     "(?<!data\\.table::)", # Ensure no leading data.table
#     "(",
#     paste0(dtFunctions, collapse = "|"),
#     ")",
#     "(?=\\()", # Ensure function call
#     ""
#   )
#   text <- gsub(regex, "data.table::\\1", text, perl = TRUE)
#   text <- paste0(text, collapse = "\n")
#   
#   rstudioapi::setDocumentContents(text = text, id = e$id)
#   rstudioapi::documentSave(id = e$id)
#   
#   sink <- NULL
# }
# 
# 
# # -------------------------------------------------------------------------#
# 14 projectPath projectComment from section header ----
# -------------------------------------------------------------------------#

#' Add projectPath and projectComment automatically
#' 
#' Transform 
#' # -------------------------------------------------------------------------#
#' # 15 estimate CL ----
#' # -------------------------------------------------------------------------#
#' 
#' to 
#' # -------------------------------------------------------------------------#
#' # 16 estimate CL ----
#' # -------------------------------------------------------------------------#
#' projectPath <- file.path(.modelFolder, "MODEL01") 
#' projectComment <- "estimate CL"
#' 
#' @return
#' @export
#'
#' @examples
projectPathComment_fromSection <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  rowCursor <- e$selection[[1]]$range$start[1]
  documentText <- readLines(e$path)
  rowSection <- grep("^# \\d+ .* ----$", documentText)
  rowSection <- max(rowSection[rowSection <= rowCursor])
  sectionText <- documentText[rowSection]
  
  rowProjectPath <- rowSection + 2
  rowProjectComment <- rowSection + 3
  
  projectNumber <- as.numeric(gsub("# (\\d+) .*","\\1", sectionText))
  projectPathText <- sprintf("projectPath <- file.path(.modelFolder, \"MODEL_%02d\")", projectNumber)
  
  projectComment <- gsub("# \\d+ (.*) ----","\\1", sectionText)
  projectCommentText <- sprintf("projectComment <- \"%s\"", projectComment)
  
  # Insert or modify the text, depending on whether it is present
  if (grepl("projectPath <- ", documentText[rowProjectPath])) {
    rstudioapi::modifyRange(location = rstudioapi::document_range(
      start = rstudioapi::document_position(rowProjectPath, 1),
      end = rstudioapi::document_position(rowProjectPath, Inf)),
      text = projectPathText, e$id)
    
  } else {
    rstudioapi::insertText(location = rstudioapi::document_position(rowProjectPath, 1), text = 
                             paste0(projectPathText, "\n"),id =  e$id)
  }
  
  if (grepl("projectComment <- ", documentText[rowProjectComment])) {
    rstudioapi::modifyRange(location = rstudioapi::document_range(
      start = rstudioapi::document_position(rowProjectComment, 1),
      end = rstudioapi::document_position(rowProjectComment, Inf)),
      text = projectCommentText, e$id)
  } else {
    rstudioapi::insertText(location = rstudioapi::document_position(rowProjectComment, 1), text = 
                             paste0(projectCommentText, "\n"),id =  e$id)
  }
  
  rstudioapi::documentSave(id = e$id)
}


#' Add projectPath and projectComment automatically
#' 
#' Transform 
#' # -------------------------------------------------------------------------#
#' # 17 estimate CL ----
#' # -------------------------------------------------------------------------#
#' 
#' to 
#' # -------------------------------------------------------------------------#
#' # 18 estimate CL ----
#' # -------------------------------------------------------------------------#
#' projectPath <- file.path(.modelFolder, "MODEL01") 
#' projectComment <- "estimate CL"
#' 
#' @return
#' @export
#'
#' @examples
projectPathComment_allSections <- function() {
  
  RSAddins::renumber_sections()
  
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  message("Model sections are identified by their first line containing 'projectPath'")
  
  documentText <- readLines(e$path)
  sectionStartRows <- grep("^# \\d+ .* ----$", documentText)
  isModelSection <- sapply(sectionStartRows, function(x) {grepl("projectPath", documentText[x+2])})
  sectionStartRows <- sectionStartRows[isModelSection]
  
  for (rowSection in sectionStartRows){
    sectionText <- documentText[rowSection]
    
    rowProjectPath <- rowSection + 2
    rowProjectComment <- rowSection + 3
    
    projectNumber <- as.numeric(gsub("# (\\d+) .*","\\1", sectionText))
    projectPathText <- sprintf("projectPath <- file.path(.modelFolder, \"MODEL_%02d\")", projectNumber)
    
    projectComment <- gsub("# \\d+ (.*) ----","\\1", sectionText)
    projectCommentText <- sprintf("projectComment <- \"%s\"", projectComment)
    
    # Modify the text in the two rows below section header
    rstudioapi::modifyRange(location = rstudioapi::document_range(
      start = rstudioapi::document_position(rowProjectPath, 1),
      end = rstudioapi::document_position(rowProjectPath, Inf)),
      text = projectPathText, e$id)
    
    rstudioapi::modifyRange(location = rstudioapi::document_range(
      start = rstudioapi::document_position(rowProjectComment, 1),
      end = rstudioapi::document_position(rowProjectComment, Inf)),
      text = projectCommentText, e$id)
    
    rstudioapi::documentSave(id = e$id)
  }
}


# -------------------------------------------------------------------------#
# 19 flplot from section header ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @param text 
#'
#' @return
#' @export
#'
#' @examples
parseSectionTable <- function(text) {
  # Get all sections
  s1 <- grep(" -{4}$", text, perl = TRUE)
  s2 <- grep(" -{5}$", text)
  s3 <- grep(" -{6}$", text)
  
  ds1 <- if (length(s1)) data.table::data.table(line = s1, level = 1) else NULL
  ds2 <- if (length(s2)) data.table::data.table(line = s2, level = 2) else NULL
  ds3 <- if (length(s3)) data.table::data.table(line = s3, level = 3) else NULL
  ds <- data.table::rbindlist(list(ds1,ds2,ds3), use.names = TRUE)
  ds <- ds[order(line)]
  ds[,`:=`(s1 = 0)]
  ds[level == 1,`:=`(s1 = 1)]
  ds[,`:=`(s1 = cumsum(s1) - 1)] # sections start at 0, for historical reasons
  ds[,`:=`(s2 = 0)]
  ds[level == 2,`:=`(s2 = 1)]
  ds[,`:=`(s2 = cumsum(s2)), by = c("s1")]
  ds[,`:=`(s3 = 0)]
  ds[level == 3,`:=`(s3 = 1)]
  ds[,`:=`(s3 = cumsum(s3)), by = c("s1", "s2")]
  
  data.table::copy(ds) # update print behaviour
}


#' In text, search for regex in the range of line:lineEnd and return the global index in text
#'
#' @param text 
#' @param line 
#' @param lineEnd 
#' @param regex 
#'
#' @return index as it would be returned by grep
#' @export
#'
#' @examples
#' text <- c("pattern but out of range","asdf","pattern", "sadlkf")
#' grepInUncommentedRange(pattern = "pattern", x = text, start = 2, end = 3)
#' grepInUncommentedRange(pattern = "pattern", x = paste0("# ", text), start = 2, end = 3)
grepInUncommentedRange <- function(pattern = " *flplot *<- *", x, start, end) {
  isCommentedOut <- all(grepl("^ *#", x[start:end]))
  idxInRange <- grep(pattern, x[start:end])
  # Ignore sections if they are fully commented-out
  if (length(idxInRange) == 0) {
    NA_real_
  } else if (isCommentedOut || length(idxInRange) > 1) {
    if (length(idxInRange) > 1) {
      message("Spotted multiple plot files in one (sub/subsub)-section. This section will be unchanged.")
    }
    NA_real_
  } else {
    start + idxInRange - 1
  }
}

#' Work horse to rename files and update the file paths in all reports
#'
#' @param ds 
#' @param plotOrTab 
#'
#' @return
#' @export
#'
#' @examples
renameFilesInPlaceAndInReport <- function(ds, text, plotOrTab = c("plot", "tab"), .outputFolder) {
  plotOrTab <- match.arg(plotOrTab)
  
  dsRename <- data.table::copy(ds)
  if (plotOrTab == "plot") {
    dsRename[,`:=`(oldFl = gsub(" *flplot *<- *","", text[lineFlplot]))]
    dsRename[,`:=`(newFl = paste0("file.path(.outputFolder, \"", fl, ".pdf\")"))]
    dsRename <- dsRename[!is.na(lineFlplot)]
  } else if (plotOrTab == "tab") {
    dsRename[,`:=`(oldFl = gsub(" *fltab *<- *","", text[lineFltab]))]
    dsRename[,`:=`(newFl = paste0("file.path(.outputFolder, \"", fl, ".txt\")"))]
    dsRename <- dsRename[!is.na(lineFltab)]
  } 
  # Remove dupes (don't rename when you don't know which of the two files to rename)
  dsRenameDupes <- dsRename[duplicated(oldFl)]
  dsRename <- dsRename[!dsRenameDupes, on = "oldFl"]
  if (nrow(dsRename) ==0) {return()}
  dsRename[,`:=`(oldFlParsed = tryCatch(eval(parse(text = oldFl)), error = function(x) NA_character_)), by = 1:nrow(dsRename)]
  dsRename[,`:=`(newFlParsed = tryCatch(eval(parse(text = newFl)), error = function(x) NA_character_)), by = 1:nrow(dsRename)]
  # Only rename files which exists and which are different from before
  dsRename <- dsRename[!is.na(oldFlParsed)]
  if (nrow(dsRename) ==0) {return()}
  dsRename <- dsRename[file.exists(oldFlParsed)]
  if (nrow(dsRename) ==0) {return()}
  dsRename <- dsRename[oldFlParsed != newFlParsed]
  if (nrow(dsRename) ==0) {return()}
  
  # Construct file paths which are relative to project
  eat <- function(x) {gsub("[^/]+/\\.\\./","",gsub("/+","/", x))}
  dsRename[,`:=`(oldFlWork = eat(gsub(here::here(), "", eat(file.path(getwd(), oldFlParsed)))))]
  dsRename[,`:=`(oldFlWork = gsub("/", "/+", oldFlWork))] # turn oldFlWork into a regex which can contain multiple / 
  dsRename[,`:=`(oldFlWork = gsub(".", "\\.", oldFlWork, fixed = TRUE))] # turn oldFlWork into a regex which can contain multiple / 
  dsRename[,`:=`(newFlWork = gsub(here::here(), "", eat(file.path(getwd(), newFlParsed))))]
  
  # Rename the files
  for (i in seq_len(nrow(dsRename))) {
    file.rename(dsRename[i,oldFlParsed], dsRename[i,newFlParsed])
  }
  
  # Prepare to walk through report
  reportFiles <- list.files(here::here("Report"), pattern = ".rmd$", full.names = TRUE, recursive = TRUE)
  for (flx in reportFiles) {
    textReport <- readLines(flx)
    for (i in seq_len(nrow(dsRename))) {
      textToReplace <- grep(dsRename[i,oldFlWork], textReport)
      if (length(textToReplace)) {message("Replacing ", dsRename[i,oldFlWork], " in ", gsub(here::here(), "", flx))}
      textReport <- gsub(dsRename[i,oldFlWork], dsRename[i,newFlWork], textReport)
    }  
    writeLines(textReport, flx)
  }
}



#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
flplotFromSectionHeader_allSections <- function() {
  try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
  .outputFolder <- file.path("../Output", gsub("SCRIPT_|.R$", "", basename(rstudioapi::getSourceEditorContext()$path)))
  
  # Update this, so it is in sync
  RSAddins::renumber_sections()
  
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  
  ds <- parseSectionTable(text)
  
  # Construct flplot name
  # regexplanation: 
  # * Take care of indent
  # * There can be multiple #'s due to commenting of full section
  # * Take care of dots for subsections and subsubsections
  # * Strip dashes at the end of the sections
  ds[,`:=`(fl = trimws(gsub("^ *(# *)+\\.* *\\d+| *-*$", "", text[line])))]
  ds[,`:=`(fl = gsub(" ", "_", fl))]
  # Disallow special characters. Potentially unsafe regex, but ok for this purpose
  ds[,`:=`(fl = gsub("[,(){};/\\\\'\"]", "", fl))]
  ds[,`:=`(fl = gsub("\\[|\\]", "", fl))]
  # Apply heuristics to shorten filename
  ds[,`:=`(fl = gsub("_-_", "-", fl))] # many redundant spaces
  ds[,`:=`(fl = gsub("__", "_", fl))]  # many redundant spaces
  # Assemble filename  
  ds[,`:=`(fl = sprintf(paste0("%0", max(s1) %/% 10, "d",
                               "%0", max(s2) %/% 10, "d",
                               "%0", max(s3) %/% 10, "d",
                               "_%s"),s1,s2,s3,fl))]
  
  
  # Identify subsections with flplot and fltab
  ds[,`:=`(lineEnd = c(line[-1] - 1,length(text)))]
  ds[,`:=`(lineFlplot = {grepInUncommentedRange(pattern = " *flplot *<- *", x = text, start = line, end = lineEnd)}), by = "line"]
  ds[,`:=`(lineFltab = {grepInUncommentedRange(pattern = " *fltab *<- *", x = text, start = line, end = lineEnd)}), by = "line"]
  
  # Potentially rename filenames
  renameFilesInPlaceAndInReport(ds = ds, text = text, plotOrTab = "plot", .outputFolder = .outputFolder)
  renameFilesInPlaceAndInReport(ds = ds, text = text, plotOrTab = "tab" , .outputFolder = .outputFolder)
  
  # Insert new filename
  i <- (rev(seq_len(nrow(ds))))[[1]]
  for (i in rev(seq_len(nrow(ds)))) {
    if (!is.na(ds[i,lineFlplot])){
      line_rm <- ds[i,lineFlplot]
      text_replace <- ds[i,paste0("flplot <- file.path(.outputFolder, \"", fl, ".pdf\")")]
      text[line_rm] <- text_replace
    }
    if (!is.na(ds[i,lineFltab])){
      line_rm <- ds[i,lineFltab]
      text_replace <- ds[i,paste0("fltab <- file.path(.outputFolder, \"", fl, ".txt\")")]
      text[line_rm] <- text_replace
    }
  }
  rstudioapi::setDocumentContents(paste0(c(text, ""), collapse = "\n"), e$id)
  
  # Return nothing
  invisible()  
}




# -------------------------------------------------------------------------#
# 21 Section handling ----
# -------------------------------------------------------------------------#
# .. 1 Comment/uncomment section -----

#' Comment/uncomment a whole section in one key stroke
#'
#' @return
#' @export
#' @md
#' @family 
#' @importFrom rstudioapi getSourceEditorContext documentSave document_range document_position modifyRange
#'
#' @examples
toggleCommentForWholeSection <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  currentRow <- e$selection[[1]]$range$start[1]
  
  ds <- parseSectionTable(text)
  idx <- min(which(currentRow - ds$line <= 0)) - 1
  rowStart <- ds$line[idx]
  rowEnd <- ds$line[idx + 1] - 1
  
  textRange <- text[seq(rowStart, rowEnd)]
  if (all(grepl("^# ?",textRange))) {
    textRange <- gsub("^# ?", "", textRange)
  } else {
    textRange <- paste0("# ", textRange)
  }
  textRange <- paste0(c(textRange), collapse = "\n")
  
  range <- rstudioapi::document_range(
    start = rstudioapi::document_position(rowStart, 1),
    end = rstudioapi::document_position(rowEnd, Inf)
  )
  
  rstudioapi::modifyRange(location = range, text = textRange, id = e$id)
  
}

# .. 2 Duplicate section -----

#' Duplicate a whole section in one key stroke
#'
#' @return
#' @export
#' @md
#' @family 
#' @importFrom rstudioapi getSourceEditorContext documentSave document_range document_position modifyRange
#'
#' @examples
duplicateSection <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  currentRow <- e$selection[[1]]$range$start[1]
  
  ds <- parseSectionTable(text)
  idx <- min(which(currentRow - ds$line <= 0)) - 1
  rowStart <- ds$line[idx]
  rowEnd <- ds$line[idx + 1] - 1
  
  textRange <- text[seq(rowStart, rowEnd)]
  # Handle edge cases with overflow
  if (grepl("# -------------------------------------------------------------------------#",textRange[2]) & !grepl("# -------------------------------------------------------------------------#", textRange[length(textRange)])) {
    textRange <- c(textRange[2], textRange)
  } else if (!grepl("# -------------------------------------------------------------------------#",textRange[2]) & grepl("# -------------------------------------------------------------------------#", textRange[length(textRange)])) {
    textRange <- textRange[-length(textRange)]
  }
  textRange <- paste0(paste0(c(textRange), collapse = "\n"), "\n")
  
  rstudioapi::insertText(location = rstudioapi::document_position(rowStart, 1), text = textRange, id = e$id)
}



# .. 3 Delete section -----

#' Delete a whole section in one key stroke
#'
#' @return
#' @export
#' @md
#' @family 
#' @importFrom rstudioapi getSourceEditorContext documentSave document_range document_position modifyRange
#'
#' @examples
deleteSection <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  text <- readLines(e$path)
  currentRow <- e$selection[[1]]$range$start[1]
  
  ds <- parseSectionTable(text)
  idx <- min(which(currentRow - ds$line <= 0)) - 1
  rowStart <- ds$line[idx]
  rowEnd <- ds$line[idx + 1] - 1
  
  textRange <- text[seq(rowStart, rowEnd)]
  # Handle edge cases with overflow. Simply assume that if textRange[2] contains the section markup, that this markup will also be above rowStart
  if ( grepl("# -------------------------------------------------------------------------#",textRange[2]) & !grepl("# -------------------------------------------------------------------------#", textRange[length(textRange)])
  ) {
    rowStart <- rowStart - 1
  } else if (!grepl("# -------------------------------------------------------------------------#",textRange[2]) & grepl("# -------------------------------------------------------------------------#", textRange[length(textRange)])) {
    rowEnd <- rowEnd - 1
  }
  
  range <- rstudioapi::document_range(
    start = rstudioapi::document_position(rowStart, 1),
    end = rstudioapi::document_position(rowEnd + 1, 1)
  )
  
  rstudioapi::modifyRange(location = range, text = "", id = e$id)
}

# -------------------------------------------------------------------------#
# Extract resource ----
# -------------------------------------------------------------------------#

#' Title
#'
#' @return
#' @export
#'
#' @examples
extractResource <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row <- e$selection[[1]]$range$start[1]
  documentText <- readLines(e$path)
  
  rowStart <- findFunctionStartRow(documentText, row)
  
  roxyIdxs <- findRoxyIdxs(rowStart, documentText)
  roxyText <- documentText[roxyIdxs]
  
  funText <- findFunctionCode(documentText, row)
  text <- c(roxyText, funText)
  
  funName <- trimws(gsub("<- *function.*","", funText[1]))
  
  # Export to Resources
  dir.create("Resources", FALSE)
  fl <- file.path("Resources", paste0(funName, ".R"))
  writeLines(c(text, ""), fl)
  message(fl, " was created or overwritten.")
  
  # Insert source call
  codeToInsert <- paste0("\nsource(\"Resources/", funName, ".R\")\n")
  
  rstudioapi::insertText(location = rstudioapi::document_position(min(c(rowStart, roxyIdxs)) - 1, 1), 
                         text = codeToInsert, e$id)
  rstudioapi::documentSave(id = e$id)
}


# 22 [ ] >>>> Continue here / Todolist <<<<<<<<<<< ----
# 
# [ ] Sequential shortcuts. ctrl+k ctrl+k
# [ ] Sequential shortcuts. ctrl+k ctrl+u
# [ ] Sequential shortcuts, ctrl+k ctrl+v verbose script
# 
# [ ] evinceLastPlot. Extract last pdf file name from history to flplot and call evince
#     re-use lines from history() for this.
# 
# [x] toggle betwee Multiline, single line
#   list(a,
#        b, 
#        c)
#   list(a, b, c)
# 
# [ ] quote the arguments
#   list(a, b, c)
#   list("a", "b", "c")
# 
# [-] Remove a magrittr pipeline
#   a %>% fun
#   fun(a)
#
# [ ] safer exposeAsArgument which writes to the correct function
#
# [ ] Toggle between for and lapply
#   for (x in y) {do stuff}
#   lapply(y, function(x) {do stuff})
# 
# [ ] Toggle verbose script on or off
#   Line # .. bla ----- ###
#   gets the following line directly below it:
#   cat(".. bla ------", "\n")
#   Can also apply to (interactive) tags, etc
# 
##
# text <- 'result <- fun(arg1 = argval1, arg2, c(arg3.1, arg.32))'
# parsedText <- parse(text = text)
# length(parsedText[[1]])
# length(parsedText[[1]][[3]])
# 
# One problem: Formals vanish
# text <- 'result <- fun(arg1 = argval1, arg2, c(arg3.1, arg.32))'
# parsedText <- parse(text = text)
# deparse(parsedText[[1]]) # OK
# deparse(parsedText[[1]][[3]]) # OK
# deparse(parsedText[[1]][[3]][[2]]) # FAIL
# as.list(parsedText[[1]][[3]]) # Maybe this is a way
# Read more tricks here https://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html#Language-objects
# 
# 
# 
# 23 [ ] >>>> // Continue here <<<<<<<<<<< ----



# Exit ----
