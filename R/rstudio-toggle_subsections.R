# -------------------------------------------------------------------------#
# Subsection ----
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
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .... ", nsec, " "))
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
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# .. ", nsec, " "))
    }
  }
  
  # .. 4 Number sections -----
  if (length(s1)){
    ds1 <- data.table::data.table(s = s1)
    ds1[,`:=`(number = 1:.N - 1)]  
    for (i in 1:nrow(ds1)) {
      line <- ds1[i,s]
      nsec <- ds1[i,number]
      reg <- regexpr("# \\d* ?", text[line])
      rstudioapi::modifyRange(c(line, reg, line, reg + attr(reg, "match.length")), paste0("# ", nsec, " "))
    }
  }
  rstudioapi::documentSave(id = e$id)
  NULL
}


# -------------------------------------------------------------------------#
# Loopdebugger ----
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
extract_apply <- function(textline) {
  loopval <- trimws(gsub(".*apply\\((.+), *function.*", "\\1", textline))
  loopvar <- gsub(".*apply\\(.+, *function\\( *(\\w+) *\\).*", "\\1", textline)
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
# Function arguments ----
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
# Toggle mclapply/lapply ----
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
# Debugonce ----
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
# History ----
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
# BLABLA ----
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
# importFrom ----
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
# Function call ----
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
  for (x in args) codeToInsert <- gsub(x, paste0(x, " = ", x), codeToInsert)
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
# Insert dput ----
# -------------------------------------------------------------------------#

#' Execute selected text and insert its result as source code into your script
#' 
#' data.frames are inserted as tibble::tribble
#' 
#' @return
#' @export
#'
#' @examples
#' # Uncomment and try out
#' 1+1
#' a <- c(
#'   a= 1:3,
#'   b = list("a", "b", "c"),
#'   d = list("a", "b", "c")
#' )
#' wup <- data.frame(a = 1+1, b ="c")
insertDput <- function() {
  e <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id = e$id)
  
  row <- e$selection[[1]]$range$start[1]
  rowEnd <- e$selection[[1]]$range$end[1]
  documentText <- readLines(e$path)
  
  text <- e$selection[[1]]$text
  if (text == "") text <- findConnectedCode(documentText, row)
  
  textPasted <- paste0(text, collapse = "\n")
  variable <- ifelse(grepl("<-", textPasted),gsub("(.*)<-.*", "\\1", textPasted), "x")
  x <- eval(parse(text = paste0("{", paste0(text, collapse = "\n"), "}")))
  
  if (is.data.frame(x)) {
    rstudioapi::insertText(location = rstudioapi::document_position(rowEnd + 1, 1), text = "\n",id =  e$id)
    rstudioapi::setCursorPosition(position = rstudioapi::document_position(rowEnd + 1, 1), id = e$id)
    outputMdTable(x, NFLAGtribble = 2)
  } else {
    codeToInsert <- paste0(variable, " <- ", paste0(capture.output(dput(x)), collapse = "\n"), "\n")
    rstudioapi::insertText(location = rstudioapi::document_position(rowEnd + 1, 1), text = codeToInsert, e$id)
  }
  rstudioapi::documentSave(id = e$id)
}

#' Find rows which, taken together, are parseable code
#'
#' @param documentText 
#' @param row 
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
findConnectedCode <- function(documentText, row) {
  for (rowUp in rev(seq_len(row))) {
    for (rowDown in seq(rowUp, length(documentText))) {
      isParsed <- suppressMessages(suppressWarnings(
        try(parse(text = paste0(documentText[rowUp:rowDown], collapse = "\n")), silent = TRUE)))
      if (!inherits(isParsed, "try-error")) break
    }
    if (!inherits(isParsed, "try-error")) break
  }
  text <- documentText[rowUp:rowDown]
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



# -------------------------------------------------------------------------#
# Toggle roxy comments ----
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







# Exit ----
