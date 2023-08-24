
# .. Collapse multiline executable code ----- #


# ..  ----- #
# devtools::load_all()
library(dplyr)
library(data.table)
codeToCompress <- c(
  "f <- function(x) {",
  "  paste0(x, c('bla',",
  "  'bla'),",
  "  'bla'",
  "  )",
  "}",
  "f('asdf')"
)

codeToCompress0 <- codeToCompress



# --- Collapsed multiline code. ---- 
# For snippets, wrap the expression into `r {expr}` by using the surrounding lines.

`r {
codeToCompress <- c( "f <- function(x) {", " paste0(x, c('bla',", " 'bla'),", " 'bla'", " )", "}", "f('asdf')" ); codeToCompress0 <- codeToCompress; 
}`

# --- End collapsed multiline code. ---- 




