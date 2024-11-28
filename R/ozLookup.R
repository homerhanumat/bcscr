#' @title Look Up Words in the Wizard of Oz

#' @description Look up words in the Wizard of Oz.
#' This is primarily a utility-function for use of webR in the book.
#'
#' @rdname ozLookup
#' @usage ozLookup(word)
#' @param word a character vecotr of length 1, the word to be looked up
#' @return side effects
#' @export
#' @author Homer White \email{homerhanumat@gmail.com}
#' @examples
#' ozLookup(word = "humbug")
ozLookup <- function(word) {

  file <- oz
  lexicon <- ozWords
  index <- ozIndex

  if (!(word %in% lexicon)) {
    message <- paste0("\"", word, "\" is not in the lexicon!\n")
    return(cat(message))
  }
  matchLines <- index[[word]]
  number <- length(matchLines)
  cat(
    "There are ", number,
    "lines that contain your request.\n\n"
  )
  hrule <- rep("-", times = 30)
  for (i in 1:number) {
    lineNum <- matchLines[i]
    cat(hrule, "\n")
    cat(lineNum, ":  ", file[lineNum], "\n")
  }
}
