# railtrail----
railtrail <- mosaicData::RailTrail

n <- nrow(railtrail)
season <- character(n)
for ( i in 1:n ) {
  if ( railtrail$spring[i] ) {
    season[i] <- "spring"
  } else if ( railtrail$summer[i] ) {
    season[i] <- "summer"
  } else if ( railtrail$fall[i] ) {
    season[i] <- "fall"
  }
}
railtrail$season <- factor(season,
                           levels = c("spring", "summer", "fall"))
railtrail$spring <- railtrail$summer <- railtrail$fall <- NULL
save(railtrail, file = "data/railtrail.rda")

# For ozLookup ----
library(tidyverse)
oz <- readLines(con = "downloads/oz.txt")
# a helper function
findIndex <- function(pattern, text) {
  str_detect(text, pattern = pattern) %>%
    which()
}

# now find lines to start and end at:
firstLine <-
  findIndex(
    "^\\*\\*\\* START OF THIS PROJECT GUTENBERG",
    oz
  ) + 1
lastLine <-
  findIndex(
    "^End of Project Gutenberg's",
    oz
  ) - 1

# trim oz to the desired text:
oz2 <- oz[firstLine:lastLine]
ozwds <-
  oz2 %>%
  str_split(
    pattern = "(?x)    # allow comments
              (-{2,})  # two or more hyphens
              |        # or
              (\\s+)   # whitespace
              "
  ) %>%
  unlist()
ozWords <-
  ozwds %>%
  # strip leading punctuation:
  str_replace(
    pattern = "^\\p{P}+",
    replacement = ""
  ) %>%
  # strip trailing punctuation:
  str_replace(
    pattern = "\\p{P}+$",
    replacement = ""
  ) %>%
  str_to_lower() %>%
  unique() %>%
  str_sort()
isNumber <- str_detect(ozWords, pattern = "^\\d+")
isEmpty <- ozWords == ""
validWord <- !isNumber & !isEmpty
ozWords <- ozWords[validWord]


indexFactory <- function(lexicon, fn) {
  index <- list()
  fileLines <- readLines(con = fn)
  for (i in seq_len(length(lexicon))) {
    word <- lexicon[i]
    pattern <- str_c("(?i)\\b", word, "\\b")
    hasWord <- str_detect(fileLines, pattern = pattern)
    index[[word]] <- which(hasWord)
  }
  index
}


ozIndex <- indexFactory(ozWords, "downloads/oz.txt")

usethis::use_data(oz, ozWords, ozIndex, internal = TRUE, overwrite = TRUE)
