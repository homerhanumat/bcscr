#' Extract Portions from R Help Files for Use in Documents
#'
#' Extracts specified portions of R help files for use in Sweave or R-markdown
#' documents.
#'
#' The \code{type} argument accepts: \itemize{ \item \code{"m_code"}: For use
#' with markdown documents in instances where highlighted code is expected, for
#' example the "Usage" section. \item \code{"m_text"}: For use with markdown
#' documents in instances where regular text is expected, for example the
#' "Description" section. \item \code{"s_code"}: For use with Sweave documents
#' in instances where highlighted code is expected, for example the "Usage"
#' section. \item \code{"s_text"}: For use with Sweave documents in instances
#' where regular text is expected, for example the "Description" section. } To
#' insert a chunk into a markdown document, use something like:
#'
#' \verb{```{r, echo=FALSE, results='asis'}} \verb{cat(helpExtract(cor), sep =
#' "\n")} \verb{```}
#'
#' To insert a chunk into a Sweave document, use something like:
#'
#' \verb{\Sexpr{knit_child(textConnection(helpExtract(cor, type = "s_code")),
#' options = list(tidy = FALSE, eval = FALSE))}}
#'
#' @param Function The function that you are extracting the help file from.
#' @param section The section you want to extract. Defaults to \code{"Usage"}.
#' @param type The type of character vector you want returned. Defaults to
#' \code{"m_code"}. See \emph{Details}
#' @param \dots Other arguments passed to \code{getHelpFile}.
#' @return A character vector to be used in a Sweave or R-markdown document.
#' @author Ananda Mahto (\url{https://github.com/mrdwab/SOfun})
#' @note Reproduced here with helper functions defined from R source,
#' so there are no ::: calls.
#' @export
#'
helpExtract <- function(Function, section = "Usage", type = "m_code", ...) {
  A <- deparse(substitute(Function))
  x <- capture.output(tools::Rd2txt(.getHelpFile(utils::help(A, ...)),
                                    options = list(sectionIndent = 0)))
  B <- grep("^_", x)                    ## section start lines
  x <- gsub("_\b", "", x, fixed = TRUE) ## remove "_\b"
  X <- rep(FALSE, length(x))
  X[B] <- 1
  out <- split(x, cumsum(X))
  out <- out[[which(sapply(out, function(x)
    grepl(section, x[1], fixed = TRUE)))]][-c(1, 2)]
  while(TRUE) {
    out <- out[-length(out)]
    if (out[length(out)] != "") { break }
  }

  switch(
    type,
    m_code = c("```r", out, "```"),
    s_code = c("<<>>=", out, "@"),
    m_text = paste("    ", out, collapse = "\n"),
    s_text = c("\\begin{verbatim}", out, "\\end{verbatim}"),
    stop("`type` must be either `m_code`, `s_code`, `m_text`, or `s_text`")
  )
}

.getHelpFile <- function(file)
{
  path <- dirname(file)
  dirpath <- dirname(path)
  if(!file.exists(dirpath))
    stop(gettextf("invalid %s argument", sQuote("file")), domain = NA)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if(!file.exists(paste(RdDB, "rdx", sep = ".")))
    stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", sQuote(pkgname)), domain = NA)
  fetchRdDB(RdDB, basename(file))
}

fetchRdDB <-
  function(filebase, key = NULL)
  {
    fun <- function(db) {
      vals <- db$vals
      vars <- db$vars
      datafile <- db$datafile
      compressed <- db$compressed
      envhook <- db$envhook

      fetch <- function(key)
        lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)

      if(length(key)) {
        if(! key %in% vars)
          stop(gettextf("No help on %s found in RdDB %s",
                        sQuote(key), sQuote(filebase)),
               domain = NA)
        fetch(key)
      } else {
        res <- lapply(vars, fetch)
        names(res) <- vars
        res
      }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key))
      res
    else
      invisible(res)
  }

