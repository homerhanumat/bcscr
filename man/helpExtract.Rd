% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpExtract.R
\name{helpExtract}
\alias{helpExtract}
\title{Extract Portions from R Help Files for Use in Documents}
\usage{
helpExtract(Function, section = "Usage", type = "m_code", ...)
}
\arguments{
\item{Function}{The function that you are extracting the help file from.}

\item{section}{The section you want to extract. Defaults to \code{"Usage"}.}

\item{type}{The type of character vector you want returned. Defaults to
\code{"m_code"}. See \emph{Details}}

\item{\dots}{Other arguments passed to \code{getHelpFile}.}
}
\value{
A character vector to be used in a Sweave or R-markdown document.
}
\description{
Extracts specified portions of R help files for use in Sweave or R-markdown
documents.
}
\details{
The \code{type} argument accepts: \itemize{ \item \code{"m_code"}: For use
with markdown documents in instances where highlighted code is expected, for
example the "Usage" section. \item \code{"m_text"}: For use with markdown
documents in instances where regular text is expected, for example the
"Description" section. \item \code{"s_code"}: For use with Sweave documents
in instances where highlighted code is expected, for example the "Usage"
section. \item \code{"s_text"}: For use with Sweave documents in instances
where regular text is expected, for example the "Description" section. } To
insert a chunk into a markdown document, use something like:

\verb{```{r, echo=FALSE, results='asis'}} \verb{cat(helpExtract(cor), sep =
"\n")} \verb{```}

To insert a chunk into a Sweave document, use something like:

\verb{\Sexpr{knit_child(textConnection(helpExtract(cor, type = "s_code")),
options = list(tidy = FALSE, eval = FALSE))}}
}
\note{
Reproduced here with helper functions defined from R source,
so there are no ::: calls.
}
\author{
Ananda Mahto (\url{https://github.com/mrdwab/SOfun})
}
