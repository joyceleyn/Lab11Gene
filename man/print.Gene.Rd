\name{print.Gene}
\alias{print.Gene}
\title{
print-Gene
}
\description{
This function prings out the Gene ID, Gene Symbol, and length of the gene in nucleotides and Amino acids~~
}
\usage{
\method{print}{Gene}(object, ...)
}

\arguments{
  \item{object}{
The list from the Gene function
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
prints to the screen the gene name, ID and Length of CDS
}

\author{
Joyce Njuguna
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----

## The function is currently defined as
print.gene(object, ...) 
{
    cat(paste("Gene Id:", object$ID), paste("Symbol:", object$symbol), 
        paste("Length in bp:", nchar(object$CDS)), paste("length in AA:", 
            proteinLength(object)), sep = "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
