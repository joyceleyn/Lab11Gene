\name{print.Gene}
\alias{print.Gene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to pring gene information
}
\description{
this method prints gene information
}
\usage{
\method{print}{Gene}(geneobject, ...)
}
\arguments{
  \item{object}{
A gene object produced by the gene function
}
  \item{\dots}{
other arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the information is printed to the standart output
}


\examples{
my_object <- Gene("gene00001", "XYZ", c(8020, 112, 900),
               "ATGAAATAG")
print(my_object) 

}
