# Constructor function for the 'Gene' class
## Needs to be fixed as described in Lab 11 ##
Gene <- function(ID, symbol, ontology, CDS){
  if(length(ID) > 1) stop("Only provide one ID")
  if(length(symbol) > 1) stop("Only provide one symbol")
  if(length(CDS) > 1) stop("Only provide one CDS")
  if(!is.character(ID)) stop("ID must be a character string")
  if(!is.character(symbol)) stop("symbol must be a character string")
  if(!is.numeric(ontology)) stop("ontology must be a vector of GO numbers")
  if(!is.character(CDS)) stop("CDS must be a character string")
  
  # clean out line breaks from CDS and check that it is DNA sequence
  CDS <- gsub("\n", "", CDS)
  if(nchar(CDS) %% 3 != 0){
    stop("Length of protein coding sequence must be a multiple of three.")
  }
  if(!all(strsplit(CDS, "")[[1]] %in% c("A", "C", "G", "T"))){
    stop("CDS must be DNA sequence.")
  }
  
  out <- list(ID = ID, symbol = symbol, ontology = as.integer(ontology),
              CDS = CDS)
  class(out) <- c("Gene", class(out))
  return(out)
}

#function to find protein length
proteinLength <- function(object, ...){
  UseMethod("proteinLength", object)
}
proteinLength.Gene <- function(object, ...){
  amino_acid_len <- nchar(object$CDS)/3
  return(amino_acid_len)
}

#print some information from the Gene object

print.Gene <- function(object, ...){
  cat(paste("Gene Id:", object$ID ),
      paste("Symbol:",object$symbol),
      paste("Length in bp:",nchar(object$CDS)),
      paste("length in AA:",proteinLength(object)),
      sep = "\n")
}

