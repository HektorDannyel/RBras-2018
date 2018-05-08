kid <- setClass(
  "kid",
  
  slots = c(
    album = "logical",
    collection = "numeric"
  ),
  
  prototype = list(
    album = logical(10),
    collection = numeric(10)
  ),
  
  validity = function(object){
    if(!is.logical(object@album)){
      return("kid@album is not logical.")
    }
    else if(!is.numeric(object@collection)){
      return("kid@collection is not numeric.")
    }
    else if(!length(object@album) == length(object@collection)){
      return("Album and collection must be of same length.")
    }
  }
)

bi <- setClass(
  "bi",
  
  contains = "kid"
)

uni <- setClass(
  "uni",
  
  contains = "kid"
)

create_universe <- function(kids_total = 10, album_size = 100, kid_class = "bi"){
  if(kid_class = "bi"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), bi(album = logical(album_size), collection = numeric(album_size)), envir = .GlobalEnv)
    }
  }  
  kids <- eval(parse(text = paste0("list(", paste0("kid", 1:kids_total, collapse = ", "), ")")))
  assign("kid_class", c("kids", kid_class), envir = .GlobalEnv)
  assign("album_size", album_size, envir = .GlobalEnv)
}