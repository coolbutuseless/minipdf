

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Hex encoding
#' 
#' @param bytes raw (or integer) vector with values in [0, 255]
#' @return hex encoding
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enc_hex <- function(bytes) {
  
  if (is.integer(bytes)) {
    stopifnot(!anyNA(bytes) && all(bytes >= 0 & bytes <= 255))
    bytes <- as.raw(bytes)
  }
  stopifnot(is.raw(bytes))
  
  bytes |>
    as.character() |> 
    paste0(collapse = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Hex decoding - used for testing only
#' @param s string containing hex bytes
#' @return raw vector of values
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dec_hex <- function(s) {
  cs <- strsplit(s, '')[[1]]
  mm <- c(0:9, letters[1:6])
  
  vals <- match(cs, mm) - 1L
  
  mat <- matrix(vals, ncol = 2, byrow = TRUE) 
  res <- mat[,1] * 16L + mat[,2]
  as.raw(res)  
}


if (FALSE) {
  bytes <- seq(1:25)
  enc_hex(bytes) -> s
  s

  enc_hex(bytes) |> dec_hex()
  identical(as.raw(bytes), enc_hex(bytes) |> dec_hex())
}

