

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Hex encoding
#' 
#' @param bytes raw (or integer) vector with values in [0, 255]
#' @return hex encoding
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enc_hex <- function(bytes) {
  
  if (is.numeric(bytes)) {
    stopifnot(!anyNA(bytes) && all(bytes >= 0 & bytes <= 255))
    bytes <- as.raw(bytes)
  }
  
  # if (!is.raw(bytes)) {
  #   print(bytes)
  #   stop("Ugh:")
  # }
  stopifnot(is.raw(bytes))
  
  res <- bytes |>
    as.character() |> 
    paste0(collapse = "")
  
  paste0(res, ">")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Hex decoding - used for testing only
#' @param s string containing hex bytes
#' @return raw vector of values
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dec_hex <- function(s) {
  stopifnot(grepl(">$", s))
  cs <- strsplit(s, '')[[1]]
  # Remove last char. The end-of-data marker ">"
  cs <- cs[-length(cs)]
  
  mm <- c(0:9, letters[1:6])
  
  vals <- match(cs, mm) - 1L
  
  mat <- matrix(vals, ncol = 2, byrow = TRUE) 
  res <- mat[,1] * 16L + mat[,2]
  as.raw(res)  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Runlength encode
#' 
#' Encode raw byte data with Run Length Encoding
#' [controlbyte, sequence, controlbyte, sequence, end-of-data]
#' Control byte:
#'   0-127 indicates the following (n + 1) bytes should be copied verbatim
#'   129-255 The following 257-n bytes are all repeats of the next character
#'   128 is the end-of-data marker
#'   
#' @param rv raw, integer or numeric vector containing byte values in the
#'        range [0, 255]
#' @return raw vector of run-length encoded valeus as per PDF docs
#'         Sect 7.4.5. "RunLengthDecode filter"
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
enc_rle <- function(rv) {
  
  stopifnot(!anyNA(rv))
  stopifnot(all(rv >= 0) && all(rv <= 255))
  
  rv <- as.integer(rv)
  
  chunks <- chunk128(rv)
  chunks <- lapply(chunks, function(chunk) {
    if (length(unique(chunk)) == 1) {
      c(257 - length(chunk), chunk[1])
    } else {
      c(length(chunk) - 1, chunk)
    }
  })
  
  res <- unlist(chunks, use.names = FALSE)
  res <- c(res, 128) # End of data marker
  as.raw(res)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Split a vector into a list of vectors.  Each vector in the list should 
# be 128 elements, except for the last vector.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chunk128 <- function(x) {
  split(x, ceiling(seq_along(x)/128))
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Encode binary data into ASCII85-encoded string
#' @param rv raw,integer or numeric vector with values in range [0, 255]
#' @return ASCII85-encoded string
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
enc_ascii85 <- function(rv) {
  
  stopifnot(!anyNA(rv))
  stopifnot(all(rv >= 0) && all(rv <= 255))
  
  # Pad to a multiple of 4 characters
  pad <- 4 - (length(rv) %% 4)
  pad <- ifelse(pad == 4, 0, pad)
  rv <- c(rv, rep(0, pad))
  length(rv)
  
  # Calculate the value for each set of 4 bytes
  mat <- matrix(as.integer(rv), ncol = 4, byrow = TRUE)
  
  v <- 
    mat[,1] * 256^3 + 
    mat[,2] * 256^2 +
    mat[,3] * 256^1 +
    mat[,4] * 256^0
  
  # Calculate the base 85 representation
  cc <- matrix(0, nrow = 5, ncol = length(v))
  for (i in seq(5, 1)) {
    cc[i,] <- v %% 85
    v <- v %/% 85
  }
  
  # Keep track of special case where all values in a group-of-five are 0
  # This needs to be encoded as 'z', not '!!!!!'
  all_zeros <- which(colSums(cc) == 0)
  
  # If there is padding, then the last column does not get the z/!!!!! replacement
  if (pad > 0) {
    all_zeros <- setdiff(all_zeros, ncol(cc))
  }
  
  # Calculate the 5-letter groups for each 4-byte group 
  cc <- cc + utf8ToInt('!')
  five_letters <- apply(cc, 2, intToUtf8)
  
  # When all the bytes are zero, this should be replaced by 'z'
  five_letters[all_zeros] <- 'z'
  
  # Create the single long encoded string
  enc <- paste(five_letters, collapse = "")
  
  # truncate the string equivalent to the amount of padding
  # that was added
  enc <- substr(enc, 1, nchar(enc) - pad)
  
  # Add the end-of-data suffix string
  enc <- paste0(enc, "~>")
  enc
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decode a ASCII85-encoded character string into binary data
#' @param s string
#' @return raw vector
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dec_ascii85 <- function(s) {
  
  stopifnot(is.character(s) && length(s) == 1)
  
  # Check for end-of-data marker
  stopifnot(grepl("~>$", s))
  
  # Remove end-of-data marker
  dec <- substr(s, 1, nchar(s) - 2)
  
  # Every 'z' string should be a '!!!!!' sequence to represent a 
  # set of 5 zero values
  dec <- gsub("z", "!!!!!", dec)
  
  # Pad to bring length to multiple of 5
  # Use 'u' to ensure that decoded high bits are handled correctly
  pad <- 5 - (nchar(dec) %% 5)
  pad <- ifelse(pad == 5, 0, pad)
  dec <- paste0(dec, paste(rep("u", pad), collapse = ""))
  
  # Multipley out to get a value for each group of 5 letters
  cc <- matrix(utf8ToInt(dec) - utf8ToInt('!'), ncol = 5, byrow = TRUE)
  v <- 
    cc[, 1] * 85^4 + 
    cc[, 2] * 85^3 + 
    cc[, 3] * 85^2 + 
    cc[, 4] * 85^1 +
    cc[, 5] * 85^0
  
  # Calculate the 4 byte values for each 5-char values
  mat <- matrix(0, nrow = 4, ncol = length(v))
  for (i in seq(4, 1)) {
    mat[i, ] <- v %% 256
    v <- v %/% 256
  }
  
  # return a raw vector.
  dec <- as.raw(as.vector(mat))
  dec <- dec[1:(length(dec) - pad)]
  
  dec  
}







if (FALSE) {
  bytes <- seq(1:25)
  enc_hex(bytes) -> s
  s

  enc_hex(bytes) |> dec_hex()
  identical(as.raw(bytes), enc_hex(bytes) |> dec_hex())
}

