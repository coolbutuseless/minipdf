

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Dictionary object creator
#'
#' @import R6
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDict <- R6::R6Class(
  "PDFDict",

  public = list(
    dict = NULL,

    initialize = function(...) {
      varargs <- list(...)
      if (!all_named(varargs)) {
        stop("PDFDict$new(): All arguments must be named")
      }
      self$dict <- varargs
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update values in the dictionary. Each named named argument to this
    # function is considered a name/value pair to include in the dictionary.
    # Unnamed arguments are silently dropped
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update = function(...) {
      self$dict <- modifyList(self$dict, list(...))
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Character representation of this dict and all sub dictionaries.
    # @return single character string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(depth = 0) {
      indent1 <- create_indent(depth)
      indent2 <- create_indent(depth + 1)


      contents <- c()
      for (i in seq_along(self$dict)) {
        name <- names(self$dict[i])
        item <- self$dict[[i]]
        item_content <- as.character(item, depth = depth + 1L)
        item_content <- glue("{indent2}/{name} {item_content}")
        contents <- c(contents, item_content)
      }

      contents <- paste(contents, collapse = "\n")

      paste0("<<\n", contents, "\n", indent1, ">>")
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function() {
      cat(self$as_character(), "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert this dict into a full object representation for inclusion in
    # a PDFDocument
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_object = function(obj_idx) {
      glue("{obj_idx} 0 obj\n{self$as_character()}\nendobj")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Since PDFDict objects can contain other PDFDict objects in a list
    # need to do a deep copy to properly clone see private/deep_clone()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone(deep = TRUE)
    }
  ),

  private = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # When called with `$clone(deep = TRUE)`, the 'deep_clone' function is
    # called for every name/value pair in the object.
    # See: https://r6.r-lib.org/articles/Introduction.html
    # Need special handling for:
    #   - 'dict' as it a list of R6 objects or perhaps character values.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    deep_clone = function(name, value) {
      if (name == 'dict') {
        lapply(value, function(x) {if (inherits(x, "R6")) x$clone(deep = TRUE) else x})
      } else {
        value
      }
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrapper to help create a PDFDict object
#'
#' @param ... arguments passed to \code{PDFDict$new()}
#'
#' @return PDFDict R6 object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dict <- function(...) {
  PDFDict$new(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method for converting PDFDict to character
#'
#' @param x object
#' @param ... unused
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.PDFDict <- function(x, ...) {
  x$as_character(...)
}

