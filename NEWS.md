

# minipdf 0.2.5.9001 2025-08-11

* Replace object-oriented R6 approach with pipe-able approach
* Add support for
    * multi-page documents
    * per-text font settings
    * global and local transforms

* Introduction of pipe-able PDF document creation.

# minipdf 0.2.5 (The Updatening)

* Update roxygen to latest
* Use roxygen for R6 documentation 
* Remove dependency on 'glue'
* Remove Travis CI and replace with Github Actions

# minipdf 0.2.4

* Add a `$show()` method to `PDFDocument`.  In Rstudio, this will launch a PDF
  app to view the current document.

# minipdf 0.2.3

* Curtailed the accessability to global clipping path as it is cumulative and not
resettable within a document making it a bit of a sledghammer.
* Instead use `clip_path()` or `clip_rect()` on specific objects
* Renamed the `stream` helper to `ptag` to be more reminiscent of the `tag` helper
  from `shiny`

# minipdf 0.2.2

* draw state parameters are now set via their own methods e.g. `obj$fill('black')`, 
  however using `update()` still works e.g. `obj$update(fill = 'black')`
* refactored internal `attrib` handling.

# minipdf 0.2.1

* transformations are now methods directly on the PDFStream object rather than attributes
    * e.g. `obj <- PDFRect$new(x=0, y=0, width=100, height=100)$translate(0, 100)`
* renaming of some methods in `PDFDocument` 
    * e.g. `add_rect()` has become just `rect`
    * `add()` is now `append()`
    
    
# minipdf 0.2.0 
    
* Building a document is now much more structured - the main objects 
within a PDF now classifed as either `Dictionary` or `Stream` objects 
(with R6 objects to represent both).
* Since R6 objects are used throughout to represent PDF stream/dict objects, we
  can make use of R6's reference semantics to update individual nodes within a document *in-situ* i.e.
  without having to recreate the document from scratch.
* The `ptag` data structure can be used to build R6 `PDFStream` objects of the
  required sub-class e.g. `ptag()` will help create a `PDFRect` object.
* The `dict` function can be used to build R6 `PDFDict` objects.
* Alternately, the `PDFDocument` class includes helper methods (e.g. `add_rect()`) which
  will create a stream of dict object of the correct type and add it to the document.
* Text output is formatted much more nicely with better use of indentation and line breaks.
* The tidy-friendly helper functions have been removed.  This was partly due to the namespace pollution of having
all these functions exported from the package, but also due to the finnicky way
the R6 methods were exported as package functions.
* All these changes mean that updating and extending `minipdf` will be much easier than before.

# minipdf 0.1.0

* Initial release.
