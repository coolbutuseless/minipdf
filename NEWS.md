
# minipdf 0.2.2

* draw state parameters are now set via their own methods e.g. `obj$fill('black')`, 
  however using `update()` still works e.g. `obj$update(fill = 'black')`

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
* The `stream` data structure can be used to build R6 `PDFStream` objects of the
  required sub-class e.g. `stream$rect()` will help create a `PDFRect` object.
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
