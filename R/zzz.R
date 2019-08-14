
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wrap all the r6 methods into the global environment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r6_to_funcs(PDFDocument, envir = topenv(),
            object_name = "pdfdoc",
            include = c(
              'add_text', 'add_rect', 'add_line', 'add_polyline', 'add_polygon',
              'add_circle', 'write_pdf', 'set_clip_rect', 'reset_clip_rect'),
            silent = 'write_pdf')
