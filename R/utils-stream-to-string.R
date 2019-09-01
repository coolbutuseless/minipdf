

create_linetype_spec <- function(attrib) {
  linetype <- attrib$linetype %||% 0
  switch(
    linetype + 1,
    NULL,              # 0 = blank
    NULL       ,       # 1 = solid
    "[3] 0 d",         # 2 = dashed
    "[1 3] 0 d",       # 3 = dotted
    "[1 1 3 1] 0 d",   # 4 = dotdash
    "[5] 0 d",         # 5 = longdash
    "[1 1 3 1] 0 d",   # 6 = twodash
  )
}

create_linewidth_spec <- function(attrib) {
  if (is.null(attrib$linewidth)) {
    NULL
  } else {
    paste(attrib$linewidth, 'w')
  }
}

create_fill_spec <- function(attrib) {
  fill <- sanitise_colour_to_rgba_vec(attrib$fill)
  if (is.null(fill)) {
    NULL
  } else {
    glue("{fill[1]} {fill[2]} {fill[3]} rg")
  }
}

create_stroke_spec <- function(attrib) {
  stroke <- sanitise_colour_to_rgba_vec(attrib$stroke)
  if (is.null(stroke)) {
    NULL
  } else {
    glue("{stroke[1]} {stroke[2]} {stroke[3]} RG")
  }
}



get_alphas <- function(attrib) {
  fill   = sanitise_colour_to_rgba_vec(attrib$fill)
  stroke = sanitise_colour_to_rgba_vec(attrib$stroke)

  fill_alpha   <-   fill[4] %||% 1
  stroke_alpha <- stroke[4] %||% 1

  fill_alpha   <- round(fill_alpha  , 2)
  stroke_alpha <- round(stroke_alpha, 2)

  c(fill_alpha, stroke_alpha)
}


get_gs_name <- function(attrib) {
  alphas <- get_alphas(attrib)
  paste0("GS", alphas[1], alphas[2])
}

create_gs_spec <- function(attrib) {
  paste0("/", get_gs_name(attrib), " gs")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the path painting style based upon which of fill/stroke are NULL
# Return one of 'n', 's', 'f', 'b'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_paint_spec <- function(attrib) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For path painting operations. See Table60 PDF3200-1:2008 (page 135)
  # S  - stroke
  # s  - close and stroke (equivalent to 'h S')
  # f  - fil the path (using non-zero winding number)
  # f* - fill the path using even odd rule
  # B  - fill and then stroke the path (nonzero winding)
  # B* - fill and then stroke the path (even-odd)
  # b  - close, fill & stroke (nonzero winding)
  # b* - close, fill & stroek (even odd)
  # n  - do nothing. no-op
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill   <- attrib$fill
  stroke <- attrib$stroke

  if (is.null(fill) && is.null(stroke)) {
    'n'
  } else if (is.null(fill) && !is.null(stroke)) {
    's'
  } else if (!is.null(fill) && is.null(stroke)) {
    'f'
  } else {
    'b'
  }
}