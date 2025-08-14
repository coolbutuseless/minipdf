
if (interactive()) {

tt <- function() {
  doc <- create_pdf(width = 600)
  
  doc <- pdf_rect(doc, 120, 120, 200, 100, fill = sample(colors(), 1), alpha = 0.8)
  doc <- pdf_line(doc, 20, 0, 120, 200, col = 'blue', lwd = 20, lineend = 'butt', lty = 3)
  
  doc <- pdf_clip_rect(doc, 80, 80, 300, 300); # Global clipping
  
  N  <- 10
  xs <- runif(N, 1, 400)
  ys <- runif(N, 1, 400)
  doc <- pdf_polyline(doc, xs, ys, col = 'darkgreen', alpha = 0.2)
  
  xs <- c(100, 300, 300)
  ys <- c(100, 100, 300)
  doc <- pdf_polygon(doc, xs, ys, col = 'black', fill = "#ff000080")
  
  doc <- pdf_circle(doc, 300, 300, 100, col = 'hotpink', fill = '#00ff0080')
  
  # doc <- pdf_clip_polygon(doc, xs = c(0, 0, 400), ys = c(0, 400, 0))
  
  
  doc <- pdf_text(doc, "Hello #RStats", 50, 50, fontsize = 40, fill = 'black', col = 'hotpink',
                  fontfamily = "mono", fontface = 'bold.italic', mode = 2,
                  tf = tf_rotate(0.5, 50, 50), 
                  clip = clip_polygon(xs = c(0, 0, 400), ys = c(0, 400, 0)))
  
  
  xs <- c(100, 300, 300, 100,   150, 250, 250, 150) + 50
  ys <- c(100, 100, 300, 300,   150, 150, 250, 250) + 50
  id <- c(1, 1, 1, 1, 2, 2, 2, 2)
  doc <- pdf_polygon(doc, xs, ys, id = id, rule = 'evenodd')
  
  
  w <- 10
  h <- 10
  im <- matrix(as.integer(100 + 50 * sin(8 * seq(w * h))), w, h)
  doc <- pdf_image(doc, im, x = 50, y = 50, scale = 10, interpolate = FALSE)
  
  
  w <- 10
  h <- 10
  im <- array(as.integer(0), dim = c(w, h, 2))
  im[,,1] <- 255
  im[,,2] <- 200
  
  doc <- pdf_image(doc, im, x = 150, y = 150, scale = 10)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_newpage(doc)
  
  theta <- seq(0, 359, 60) + 30
  xs1 <- 200 * cos(theta * pi/180) + 300
  ys1 <- 200 * sin(theta * pi/180) + 200
  xs2 <-  40 * cos(theta * pi/180) + 300
  ys2 <-  40 * sin(theta * pi/180) + 200
  
  xs <- c(xs1, xs2)
  ys <- c(ys1, ys2)
  id <- c(
    rep(1, length(xs1)),
    rep(2, length(xs2))
  )
  
  doc <- pdf_clip_polygon(doc, xs, ys, id = id, rule = 'evenodd')
  # doc <- pdf_clip_polygon(doc, xs1, ys1)
  
  
  N <- 100
  xs <- sample(600, N, TRUE)
  ys <- sample(400, N, TRUE)
  rs <- sample(100, N, TRUE)
  cs <- sample(colors(), N, TRUE)
  
  for (i in seq_len(N)) {
    doc <- pdf_circle(doc, xs[i], ys[i], rs[i], col = NA, fill = cs[i], alpha = 0.2)
  }
  
  cs <- rainbow(400)
  for (i in seq(1, 400, 10)) {
    doc <- pdf_line(doc, i, 0, 0, 400 - i, col = cs[i], alpha = 0.2)
  }
  
  doc <- pdf_translate(doc, 120, -80)
  
  doc <- pdf_text(doc, "Hello", 20, 300, fontsize = 90, mode = 0, fill = 'black', 
                  fontface = 'plain')
  
  doc <- pdf_text(doc, "#RStats", 20, 200, fontsize = 80, mode = 1, col = 'royalblue', 
                  fontface = 'bold.italic', lwd = 5)
  
  
  
  im  <- png::readPNG(system.file("img", "Rlogo.png", package="png")) * 255
  doc <- pdf_image(doc, im, x = 250, y = 290, scale = 1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc
  write_pdf(doc) |> cat()
  write_pdf(doc, "working/test.pdf")
  invisible(doc)
}

ttt <- function() {
  doc <- create_pdf(width = 600, height = 400)
  
  doc <- pdf_text(doc, "He)ll(o Use a \\ to escape", 20, 200, fontsize = 30)
  
  doc
  write_pdf(doc) |> cat()
  write_pdf(doc, "working/test.pdf")
  invisible(doc)
}


ee <- function() {
  doc <- create_pdf()
  write_pdf(doc) |> cat()
  write_pdf(doc, "working/test.pdf")
  invisible(doc)
}



}


if (FALSE) {
  
  set.seed(1)
  doc <- create_pdf()
  
  N <- 100
  xs <- sample(400, N, TRUE)
  ys <- sample(400, N, TRUE)
  rs <- sample(100, N, TRUE)
  cs <- sample(colors(), N, TRUE)
  
  for (i in seq_len(N)) {
    doc <- pdf_circle(doc, xs[i], ys[i], rs[i], col = NA, fill = cs[i], alpha = 0.2)
  }
  
  cs <- rainbow(400)
  for (i in seq(1, 400, 10)) {
    doc <- pdf_line(doc, i, 0, 0, 400 - i, col = cs[i], alpha = 0.2)
  }
  
  doc <- pdf_text(doc, "Hello", 20, 300, fontsize = 90, mode = 0, fill = 'black', 
                  fontface = 'plain')
  
  doc <- pdf_text(doc, "#RStats", 20, 200, fontsize = 90, mode = 1, col = 'hotpink', 
                  fontface = 'bold.italic', lwd = 5)
  
  write_pdf(doc, "working/test.pdf")
  
  
}


