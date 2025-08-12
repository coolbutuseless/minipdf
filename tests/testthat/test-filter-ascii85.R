
test_that("ascii85 enc/dec works", {
  
  bytes <- seq(1:25)
  enc_ascii85(bytes) -> s
  s
  
  enc_ascii85(bytes) |> dec_ascii85()
  expect_identical(as.raw(bytes), enc_ascii85(bytes) |> dec_ascii85())
  
})

