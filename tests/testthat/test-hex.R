test_that("hex enc/dec works", {

  bytes <- seq(1:25)
  enc_hex(bytes) -> s
  s
  
  enc_hex(bytes) |> dec_hex()
  expect_identical(as.raw(bytes), enc_hex(bytes) |> dec_hex())
  
})
