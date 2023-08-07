### Assignment structure ###

context("Labbinformation")

test_that("Obligatoriska object", {
  expect_true(exists("Namn"), info = "Variabeln 'Namn' saknas.")
  expect_true(exists("LiuId"), info = "Variabeln 'LiuId' saknas.")
  
  expect_is(Namn, "character", info = "Variabeln 'Namn' ska vara en textvariabel.")
  expect_true(nchar(Namn[1]) > 0, info = "Ange era Namn.")
  
  expect_is(LiuId, "character", info = "Variabeln 'LiuId' ska vara en textvariabel.")
  expect_true(nchar(LiuId[1]) > 0, info = "Ange era LiuId.")
  
  
  expect_true(all(strsplit(substr(LiuId,start = 1,stop = 5),split = "")[[1]]%in%letters) , 
              info = "De fem första tecknen i LiuId ska vara små bokstäver.")
  expect_true(all(strsplit(substr(LiuId,start = 6,stop = 8),split = "")[[1]]%in%0:9) , 
              info = "De tre sista tecknen i LiuId ska vara siffor.")
  
})