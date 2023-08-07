
context("lmSimple()")

test_that("lmSimple", {  
  
  
  body_contain<-function(object,expected) {any(grepl(x = as.character(body(object)), pattern = expected))}
  package_loaded<-function(object){any(grepl(object, search()))}
  
  
  
  expect_true(exists("lmSimple"),
              info = "Fel: lmSimple() saknas.")
  
  expect_true(is.function(lmSimple),
              info = "Fel: lmSimple() är inte en funktion.")
  
  expect_function_self_contained(object = lmSimple,
                                 "Fel: Funktionen innehåller fria variabler = definerade utanför funktionen")
  
  expect_function_arguments(object = lmSimple,expected = c("x","y"),
                            info = "Fel: Argumenten i funktionen har felaktiga namn.")
  
  # testar argument
  # testar x:
  expect_error(lmSimple(x = 1:130,y = FALSE),
               info="Funktionen ger inte fel när y inte är numerisk")
  expect_error(lmSimple(x = 1:10,y = "hej"),
               info="Funktionen ger inte fel när y inte är numerisk")
  expect_error(lmSimple(x = 1:10,y = "abc123"),"^y är inte numerisk$",
               info="Fel: Funktionen ger inte rätt felmeddlende när y inte är numerisk: y är inte numerisk")
  
  # testar y:
  expect_error(lmSimple(x = letters[1:3],y = 3:5),
               info="Funktionen ger inte fel när x inte är numerisk")
  expect_error(lmSimple(x = c(FALSE,TRUE),y = 3:39),
               info="Funktionen ger inte fel när x inte är numerisk")
  expect_error(lmSimple(x = c(FALSE,TRUE),y = 3:39),"^x är inte numerisk$",
               info="Fel: Funktionen ger inte rätt felmeddlende när x inte är numerisk: x är inte numerisk")
  
  # testar lika längd på x och y:
  expect_error(lmSimple(x = 1:200,y = 1:3),
               info="Funktionen ger inte fel när x och y är olika långa")
  expect_error(lmSimple(x = 1:20,y = 1:3),"^x och y har inte lika många element$",
               info="Fel: Funktionen ger inte rätt felmeddlende när x och y är olika långa: x och y har inte lika många element")
  
  # testar output
  expect_true(is.list(lmSimple(x = 1:20,y = 3*(1:20)+4)),
              info="Fel: Funktionen returnerar inte en lista")
  
  expect_equal(names(lmSimple(x = 1:20,y = -4*(1:20)-30)),c("beta0","beta1","sigma"),
               info="Fel: Funktionen returnerar inte en lista med rätt namn på elementen")
  
  # kollar förbjudna funktioner:
  expect_false(body_contain(object = lmSimple,expected = "lm"),
               info = "Fel: Funktionen lm() används i funktionen.")
  
  # data utan brus:
  # testar fallet: lmSimple(x = 1:20,y = 1:20+4)
  expect_equal( lmSimple(x = 1:20,y = 1:20+4)$beta1,1,
                info="beta1 är ej korrekt för lmSimple(x = 1:20,y = 1:20+4)")
  expect_equal( lmSimple(x = 1:20,y = 1:20+4)$beta0,4,
                info="beta0 är ej korrekt för lmSimple(x = 1:20,y = 1:20+4)")
  expect_equal( lmSimple(x = 1:20,y = 1:20+4)$sigma,0,
                info="sigma är ej korrekt för lmSimple(x = 1:20,y = 1:20+4)")
  
  
  # testar fallet: lmSimple(x = 1:30,y = -5*(1:30)-100)
  expect_equal( lmSimple(x = 1:30,y = -5*(1:30)-100)$beta1,-5,
                info="beta1 är ej korrekt för lmSimple(x = 1:30,y = -5*(1:30)-100)")
  expect_equal( lmSimple(x = 1:30,y = -5*(1:30)-100)$beta0,-100,
                info="beta0 är ej korrekt för lmSimple(x = 1:30,y = -5*(1:30)-100)")
  expect_equal( lmSimple(x = 1:30,y = -5*(1:30)-100)$sigma,0,
                info="sigma är ej korrekt för lmSimple(x = 1:30,y = -5*(1:30)-100)")
  
  # testar fallet: lmSimple(x = 1:30,y = 0*(1:30)-10)
  expect_equal( lmSimple(x = 1:30,y = 0*(1:30)-10)$beta1,0,
                info="beta1 är ej korrekt för lmSimple(x = 1:30,y = 0*(1:30)-10)")
  expect_equal( lmSimple(x = 1:30,y = 0*(1:30)-10)$beta0,-10,
                info="beta0 är ej korrekt för lmSimple(x = 1:30,y = 0*(1:30)-10)")
  expect_equal( lmSimple(x = 1:30,y = 0*(1:30)-10)$sigma,0,
                info="sigma är ej korrekt för lmSimple(x = 1:30,y = 0*(1:30)-10)")
  
  
  # testar fallet: lmSimple(x = 1:30,y = 20*(1:30)+0)
  expect_equal( lmSimple(x = 1:30,y = 20*(1:30)+0)$beta1,20,
                info="beta1 är ej korrekt för lmSimple(x = 1:30,y = 20*(1:30)+0)")
  expect_equal( lmSimple(x = 1:30,y = 20*(1:30)+0)$beta0,0,
                info="beta0 är ej korrekt för lmSimple(x = 1:30,y = 20*(1:30)+0)")
  expect_equal( lmSimple(x = 1:30,y = 20*(1:30)+0)$sigma,0,
                info="sigma är ej korrekt för lmSimple(x = 1:30,y = 20*(1:30)+0)")
  
  
  
  
  # fallet: beta1 = 0 och beta0 = 0, men sigma>0
  set.seed(42)
  temp_list1<-lmSimple(x = 1:5000,y = rnorm(5000))
  
  expect_equal( temp_list1$beta1,1.823625e-06,tolerance=1e-6,
                info="beta1 är ej korrekt för set.seed(42); temp_list1<-lmSimple(x = 1:5000,y = rnorm(5000))")
  expect_equal( temp_list1$beta0,-0.01895303,tolerance=1e-6,
                info="beta0 är ej korrekt för set.seed(42); temp_list1<-lmSimple(x = 1:5000,y = rnorm(5000))")
  expect_equal( temp_list1$sigma,1.005041,tolerance=1e-6,
                info="sigma är ej korrekt för set.seed(42); temp_list1<-lmSimple(x = 1:5000,y = rnorm(5000))")
  rm(temp_list1)
  
  
  # fler vanliga fall:
  # trees data
  temp_list2<-lmSimple(x = trees$Girth,y = trees$Volume) 
  expect_equal( temp_list2$beta1,5.065856,tolerance=1e-4,
                info="beta1 är ej korrekt för lmSimple(x = trees$Girth,y = trees$Volume)")
  expect_equal( temp_list2$beta0,-36.94346,tolerance=1e-4,
                info="beta0 är ej korrekt för lmSimple(x = trees$Girth,y = trees$Volume)")
  expect_equal( temp_list2$sigma,4.251988,tolerance=1e-4,
                info="sigma är ej korrekt för lmSimple(x = trees$Girth,y = trees$Volume)")
  rm(temp_list2)
  
  
  temp_list3<-lmSimple(x = trees$Height,y = trees$Volume) 
  expect_equal( temp_list3$beta1,1.54335,tolerance=1e-4,
                info="beta1 är ej korrekt för lmSimple(x = trees$Height,y = trees$Volume)")
  expect_equal( temp_list3$beta0,-87.12361,tolerance=1e-4,
                info="beta0 är ej korrekt för lmSimple(x = trees$Height,y = trees$Volume)")
  expect_equal( temp_list3$sigma,13.39698,tolerance=1e-4,
                info="sigma är ej korrekt för lmSimple(x = trees$Height,y = trees$Volume)")
  rm(temp_list3)
  
  # cars data:
  temp_list4<-lmSimple(x = cars$speed,y = cars$dist) 
  expect_equal( temp_list4$beta1,3.932409,tolerance=1e-4,
                info="beta1 är ej korrekt för lmSimple(x = cars$speed,y = cars$dist)")
  expect_equal( temp_list4$beta0,-17.57909,tolerance=1e-4,
                info="beta0 är ej korrekt för lmSimple(x = cars$speed,y = cars$dist)")
  expect_equal( temp_list4$sigma,15.37959,tolerance=1e-4,
                info="sigma är ej korrekt för lmSimple(x = cars$speed,y = cars$dist)")
  rm(temp_list4)
  
  temp_list5<-lmSimple(x = cars$speed,y = -1*cars$dist+100)  
  expect_equal( temp_list5$beta1, -3.932409,tolerance=1e-4,
                info="beta1 är ej korrekt för lmSimple(x = cars$speed,y = -1*cars$dist+100)")
  expect_equal( temp_list5$beta0,117.5791,tolerance=1e-4,
                info="beta0 är ej korrekt för lmSimple(x = cars$speed,y = -1*cars$dist+100)")
  expect_equal( temp_list5$sigma,15.37959,tolerance=1e-4,
                info="sigma är ej korrekt för lmSimple(x = cars$speed,y = -1*cars$dist+100)")
  rm(temp_list5)
  
})