context("lmFunc()")

test_that("lmFunc", {  
  
  
  
  body_contain<-function(object,expected) {any(grepl(x = as.character(body(object)), pattern = expected))}
  package_loaded<-function(object){any(grepl(object, search()))}
  
  
  
  expect_true(exists("lmFunc"),
              info = "Fel: lmFunc() saknas.")
  
  expect_true(is.function(lmFunc),
              info = "Fel: lmFunc() är inte en funktion.")
  
  expect_function_self_contained(object = lmFunc,
                                 "Fel: Funktionen innehåller fria variabler = definerade utanför funktionen")
  
  expect_function_arguments(object = lmFunc,expected = c("X","y","betaTest","mu0"),
                            info = "Fel: Argumenten i funktionen har felaktiga namn.")
  
  # testar argument
  # testar y:
  expect_error(lmFunc(X = trees[,1:2],y = data.frame(3),betaTest = FALSE,mu0 = c(0,0,0)) ,
               info="Funktionen ger inte fel när y inte är en vektor")
  expect_error(lmFunc(X = trees[,1:2],y = data.frame(3),betaTest = FALSE,mu0 = c(0,0,0)) ,
               "^y är inte en vektor$",
               info="Fel: Funktionen ger inte rätt felmeddlende när y inte är en vektor: y är inte en vektor")
  
  # testar X:
  expect_error(lmFunc(X = list(2,"h"),y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0)),
               info="Funktionen ger inte fel när X inte är en matris eller en data.frame")
  expect_error(lmFunc(X = list(2,"h"),y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0)),
               "^X är ingen matris eller data.frame$",
               info="Fel: Funktionen ger inte rätt felmeddlende när x inte är en matris eller en data.frame: X är ingen matris eller data.frame")
  
  # testar längden på mu0:
  expect_error(lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = 1:10),
               info="Funktionen ger inte fel när mu0 inte har rätt längd")
  expect_error(lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = 1:10),
               "^mu0 har inte lika många element som parametrar i data$",
               info="Fel: Funktionen ger inte rätt felmeddlende när  mu0 inte har rätt längd: mu0 har inte lika många element som parametrar i data")
  
  # testar output
  expect_true(is.list(lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)) ),
              info="Fel: Funktionen returnerar inte en lista")
  
  
  expect_true( length(lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)))==3,
               info="Fel: Funktionen returnerar inte en lista av rätt längd när betaTest = FALSE")
  
  expect_true( length(lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)))==4,
               info="Fel: Funktionen returnerar inte en lista av rätt längd när betaTest = TRUE")
  
  expect_equal(names(lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)) ),c("beta","sigma","betaCov"),
               info="Fel: Funktionen returnerar inte en lista med rätt namn på elementen när betaTest = FALSE")
  
  expect_equal(names(lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) ),c("beta","sigma","betaCov","testMat"),
               info="Fel: Funktionen returnerar inte en lista med rätt namn på elementen när betaTest = TRUE")
  
  
  # kollar förbjudna funktioner:
  expect_false(body_contain(object = lmFunc,expected = "lm"),
               info = "Fel: Funktionen lm() används i funktionen.")
  expect_false(body_contain(object = lmFunc,expected = "glm"),
               info = "Fel: Funktionen glm() används i funktionen.")
  expect_false(body_contain(object = lmFunc,expected = "summary.lm"),
               info = "Fel: Funktionen summary.lm() används i funktionen.")
  expect_false(body_contain(object = lmFunc,expected = "summary"),
               info = "Fel: Funktionen summary() används i funktionen.")
  expect_false(body_contain(object = lmFunc,expected = "vcov"),
               info = "Fel: Funktionen vcov() används i funktionen.")
  
  expect_false(package_loaded(object = "vcov"),
               info = "Fel: Paketet vcov används i funktionen.")
  
  #-----------------------------------------------------------------------------
  # testar att funktionen räknar rätt
  #-----------------------------------------------------------------------------
  
  comp_list<-readRDS(file = url("https://github.com/STIMALiU/RegVar_student/blob/main/tests/tasks/test_list_lab2.rds?raw=true","rb"))
  
  #-----------------------------------------------------------------------------
  list1<-lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0))
  # test: test1
  expect_true(object = all.equal(list1$beta,comp_list$test1$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)")
  expect_true(object = all.equal(list1$sigma,comp_list$test1$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)")
  expect_true(object = all.equal(list1$betaCov,comp_list$test1$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0)")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list2<-lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0))
  # test: test2
  expect_true(object = all.equal(list2$beta,comp_list$test2$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0))")
  expect_true(object = all.equal(list2$sigma,comp_list$test2$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0))")
  expect_true(object = all.equal(list2$betaCov,comp_list$test2$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = trees[,1:2],y = trees$Volume,betaTest = FALSE,mu0 = c(0,0,0))")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list3<-lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) 
  # test: test3
  expect_true(object = all.equal(list3$beta,comp_list$test3$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) ")
  expect_true(object = all.equal(list3$sigma,comp_list$test3$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) ")
  expect_true(object = all.equal(list3$betaCov,comp_list$test3$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) ")
  expect_true(object = all.equal(list3$testMat,comp_list$test3$test_mat,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet testMat är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(0,0)) ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list4<-lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(-35,5))
  # test: test4
  expect_true(object = all.equal(list4$beta,comp_list$test4$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(-35,5)) ")
  expect_true(object = all.equal(list4$sigma,comp_list$test4$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(-35,5)) ")
  expect_true(object = all.equal(list4$betaCov,comp_list$test4$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(-35,5)) ")
  expect_true(object = all.equal(list4$testMat,comp_list$test4$test_mat,check.attributes=FALSE,tolerance=1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = trees[,1,drop=FALSE],y = trees$Volume,betaTest = TRUE,mu0 = c(-35,5)) ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list5<-lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0))
  # test: test4
  expect_true(object = all.equal(list5$beta,comp_list$test5$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0)) ")
  expect_true(object = all.equal(list5$sigma,comp_list$test5$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0)) ")
  expect_true(object = all.equal(list5$betaCov,comp_list$test5$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0)) ")
  # expect_true(object = all.equal(list5$testMat,comp_list$test5$test_mat,check.attributes=FALSE,tolerance=1e-5),
  #             info = "elementet testMat är ej korrekt för lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0))")
  expect_true(object = all(abs(list5$testMat-comp_list$test5$testMat)<1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = trees[1:15,1:2],y = trees$Volume[1:15],betaTest = TRUE,mu0 = c(0,0,0))")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  library(car) 
  data(Prestige) 
  list6<-lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = FALSE,mu0 = c(0,0,0,0)) 
  # test: test4
  expect_true(object = all.equal(list6$beta,comp_list$test6$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = FALSE,mu0 = c(0,0,0,0))  ")
  expect_true(object = all.equal(list6$sigma,comp_list$test6$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = FALSE,mu0 = c(0,0,0,0))  ")
  expect_true(object = all.equal(list6$betaCov,comp_list$test6$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = FALSE,mu0 = c(0,0,0,0))  ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list7<-lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) 
  # test: test4
  expect_true(object = all.equal(list7$beta,comp_list$test7$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) ")
  expect_true(object = all.equal(list7$sigma,comp_list$test7$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) ")
  expect_true(object = all.equal(list7$betaCov,comp_list$test7$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) ")
  # expect_true(object = all.equal(list7$testMat,comp_list$test7$test_mat,check.attributes=FALSE,tolerance=1e-5),
  #             info = "elementet testMat är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) ")
  expect_true(object =  all(abs(list7$testMat-comp_list$test7$testMat)<1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(0,0,0,0)) ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list8<-lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(-6,4,0.001,0.01))
  # test: test4
  expect_true(object = all.equal(list8$beta,comp_list$test8$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(-6,4,0.001,0.01)) ")
  expect_true(object = all.equal(list8$sigma,comp_list$test8$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(-6,4,0.001,0.01)) ")
  expect_true(object = all.equal(list8$betaCov,comp_list$test8$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(-6,4,0.001,0.01)) ")
  expect_true(object = all.equal(list8$testMat,comp_list$test8$test_mat,check.attributes=FALSE,tolerance=1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = Prestige[,1:3],y = Prestige[,4],betaTest = TRUE,mu0 = c(-6,4,0.001,0.01)) ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  set.seed(35) 
  X<-matrix(data = runif(n = 4*300,min = -1,max = 1),nrow = 300,ncol = 4) 
  colnames(X)<-letters[1:4]
  beta_vect<-c(2,0,0,-3) 
  set.seed(53) 
  y<- as.vector(-2 + X%*%beta_vect+rnorm(n = 300)) 
  list9<-lmFunc(X = X,y =y,betaTest = FALSE,mu0 = c(0,0,0,0,0)) 
  # test: test4
  expect_true(object = all.equal(list9$beta,comp_list$test9$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = X,y =y,betaTest = FALSE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben")
  expect_true(object = all.equal(list9$sigma,comp_list$test9$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = X,y =y,betaTest = FALSE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben")
  expect_true(object = all.equal(list9$betaCov,comp_list$test9$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = X,y =y,betaTest = FALSE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list10<-lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(0,0,0,0,0)) 
  # test: test4
  expect_true(object = all.equal(list10$beta,comp_list$test10$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list10$sigma,comp_list$test10$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(0,0,0,0,0))), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list10$betaCov,comp_list$test10$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list10$testMat,comp_list$test10$test_mat,check.attributes=FALSE,tolerance=1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(0,0,0,0,0)), X och y är simulerade, se labben ")
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------
  list11<-lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(-2,2,0,0,-3)) 
  # test: test4
  expect_true(object = all.equal(list11$beta,comp_list$test11$beta,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet beta är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(-2,2,0,0,-3)), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list11$sigma,comp_list$test11$sigma,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet sigma är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(-2,2,0,0,-3)), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list11$betaCov,comp_list$test11$beta_cov,check.attributes=FALSE,tolerance=1e-6),
              info = "elementet betaCov är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(-2,2,0,0,-3)), X och y är simulerade, se labben ")
  expect_true(object = all.equal(list11$testMat,comp_list$test11$test_mat,check.attributes=FALSE,tolerance=1e-5),
              info = "elementet testMat är ej korrekt för lmFunc(X = X,y =y,betaTest = TRUE,mu0 = c(-2,2,0,0,-3)), X och y är simulerade, se labben ")
  #-----------------------------------------------------------------------------
  
  
})