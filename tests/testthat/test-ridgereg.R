data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=0.2))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda=0.2))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda="0.2"))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.2)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.2)
  
  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(ridgereg_mod$print(),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("coef() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.2)
  
  expect_true(all(round(unname(ridgereg_mod$coef()),2) %in% c(3.75, -0.58, 1.47)))
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.2)
  
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]),2), c(1.84, 1.53, 1.09))    
})

