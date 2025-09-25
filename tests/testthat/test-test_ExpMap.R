test_that("map_exp works", {
    Var1 = c(1:5)
    Var2 = LETTERS[3:9]
    cols = c(2:23)
    rows = c(2:15)
    map = map_exp(cols, rows, Var1, Var2, seed =3)
  expect_equal(length(Var1) * length(Var2), nrow(map)) 
})
