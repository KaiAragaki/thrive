my_fun <- function(a) {
  enexpr(a)
}

my_second_fun <- function(b) {
  y <- enquo(b)
  print(as_label(y))
  lapply(y, function(x) my_fun(!!x))
}


library(R7)
library(rlang)

my_function <- function(x) {
  x |> enquo() |> get_expr()
}

cat <- "dog"
my_function(cat)

my_generic <- new_generic("my_generic", c("x"))

method(my_generic, class_character) <-
  function(x) {
    x |> enquo() |> get_expr()
  }

my_generic(cat)

my_s3_generic <- function(x) {
  UseMethod("my_s3_generic")
}

my_s3_generic.character <- function(x) {
  x |> enquo() |> get_expr()
}

my_s3_generic(cat)
