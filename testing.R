
matr = function(...) {
  args = ... |> cbind() |> rbind() |> substitute() |> deparse()
  args = gsub("\\|","),cbind(",args)
  eval(parse(text=args))
}
# %%
matr(1, 2, 4 |
     3, 6, 7)
matrix(c(1, 2, 4,
         3, 6, 7), nrow = 2, byrow = T)
