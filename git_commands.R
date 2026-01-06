# Package ---

library(gert)

# Adding files ----

gert::git_add(list.files(pattern = "set_")) |> as.data.frame()
