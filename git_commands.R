# Package ---

library(gert)

# Adding files ----

gert::git_add(list.files(pattern = ".wav")) |> as.data.frame()

# Commit file ----

gert::git_commit("references .bib files")

# Push ----

gert::git_push(remote = "origin", force = TRUE)

# Pulll ----

gert::git_pull()

# Aborting commit ----

gert::git_reset_hard(ref = "HEAD")
