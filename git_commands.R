# Package ---

library(gert)

# Adding files ----

gert::git_add(list.files(pattern = "commands")) |> as.data.frame()

# Commit file ----

gert::git_commit("Script to git commands")

# Push ----

gert::git_push(remote = "origin", force = TRUE)

# Pulll ----

gert::git_pull()

# Aborting commits ----

gert::git_reset_hard(ref = "HEAD")
