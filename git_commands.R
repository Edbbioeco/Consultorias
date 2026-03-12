# Package ---

library(gert)

# File status ----

gert::git_status() |> 
  as.data.frame()

# Adding files ----

gert::git_add(list.files(pattern = "git_commands.R")) |> 
  as.data.frame()

# Commit file ----

gert::git_commit("Script para comandos de Git")

# Push ----

gert::git_push(remote = "origin", force = TRUE)

# Pulll ----

gert::git_pull(remote = "origin")

# Reseting ----

gert::git_reset_mixed() |> 
  as.data.frame()

gert::git_reset_hard(ref = "HEAD~1") |> 
  as.data.frame()
