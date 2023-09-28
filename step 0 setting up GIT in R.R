install.packages("usethis")
usethis::use_git_config(user.name="Christian Thorjussen", user.email="christianbern@gmail.com")
usethis::create_github_token()
gitcreds::gitcreds_set()