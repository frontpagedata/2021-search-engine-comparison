rmarkdown::render(here::here("rmd", "final.Rmd"))

# name study like FTP_CTR_STUDY in environ-file
usethis::edit_r_environ()

# add study name under Sys.getenv 
RCurl::ftpUpload(here::here("rmd", "02_analysis.html"), Sys.getenv("SEARCH_ENGINE_COMPARISON"))


