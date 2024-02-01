# Utilities for linking the blog and the *Lessons* text.

## Cross references

LST_text_URL_root <- function() "https://dtkaplan.github.io/Lessons-in-statistical-thinking/"

LST_text_crossref <- function(url_fragment, label = "") {

  glue::glue("[{label}]({LST_text_URL_root()}{url_fragment}) ")

}
