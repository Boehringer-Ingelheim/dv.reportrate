message("lintr configuration file loaded ")

# an dv.papo orientiert
linters <- lintr::modify_defaults(
  lintr::default_linters
  , line_length_linter = lintr::line_length_linter(120L)
  #, return_linter = lintr::return_linter(return_style = "explicit")
  , indentation_linter = NULL # Weite der Einrückungen wird ignoriert
  #, object_usage_linter = NULL # hier noch mal auskommentieren und wegen den Funktionsparametern gucken.
  #, trailing_whitespace_linter = NULL
  #, cyclocomp_linter = NULL                   # prevents trivial amount of nesting and long but straightforward functions
  #, object_name_linter = NULL                 # we have reasons to capitalize. nobody in our team CamelCase. shiny does
  #, object_length_linter = NULL               # we don't type long var names just because
  #, pipe_continuation_linter = NULL           # wickham being overly prescriptive
  #, trailing_blank_lines_linter = NULL        # natural extension of trailing_whitespace_linter, present on the template
)

if(identical(Sys.getenv('CI'), "true")){
  linters <- lintr::modify_defaults(
    linters
    , object_usage_linter = NULL              # R lacks var declarations; it's easy to assign to the wrong variable by mistake
  )                                           # We only disable this lint rule on github because it fails there because
}

linters
