require(devtools)
install('~/roxygen2')
require('roxygen2')
require('purrr')
install('~/debugHelpers/pkg')
require(debugHelpers)
#roxygenize('MethodSrcRef/pkg')             

  parsed <- lapply(files, parse_blocks, env = env, registry = registry,
                   global_options = global_options, fileEncoding = desc$Encoding %||% "UTF-8")
  blocks <- unlist(parsed, recursive = FALSE)
#parsed <- parse('MethodSrcRef/pkg/R/source.R')
#refs <- utils::getSrcref(parsed)
#comment_refs <- roxygen2:::comments(refs)
#
#tokens <- lapply(comment_refs, roxygen2:::tokenise_ref)
#has_tokens <- !purrr::map_lgl(tokens, purrr::is_empty)
#registry = list()
#global_options = list()
#
