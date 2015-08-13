make_dplyr_argument = function(...) {
  parts = list(...)
  arg_string = paste0("~", paste0(parts, collapse = " ") )
  return( as.formula(arg_string, env = parent.frame()) )
}