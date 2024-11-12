# -------------------- main parsing function in C ------------------------------ #
lav_parse_model_string_c <- function(model.syntax = "", debug = FALSE) {
  .Call("lav_parse_interface", model.syntax, debug)
}
