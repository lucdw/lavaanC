# -------------------- main parsing function in C ------------------------------ #
lav_parse_model_string_c <- function(model.syntax = "") {
  .Call("lav_parse_interface", model.syntax)
}
