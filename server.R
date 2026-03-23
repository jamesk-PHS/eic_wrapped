function(input, output, session) {
  
  map(list.files("Server", recursive = TRUE, full.names = TRUE), ~source(.x)$value)
  
  
}