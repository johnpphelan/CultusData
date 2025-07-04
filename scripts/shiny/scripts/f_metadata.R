creelMetadataBox <- function(title, keys, fields) {
  HTML(sprintf(
    "<div class='container' style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);'>
      <h4 style='color: #34495e; font-weight: bold; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>%s</h4>
      <p><strong>Keys:</strong> <span style='color: #3498db;'>%s</span></p>
      <p><strong>Contents:</strong></p>
      <ul style='list-style-type: none; padding-left: 10px;'>%s</ul>
    </div>",
    title,
    keys,
    paste(sprintf(
      "<li><strong title='%s' style='cursor: help;'>%s</strong></li>",
      names(fields),
      unname(fields)
    ), collapse = "\n")
  ))
}