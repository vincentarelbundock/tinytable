  # I would like to use this fancy listener, but I can't get it to work with multiple style_tt()/group_tt() calls
  # CSS listeners
  listener_template <- "
   window.addEventListener('load', function () {
       const cellStyles = [
           %s
       ];
       cellStyles.forEach(({coords, class: cssClass}) => {
           styleCell_%s('%s', coords, cssClass);
       });
   });"

  # I would like to use the fancier listener, but I can't get it to work.
    css_rules <- split(css_rules, css_rules$id)
    coords <- lapply(css_rules, function(z) sprintf("[%s, %s]", z$i, z$j))
    coords <- lapply(coords, paste, collapse = ", ")
    for (i in seq_along(coords)) {
        co <- sprintf(
          "{coords: [%s], class: '%s'},", 
          coords[[i]],
          names(coords)[i])
        listener <- sprintf(listener_template, co, x@id, names(coords)[i])
        out <- bootstrap_setting(out, listener, component = "cell")
        css_rule <- sprintf(css_template, names(css_rules)[i], names(css_rules)[i], css_rules[[i]]$bootstrap[1])
        out <- bootstrap_setting(out, css_rule, component = "css")
    }

