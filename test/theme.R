options$layout.heights %>% {.[order(names(.))]} %>% str()
options$layout.heights %>% str()
List of 19
 # $ top.padding      : num 1
 $ main             : num 1
 $ main.key.padding : num 1
 # $ key.top          : num 1
 $ xlab.top         : num 1
 $ key.axis.padding : num 1
 $ axis.top         : num 1
 $ strip            : num 1
 $ panel            : num 1
 $ axis.panel       : num 1
 $ between          : num 1
 $ axis.bottom      : num 1
 $ axis.xlab.padding: num 1
 $ xlab             : num 1
 $ xlab.key.padding : num 0
 $ key.bottom       : num 1
 $ key.sub.padding  : num 1
 $ sub              : num 1
 $ bottom.padding   : num 1

options$layout.widths %>% {.[order(names(.))]} %>% str()

# options$axis.components %>% str()
# List of 4
#  $ left  :List of 3
#   ..$ tck : num 1
#   ..$ pad1: num 1
#   ..$ pad2: num 1
#  $ top   :List of 3
#   ..$ tck : num 1
#   ..$ pad1: num 1
#   ..$ pad2: num 1
#  $ right :List of 3
#   ..$ tck : num 1
#   ..$ pad1: num 1
#   ..$ pad2: num 1
#  $ bottom:List of 3
#   ..$ tck : num 1
#   ..$ pad1: num 1
#   ..$ pad2: num 1
