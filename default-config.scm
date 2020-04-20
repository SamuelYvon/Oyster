; This file serves as a template for the ~/.oyster.scm file
(import (oyster-core))

(add-reef "/bin") ; add /bin to the list of paths where programs can be found (it's the equivalent of PATH)

; You can set the default knife for shucking. The default knife, the butter-knife,
; will take the input literally and make an array for each line
(set! default-knife oyster#butter-knife)
