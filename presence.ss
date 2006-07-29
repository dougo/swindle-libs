(module presence "swindle.ss"
  (require "dom.ss")
  (require "stream.ss")
  (require "stanza.ss")
  (provide (all-defined))

  (defelementclass (<presence> <stanza>) *client-ns* "presence")

)
