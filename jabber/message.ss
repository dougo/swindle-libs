(module message "swindle.ss"
  (require "dom.ss")
  (require "stream.ss")
  (require "stanza.ss")
  (provide (all-defined))

  (defelementclass (<message> <stanza>) *client-ns* "message")
)
