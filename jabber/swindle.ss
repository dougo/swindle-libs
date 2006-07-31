(module swindle (lib "swindle.ss" "dom")
  (provide (all-from-except (lib "swindle.ss" "dom") version))
  (provide (rename version mz:version))
)
