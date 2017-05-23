#lang s-exp framework/keybinding-lang
(keybinding "c:s:right" (λ (editor evt) (send editor insert "↪")))