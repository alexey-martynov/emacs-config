(when (locate-library "smart-tabs-mode")
  (require 'smart-tabs-mode)

  (smart-tabs-insinuate 'c 'c++ 'javascript))
