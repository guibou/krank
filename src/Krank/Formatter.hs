module Krank.Formatter (
  showViolations
  ) where

import Krank.Types

showViolations :: [Violation]
               -> String
showViolations violations = show violations
