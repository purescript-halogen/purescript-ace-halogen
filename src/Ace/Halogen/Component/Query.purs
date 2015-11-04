module Ace.Halogen.Component.Query (AceQuery(..)) where

import DOM.HTML.Types (HTMLElement())

-- | The query algebra for the Ace component.
-- |
-- | - `Init` is not intended to be called externally, the component will raise
-- |   this itself once its `initializer` fires.
-- | - `GetText` gets the current text value.
-- | - `SetText` alters the current text value.
data AceQuery a
  = Init HTMLElement a
  | GetText (String -> a)
  | SetText String a
