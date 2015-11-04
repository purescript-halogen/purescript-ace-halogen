## Module Ace.Halogen.Component.Query

#### `AceQuery`

``` purescript
data AceQuery a
  = Init HTMLElement a
  | GetText (String -> a)
  | SetText String a
```

The query algebra for the Ace component.

- `Init` is not intended to be called externally, the component will raise
  this itself once its `initializer` fires.
- `GetText` gets the current text value.
- `SetText` alters the current text value.


