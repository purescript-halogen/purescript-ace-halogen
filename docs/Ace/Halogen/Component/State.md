## Module Ace.Halogen.Component.State

#### `AceState`

``` purescript
newtype AceState
  = AceState (Maybe Editor)
```

The Ace component state value. This is used to hold a reference to the
underlying Ace editor.

#### `runAceState`

``` purescript
runAceState :: AceState -> Maybe Editor
```

Extracts the current editor reference from the state.

#### `initialAceState`

``` purescript
initialAceState :: AceState
```

An initial empty state for the Ace component.


