## Module Ace.Halogen.Component

#### `Autocomplete`

``` purescript
data Autocomplete
  = Live
  | Basic
```

#### `AceEffects`

``` purescript
type AceEffects e = (random :: RANDOM, now :: Now, ref :: REF, ace :: ACE, avar :: AVAR, dom :: DOM | e)
```

#### `AceQuery`

``` purescript
data AceQuery a
  = Init HTMLElement a
  | GetText (String -> a)
  | SetText String a
  | SetAutocomplete (Maybe Autocomplete) a
  | SetCompleteFn (forall e. CompleteFn e) a
  | GetEditor (Maybe Editor -> a)
  | Quit a
```

Ace query algebra
`Init` used internally to handle initialization of component
`GetText` gets the current text value
`SetText` alters the current text value
`SetAutocomplete` sets autocomplete resume:
   `Nothing` turns it off
   `Just Basic` enables basic autocompletions (triggered by `Alt+Space` or `Ctrl + Space`)
   `Just Live` enables live autocomplete
`SetCompleteFn` sets function providing autocomplete variants.
`GetEditor` returns ace editor instance handled by this component.
`Quit` used internally to handle finalizing of component

#### `CompleteFn`

``` purescript
type CompleteFn e = Editor -> EditSession -> Position -> String -> Aff (AceEffects e) (Array Completion)
```

Autocomplete function. Takes editor, session, text position and prefix
returns array of possible completions in `Aff` monad.

#### `AceState`

``` purescript
type AceState = { key :: Maybe String, editor :: Maybe Editor }
```

Ace component state
`key` -- unique key of this instance
`editor` -- ace editor instance wrapped by this component

#### `aceComponent`

``` purescript
aceComponent :: forall g eff. (MonadEff (AceEffects eff) g) => (Editor -> g Unit) -> Maybe Autocomplete -> Component AceState AceQuery g
```

The Ace component.

#### `aceConstructor`

``` purescript
aceConstructor :: forall g p eff. (MonadEff (AceEffects eff) g) => p -> (Editor -> g Unit) -> Maybe Autocomplete -> SlotConstructor AceState AceQuery g p
```

A convenience function for creating a `SlotConstructor` for an Ace
component.


