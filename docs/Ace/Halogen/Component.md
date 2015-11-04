## Module Ace.Halogen.Component

#### `aceComponent`

``` purescript
aceComponent :: forall g eff. (MonadEff (ace :: ACE | eff) g) => Maybe String -> Component AceState AceQuery g
```

The Ace component.

#### `aceConstructor`

``` purescript
aceConstructor :: forall g p eff. (MonadEff (ace :: ACE | eff) g) => p -> Maybe String -> SlotConstructor AceState AceQuery g p
```

A convenience function for creating a `SlotConstructor` for an Ace
component.


