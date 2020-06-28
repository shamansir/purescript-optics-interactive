module Types where

import Prelude (($))
import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)


type Constructor = String


type Label = String


data TypeRef a
    = None
    | Unit
    | ForAll Label
    | Alias Label (TypeRef a)
    | Custom a
    | Sum Label ( Array ( Constructor /\ Array (TypeRef a) ) )
    | Product Constructor ( Array (TypeRef a) )
    | Product' ( Array ( Label /\ TypeRef a ) )


data LensFunc
    = View
    | Set
    | Over
    | Preview
    | Is
    | Review
    | FoldOf
    | FirstOf
    | LastOf
    | ToListOf


data OpticKind
    = LensOptic
    | PrismOptic
    | TraversalOptic
    | IsoOptic


type Value = String


value :: forall a. a -> TypeRef a
value = Custom


unit :: forall a. TypeRef a
unit = Unit


forall_ :: forall a. Label -> TypeRef a
forall_ = ForAll


maybe :: forall a. TypeRef a -> TypeRef a
maybe a =
    Sum "Maybe"
        [ ( "Just" /\ [ a ] )
        , ( "Nothing" /\ [] )
        ]


either :: forall a. TypeRef a -> TypeRef a -> TypeRef a
either a b =
    Sum "Either"
        [ ( "Left" /\ [ a ] )
        , ( "Right" /\ [ b ] )
        ]


tuple :: forall a. TypeRef a -> TypeRef a -> TypeRef a
tuple a b =
    Product "Tuple" [ a, b ]


record :: forall a. Array ( Label /\ TypeRef a ) -> TypeRef a
record fields =
    Product' fields


tree :: forall a. TypeRef a -> TypeRef a
tree leaf =
    Sum "Tree"
        [ ( "Empty" /\ [ ] )
        , ( "Node" /\ [ tree leaf, leaf, tree leaf ] )
        ]


contact :: TypeRef String
contact =
   Sum "Contact"
       [ ( "Phone" /\ [ Alias "Number" $ Custom "String" ] )
       , ( "Skype" /\ [ Alias "ID" $ Custom "String" ] )
       ]


entry :: TypeRef String
entry =
   Product "Entry"
       [ Alias "Name" $ Custom "String"
       , contact
       ]


book :: TypeRef String
book =
    tree entry
