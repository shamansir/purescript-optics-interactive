module Types where

import Data.Tuple.Nested ((/\), type (/\))
import Data.List (List)


type Constructor = String


type Label = String


data TypeRef a
    = None
    | Any
    | Unit
    | ForAll Label
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


int :: TypeRef Value
int = Custom "Int"


maybe :: forall a. TypeRef a -> TypeRef a
maybe a =
    Sum "Maybe"
        [ ( "Just" /\ [ a ] )
        , ( "Nothing" /\ [] )
        ]


either :: TypeRef Value -> TypeRef Value -> TypeRef Value
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
