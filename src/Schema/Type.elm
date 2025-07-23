module Schema.Type exposing
    ( Node, Type(..)
    , StringOptions, Sanitize(..), defaultStringOpts, sanitize, sanitizeAll
    , gatherNamed, isRecursive
    )

{-| Types

@docs Node, Type
@docs StringOptions, Sanitize, defaultStringOpts, sanitize, sanitizeAll
@docs gatherNamed, isRecursive

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| TODO: document
-}
type Type
    = Unit
    | String (Maybe StringOptions)
    | Uuid
    | Bool
    | Int
    | Float
    | List Node
    | Array Node
    | Maybe Node
    | Dict Node Node
    | Set Node
    | Tuple (List Node)
    | Record (List ( String, Node ))
    | CustomType CustomType_Args
    | Recursive String


type alias CustomType_Args =
    { name : String
    , variants : List { name : String, args : List Node }
    }


{-| TODO: document
-}
type alias Node =
    { type_ : Type
    , description : Maybe String
    }


{-| TODO: document
-}
type alias StringOptions =
    { multiline : Bool
    , maxLength : Maybe Int
    , minLength : Maybe Int
    , translatable : Bool
    , sensitive : Bool
    , sanitize : List Sanitize
    }


{-| TODO: document
-}
defaultStringOpts : StringOptions
defaultStringOpts =
    { multiline = False
    , maxLength = Nothing
    , minLength = Nothing
    , translatable = False
    , sensitive = False
    , sanitize = []
    }


{-| TODO: document
-}
type Sanitize
    = Trim
    | Lowercase
    | Uppercase


{-| TODO: document
-}
sanitize : Sanitize -> String -> String
sanitize op str =
    case op of
        Trim ->
            String.trim str

        Lowercase ->
            String.toLower str

        Uppercase ->
            String.toUpper str


{-| TODO: document
-}
sanitizeAll : List Sanitize -> String -> String
sanitizeAll ops str =
    List.foldl sanitize str ops


{-| TODO: document
-}
gatherNamed : Node -> Dict String Node
gatherNamed node =
    case node.type_ of
        Unit ->
            Dict.empty

        String _ ->
            Dict.empty

        Uuid ->
            Dict.empty

        Bool ->
            Dict.empty

        Int ->
            Dict.empty

        Float ->
            Dict.empty

        List itemType ->
            gatherNamed itemType

        Array itemType ->
            gatherNamed itemType

        Maybe itemType ->
            gatherNamed itemType

        Dict keyType valueType ->
            gatherNamed valueType

        Set itemType ->
            gatherNamed itemType

        Tuple types ->
            List.foldl
                (\t acc -> Dict.union acc (gatherNamed t))
                Dict.empty
                types

        Record fields ->
            List.foldl
                (\( _, fieldType ) acc -> Dict.union acc (gatherNamed fieldType))
                Dict.empty
                fields

        CustomType args ->
            List.foldl
                (\variant acc ->
                    Dict.union acc
                        (List.foldl
                            (\arg acc2 -> Dict.union acc2 (gatherNamed arg))
                            Dict.empty
                            variant.args
                        )
                )
                (Dict.singleton args.name node)
                args.variants

        Recursive name ->
            Dict.empty


isRecursive : Node -> Bool
isRecursive node =
    case node.type_ of
        CustomType args ->
            List.any
                (.args
                    >> List.any (isRecursiveHelp args.name)
                )
                args.variants

        _ ->
            False


isRecursiveHelp : String -> Node -> Bool
isRecursiveHelp name node =
    case node.type_ of
        Unit ->
            False

        String _ ->
            False

        Uuid ->
            False

        Bool ->
            False

        Int ->
            False

        Float ->
            False

        List itemType ->
            isRecursiveHelp name itemType

        Array itemType ->
            isRecursiveHelp name itemType

        Maybe itemType ->
            isRecursiveHelp name itemType

        Dict keyType valueType ->
            isRecursiveHelp name valueType

        Set itemType ->
            isRecursiveHelp name itemType

        Tuple types ->
            List.any (isRecursiveHelp name) types

        Record fields ->
            List.any (isRecursiveHelp name << Tuple.second) fields

        CustomType args ->
            List.any (List.any (isRecursiveHelp name) << .args) args.variants

        Recursive n ->
            n == name



-- JSON Encoding


typeName : Node -> String
typeName node =
    case node.type_ of
        Unit ->
            "Unit"

        String _ ->
            "String"

        Uuid ->
            "Uuid"

        Bool ->
            "Bool"

        Int ->
            "Int"

        Float ->
            "Float"

        List t ->
            "List (" ++ typeName t ++ ")"

        Array t ->
            "Array (" ++ typeName t ++ ")"

        Maybe t ->
            "Maybe (" ++ typeName t ++ ")"

        Dict k v ->
            "Dict (" ++ typeName k ++ ") (" ++ typeName v ++ ")"

        Set t ->
            "Set (" ++ typeName t ++ ")"

        Tuple ts ->
            "( " ++ String.join ", " (List.map typeName ts) ++ " )"

        Record fields ->
            List.map (\( n, t ) -> n ++ " : " ++ typeName t) fields
                |> String.join ", "
                |> (\s -> "{ " ++ s ++ " }")

        CustomType args ->
            args.name

        Recursive name ->
            name


encode : Node -> Value
encode node =
    case node.type_ of
        Unit ->
            JE.object
                [ ( "type", JE.string "Unit" )
                ]

        String opts ->
            JE.object
                [ ( "type", JE.string "String" )
                ]

        Uuid ->
            JE.object
                [ ( "type", JE.string "Uuid" )
                ]

        Bool ->
            JE.object
                [ ( "type", JE.string "Bool" )
                ]

        Int ->
            JE.object
                [ ( "type", JE.string "Int" )
                ]

        Float ->
            JE.object
                [ ( "type", JE.string "Float" )
                ]

        List itemType ->
            JE.object
                [ ( "type", JE.string "List" )
                , ( "itemType", encode itemType )
                ]

        Array itemType ->
            JE.object
                [ ( "type", JE.string "Array" )
                , ( "itemType", encode itemType )
                ]

        Maybe itemType ->
            JE.object
                [ ( "type", JE.string "Maybe" )
                , ( "itemType", encode itemType )
                ]

        Dict keyType valueType ->
            JE.object
                [ ( "type", JE.string "Dict" )
                , ( "keyType", encode keyType )
                , ( "valueType", encode valueType )
                ]

        Set itemType ->
            JE.object
                [ ( "type", JE.string "Set" )
                , ( "itemType", encode itemType )
                ]

        Tuple types ->
            JE.object
                [ ( "type", JE.string "Tuple" )
                , ( "args", JE.list encode types )
                ]

        Record fields ->
            JE.object
                [ ( "type", JE.string "Record" )
                , ( "fields"
                  , JE.list
                        (\field ->
                            JE.object
                                [ ( "name", JE.string (Tuple.first field) )
                                , ( "type", encode (Tuple.second field) )
                                ]
                        )
                        fields
                  )
                ]

        CustomType args ->
            JE.object
                [ ( "type", JE.string "CustomType" )
                , ( "name", JE.string args.name )
                , ( "args"
                  , JE.list
                        (\arg ->
                            JE.object
                                [ ( "name", JE.string arg.name )
                                , ( "args", JE.list encode arg.args )
                                ]
                        )
                        args.variants
                  )
                ]

        Recursive name ->
            JE.object
                [ ( "type", JE.string "Recursive" )
                , ( "name", JE.string name )
                ]


encode_Sanitize : Sanitize -> Value
encode_Sanitize op =
    case op of
        Trim ->
            JE.string "Trim"

        Lowercase ->
            JE.string "Lowercase"

        Uppercase ->
            JE.string "Uppercase"
