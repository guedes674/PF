--1
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
--a
calcula :: ExpInt -> Int
calcula e = case e of
            Const a -> a
            Simetrico a -> - (calcula a)
            Mais a b -> (calcula a) + (calcula b)
            Menos a b -> (calcula a) - (calcula b)
            Mult a b -> (calcula a) * (calcula b)

--b
infixa :: ExpInt -> String
infixa e = case e of
           Const a -> show a
           Simetrico a -> "-" ++ infixa a
           Mais a b -> "(" ++ infixa a ++ " + " ++ infixa b ++ ")"
           Menos a b -> "(" ++ infixa a ++ " - " ++ infixa b ++ ")"
           Mult a b -> "(" ++ infixa a ++ " * " ++ infixa b ++ ")"

--c
posfixa :: ExpInt -> String
posfixa e = case e of
            Const a -> show a
            Simetrico a -> posfixa a ++ " -"
            Mais a b -> posfixa a ++ " " ++ posfixa b ++ " +"
            Menos a b -> posfixa a ++ " " ++ posfixa b ++ " -"
            Mult a b -> posfixa a ++ " " ++ posfixa b ++ " *"
