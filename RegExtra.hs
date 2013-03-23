module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
    (===) :: a -> a -> Bool

instance Eq c => Equiv (Reg c) where
    r1 === r2 = let
            r1' = simpl r1
            r2' = simpl r2
        in r1' `eq` r2' || r2' `eq` r1' where
            eq x@((r1 :> r2) :> r3) y@(r4 :> (r5 :> r6)) = 
                (x == y) || (r1 == r4 && r2 == r5 && r3 == r6)
            eq r1 r2 = r1 == r2

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

-- partial helper function
simpl_alt :: Eq c => Reg c -> Reg c
simpl_alt (r1 :| r2) = if r1 == r2
    then simpl r1
    else case r1 of
        Empty      -> simpl r2
        r11 :| r12 -> case r2 of
            Empty      -> simpl r1
            r21 :| r22 ->
                if r11 == r21 || r12 == r21
                    then (simpl r11) :| (simpl r12) :| (simpl r22)
                    else if r11 == r22 || r12 == r22
                        then (simpl r11) :| (simpl r12) :| (simpl r21)
                        else simpl r1 :| simpl r2
            _          -> 
                if r2 == r11 || r2 == r12
                    then simpl r1
                    else (simpl r1) :| (simpl r2)
        r11 :> r12 -> case r2 of
            Empty      -> simpl r1
            r21 :> r22 ->
                if r21 == r11
                    then (simpl r11) :> ((simpl r12) :| (simpl r22))
                    else if r22 == r12
                        then ((simpl r11) :| (simpl r21)) :> (simpl r12)
                        else (simpl r1) :| (simpl r2)
            _          -> (simpl r1) :| (simpl r2)
        _          -> case r2 of
            Empty      -> simpl r1
            r21 :| r22 ->
                if r22 == r1 || r21 == r1
                    then simpl r2
                    else (simpl r1) :| (simpl r2)
            _          -> (simpl r1) :| (simpl r2)

simpl :: Eq c => Reg c -> Reg c
simpl regex = case regex of
    Empty     -> Empty 
    Eps       -> Eps
    Lit a     -> Lit a
    Many r -> Many (simpl r)
    r1 :| r2  -> simpl_alt regex
    r1 :> r2  ->let
            r1' = simpl r1
            r2' = simpl r2
        in case r1' of
            Eps   -> r2'
            Empty -> Empty
            _     -> case r2' of
                Eps   -> r1'
                Empty -> Empty
                _     -> r1' :> r2'
   
nullable :: Reg c -> Bool
nullable x = case x of
    Empty    -> False
    Eps      -> True
    Lit _    -> False
    Many _   -> True
    r1 :| r2 -> nullable r1 || nullable r2
    r1 :> r2 -> nullable r1 && nullable r2

empty :: Reg c -> Bool 
empty x = case x of
    Empty -> True
    Eps     -> False
    Lit _   -> False
    Many _  -> False
    r1 :| r2 -> empty r1 && empty r2
    r1 :> r2 -> empty r1 || empty r2

der :: Eq c => c -> Reg c -> Reg c
der c r = case r of
        Empty    -> Empty
        Eps      -> Empty
        Lit x    -> 
            if x == c
                then Eps
                else Empty
        Many x   -> let x' = der c x
            in case x' of
                Eps   -> Many x
                Empty -> Empty
                _     -> x' :> (Many x)
        r1 :| r2 -> (der c r1) :| (der c r2)
        r1 :> r2 ->
            if nullable r1
                then ((der c r1) :> r2) :| (der c r2)
                else (der c r1) :> r2
    
ders :: Eq c => [c] -> Reg c -> Reg c
ders c r = foldl' (\r'->(\c'->der c' $ simpl r')) r c

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = case simpl r of
    Lit x    -> (x == c)
    Many r1  -> mayStart c r1
    r1 :> r2 -> 
        if nullable r1 || empty r1
            then mayStart c r1 || mayStart c r2
            else mayStart c r1
    r1 :| r2 -> mayStart c r1 || mayStart c r2
    _        -> False

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = let
        r' = simpl r
    in
        foldl (match' r') Nothing $ inits w where
            match' r' m w =
                if accepts r' w
                    then Just w
                    else m
      
search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = search' r $ tails w where
    search' r []     = Nothing
    search' r (w:ws) = case match r w of
        Nothing -> search' r ws
        Just x  -> Just x

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = reverse $ findAllPos r (tails w) [] where
    findAllPos r [] found     = found
    findAllPos r (w:ws) found = case match r w of
        Nothing -> findAllPos r ws found
        Just x  -> findAllPos r (drop (length x - 1) ws) (x:found)

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
