-- | ENUM / ST
module Sound.SC3.Data.UI.ST where

import qualified Data.Map as Map {- containers -}

-- | USR action (STATE,IX) -> IO ST
type ACT st = ((st,(Int,Int)) -> IO st)

-- | (ROW-IX,(TXT,VAL))
type ENUM = Map.Map Int (String,Double)

-- | All TXT at ENUM (in sequence).
enum_txt :: ENUM -> [String]
enum_txt = map (fst . snd) . Map.toAscList

-- | ENUM from (TXT,VAL) sequence.
num_enum :: [(String,Double)] -> ENUM
num_enum lst = Map.fromList (zip [0..] lst)

-- | ENUM from TXT sequence, value is ROW-IX.
str_enum :: [String] -> ENUM
str_enum lst = num_enum (zip lst [0..])

-- | (COL-IX,(SEL-IX,ENUM))
type ST = Map.Map Int (Int,ENUM)

-- | All TXT for each COL in sequence.
st_txt :: ST -> [[String]]
st_txt = map (enum_txt . snd . snd) . Map.toAscList

-- | Get (SEL-IX,(TXT,VAL)) for COL-IX.
st_get :: ST -> Int -> (Int,(String,Double))
st_get st i =
  let get = Map.findWithDefault (error "st_get")
      (j,e) = get i st
  in (j,get j e)

-- | Get SEL-IX for COL-IX.
st_get_ix :: ST -> Int -> Int
st_get_ix st = fst . st_get st

-- | Get VAL for COL-IX.
st_get_val :: ST -> Int -> Double
st_get_val st = snd . snd . st_get st

-- | Set SEL-IX at COL-IX.
st_set :: ST -> Int -> Int -> ST
st_set st i j =
  let f (_,e) = (j,e)
  in Map.adjust f i st

-- | Lift ST action to ACT ST
st_act :: (ST -> IO ()) -> ACT ST
st_act f (st0,(x,y)) = do
  let st = st_set st0 x y
  f st
  return st
