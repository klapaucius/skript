module SKrIpt.PrimLib where

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

type Env = Map String Any

defEnv :: Env
defEnv = Map.fromList [
    ("prelude#undefined", undefined),
    ("prelude#cons", unsafeCoerce (:)),
    ("prelude#nil", unsafeCoerce []),
    ("prelude#null", unsafeCoerce (null :: [a] -> Bool)),
    ("prelude#head", unsafeCoerce head),
    ("prelude#tail", unsafeCoerce tail),
    ("prelude#PlusInteger", unsafeCoerce ((+) :: Integer -> Integer -> Integer)),
    ("prelude#MinusInteger", unsafeCoerce ((-) :: Integer -> Integer -> Integer)),
    ("prelude#TimesInteger", unsafeCoerce ((*) :: Integer -> Integer -> Integer)),
    ("prelude#EqInteger", unsafeCoerce ((==) :: Integer -> Integer -> Bool))
    ]