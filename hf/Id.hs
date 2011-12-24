{- ---------------------------------------------------------------------------
This is the start for an abstract data type Id.
We need instance Eq Id and instance Ord Id.
Furthermore an IdSupply.
However, currently Int is used for that purpose and shall slowly be
replaced by the new type Id.
-}
module Id(Id) where

type Id = Int

{- End Module Id ------------------------------------------------------------}
