module UnsafeCoerce
  ( unsafeCoerce
  )
 where

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b
