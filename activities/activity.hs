class Dummy a where
    iAmAString :: a -> String
    iAmABool :: a -> Bool
    iAmADouble :: a -> Double

instance Dummy Int where
    iAmAString x = "int string"
    iAmABool x = True
    iAmADouble x = 23.0