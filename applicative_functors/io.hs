-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)
main = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you said " ++ line ++ " backwards!"