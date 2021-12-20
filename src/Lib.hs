module Lib
    ( someFunc
    ) where

import Debug.Trace
import Control.Arrow (Arrow(first))


data Operation = Add | Sub deriving (Show)

getOperation :: String -> Operation
getOperation "+" = Add
getOperation "-" = Sub
getOperation inp =
    let operationString =  [ x | x <- inp, x == '+' || x == '-']
    in getOperation operationString

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : splitWhen p s''
          where (w, s'') = break p s'

operationToString :: Operation -> (Char -> Bool)
operationToString Add = (=='+')
operationToString Sub = (=='-')

performOperation :: Operation -> [String ] -> Float
performOperation op inp =
    let firstString = head inp
        lastString  = last inp
        firstNum    = read firstString :: Float
        lastNum     = read lastString  :: Float
    in case op of
            Add -> firstNum + lastNum
            Sub -> firstNum - lastNum

calculate :: String -> Float
calculate line =
    let op = getOperation line
        split = operationToString op
        inp = splitWhen split line
    in performOperation op inp

someFunc :: IO ()
someFunc = do
    putStrLn "What is the operation? "
    operationString <- getLine
    let operation = calculate operationString
    putStrLn ("Result => " ++ show operation)
