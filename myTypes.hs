module Shapes
(Point(..),
Shape(..),
area,
nudge,
baseCircle,
baseRect) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- whoa... constructors are functions, and the 'fields' are the args...

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy  = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy
    = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Person

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavor :: String } deriving (Show)

-- this autogenerates accessor functons
bob = Person {firstName="bob",
              lastName="bobson",
              age=37,
              height=5,
              phoneNumber="111-222",
              flavor="blue"}


-- This is my truth...

data MyBool = T | F
    deriving (Show)

not' :: MyBool -> MyBool
not' T = F
not' F = T

or' :: MyBool -> MyBool -> MyBool
or' T _ = T
or' F x = x

and' :: MyBool -> MyBool -> MyBool
and' F _ = F
and' T x = x
