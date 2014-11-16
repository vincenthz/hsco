module View where

data View a b = [ViewElement a b]
data ViewElement a b = ViewElement a b

data Syntax = Import X Y Z | Other

makeView :: ([Atom String] -> (ViewElement Syntax [Atom String], [Atom String]))
         -> [Atom String]
         -> View Syntax [Atom String]
makeView = undefined
