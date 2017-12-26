data Formula
 = Atom Bool -- atomic formula
 | And Formula Formula -- f /\ f
 | Or Formula Formula -- f \/ f
 | Implies Formula Formula -- f -> f
 | Not Formula -- not(f)

eval :: Formula -> Bool
eval (Atom True) = True
eval (Atom False) = False
eval (And x y) = eval (x)&&eval(y)
eval (Or x y) = eval (x)||eval(y)
eval (Implies x y) = eval(x)<=eval(y)
eval (Not x) = not(eval (x))
