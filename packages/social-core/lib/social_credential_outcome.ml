type 'a t = Reused of 'a | Renewed of 'a

let map f = function
  | Reused value -> Reused (f value)
  | Renewed value -> Renewed (f value)

let value = function Reused value | Renewed value -> value
