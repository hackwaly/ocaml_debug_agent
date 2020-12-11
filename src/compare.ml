type 'a t = 'a -> 'a -> int

type ordering = Ascending | Descending

let by ?(compare = compare) f a b = compare (f a) (f b)

let reverse compare a b = compare b a
