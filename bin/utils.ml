type direction_t =
	| Right
	| Left

type vector_t = { mutable x: int; mutable y: int }

let tuple_of_vector vec = (vec.x, vec.y)

let rect_add_vector (rect: Sdl.Rect.t) (vec: vector_t) =
	{ rect with x = rect.x + vec.x; y = rect.y + vec.y }

type color_t = { r: int; g: int; b: int; a: int }

let radian_of_degree degrees = ((float_of_int degrees) *. Float.pi) /. 180.

let max_int_list li =
	List.fold_left (fun m el -> max m el) 0 li

let min_int_list li =
	List.fold_left (fun m el -> min m el) 0 li

let max_float_list li =
	List.fold_left (fun m el -> max m el) 0. li

let min_float_list li =
	List.fold_left (fun m el -> min m el) 0. li

let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

let id a = a

let (%) f g x = f (g x)

