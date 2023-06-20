module Color = struct
	type t = { r: int; g: int; b: int; a: int }

	let to_tuple color = (color.r, color.g, color.b, color.b)

	let to_sdlttf_color ({r; g; b; a}: t) : Sdlttf.color = { r; g; b; a }

	let of_sdlttf_color ({r; g; b; a}: Sdlttf.color) : t = { r; g; b; a }

	let set_draw_color renderer color = Sdlrender.set_draw_color3 renderer
		~r:color.r ~g:color.g ~b:color.b ~a:color.a;
end

type direction_t =
	| Right
	| Left

module Vector = struct
	type t = { mutable x: int; mutable y: int }

	let create x y = { x; y }

	let to_tuple (vec: t) = (vec.x, vec.y)

	let rect_add (rect: Sdl.Rect.t) (vec: t) =
		{ rect with x = rect.x + vec.x; y = rect.y + vec.y }

	let add vec1 vec2 = { x = vec1.x + vec2.x; y = vec1.y + vec2.y }
end

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

