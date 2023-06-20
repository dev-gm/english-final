open Utils

let text_box (game: Game.t)
	(bg_color: Color.t) (text_color: Color.t)
	text x y w fonts font_size font_ratio
=
	let font = Hashtbl.find fonts font_size in
	let line_max_length = w / ((float_of_int font_size) *. font_ratio |> int_of_float) in
	let lines = 
		let (head, rest) = List.fold_right (fun word (head, rest) ->
			if ((String.length head) + 1 + (String.length word))
				>= line_max_length
			then
				(word, rest @ [head])
			else
				((String.concat " " [head; word]), rest)
		) (String.split_on_char ' ' text) ("", []) in rest @ [head]
	in
	let padding = (float_of_int w) *. 0.05 |> int_of_float in
	let rect =
		let w = (2 * padding) + w in
		let h = padding + ((font_size + padding) * (List.length lines)) in
		Sdlrect.make4 ~x ~y ~w ~h
	in
	Color.set_draw_color game.renderer bg_color;
	Sdlrender.fill_rect game.renderer rect;
	List.iteri (fun i line ->
		let surface = Sdlttf.render_text_solid
			font ~text:line ~color:(Color.to_sdlttf_color text_color) in
		let texture = Sdltexture.create_from_surface game.renderer surface in
		Sdlsurface.free surface;
		let dst_rect: Sdlrect.t = {
			x = rect.x + padding;
			y = rect.y + padding + ((font_size + padding) * i);
			w; h = font_size;
		} in
		Sdlrender.copy game.renderer ~texture:texture ~dst_rect ();
		Sdltexture.destroy texture;
	) lines;
	rect

(* returns if not done *)
let dialog game y fonts font_ratio (dialog: Sprite.dialog_t) : bool =
	dialog.left_until_next_char <- dialog.left_until_next_char - 1;
	if dialog.left_until_next_char <= 0 then
		dialog.left <- dialog.left - 1;
		if dialog.left > 0 then
			y := !y + 50;
			let name_rect = text_box game { r=0;g=0;b=0;a=0 } { r=255;g=255;b=255;a=0 }
				dialog.sprite_name 50 !y 450 fonts 24 font_ratio in
			let contents_rect =
				text_box game
					{ r=50;g=50;b=50;a=50 }
					{ r=255;g=255;b=255;a=0 }
					(String.sub dialog.contents
						0 ((String.length dialog.contents) - dialog.left))
					75 (!y + name_rect.y - 1) 450 fonts 16 font_ratio
			in
			y := !y + name_rect.y + contents_rect.y;
		dialog.left_until_next_char <- dialog.time_until_next_char;
	dialog.left > 0

let sprite (game: Game.t) (sprite: Sprite.t) =
	if sprite.visible then
		match Array.get sprite.src_rects sprite.src_rect with
		| Spritesheet src_rect ->
				Sdlrender.copy game.renderer
					~texture:game.spritesheet ~src_rect ~dst_rect:sprite.dest_rect ();
		| Color color ->
				Color.set_draw_color game.renderer color;
				Sdlrender.fill_rect game.renderer sprite.dest_rect

let game (game: Game.t) =
	Hashtbl.fold (fun name sprite_index full_render_order ->
		let sprite = Array.get game.sprites sprite_index in
		let sprite_in_list = ref false in
		let rec sort_render_order (render_order: (string * Sprite.t) list) =
			match render_order with
			| (elem_name, elem_sprite) :: xs ->
					sprite_in_list := elem_sprite.z_index >= sprite.z_index;
					if elem_sprite.z_index >= sprite.z_index then
						(name, sprite) :: (elem_name, elem_sprite) :: render_order
					else
						sort_render_order xs
			| [] when not !sprite_in_list -> [(name, sprite)]
			| _ -> []
		in
		sort_render_order full_render_order
	) game.sprite_names []
		|> List.iter (fun (_, s) -> sprite game s);
	let dialog_y = ref 0 in
	game.dialogs <- game.dialogs
		|> List.filter (dialog game dialog_y game.fonts game.font_ratio)

