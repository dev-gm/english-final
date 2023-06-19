open Raylib

let text_box bg_color text_color text x y width font_size font font_ratio =
	let line_max_length = (width /. (font_size *. font_ratio))
		|> int_of_float in
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
	let padding = width *. 0.05 in (*TODO: PADDING*)
	let width = (2. *. padding) +. width in
	let height = padding +.
		((font_size +. padding) *. (List.length lines |> float_of_int)) in
	draw_rectangle
		(int_of_float x)
		(int_of_float y)
		(int_of_float width)
		(int_of_float height)
		bg_color;
	List.iteri (fun i line ->
		draw_text_ex font line
			(Vector2.create
				(x +. padding)
				(y +. ((font_size +. padding) *. (float_of_int i)) +. padding))
			font_size 1. text_color (*TODO: SPACING*)
	) lines;
	Rectangle.create x y width height

(* returns if not done *)
let dialog y font font_ratio (dialog: Sprite.dialog_t) : bool =
	dialog.left_until_next_char <- dialog.left_until_next_char - 1;
	if dialog.left_until_next_char <= 0 then
		dialog.left <- dialog.left - 1;
		if dialog.left > 0 then
			y := !y +. 50.;
			let name_rect = text_box Color.lightgray Color.black
				dialog.sprite_name 50. !y 450. 24. font font_ratio in
			let contents_rect =
				text_box Color.white Color.black
					(String.sub dialog.contents
						0 ((String.length dialog.contents) - dialog.left))
					75. (!y +. (Rectangle.y name_rect) -. 1.)
					450. 16. font font_ratio
			in
			y := !y +. (Rectangle.y name_rect) +. (Rectangle.y contents_rect);
		dialog.left_until_next_char <- dialog.time_until_next_char;
	dialog.left > 0

let sprite
	(sprite: Sprite.t)
	(spritesheet: Texture2D.t)
	(scene_spritesheet: Texture2D.t)
=
	if sprite.visible then
		match Array.get sprite.src_rects sprite.src_rect with
		| Spritesheet (use_scene_spritesheet, src_rect) ->
				let spritesheet =
					if use_scene_spritesheet then
						scene_spritesheet
					else
						spritesheet
				in
				prerr_endline "x: ";
				prerr_float (Rectangle.x sprite.dest_rect);
				prerr_newline ();
				prerr_endline "y: ";
				prerr_float (Rectangle.y sprite.dest_rect);
				prerr_newline ();
				prerr_endline "width: ";
				prerr_float (Rectangle.width sprite.dest_rect);
				prerr_newline ();
				prerr_endline "height: ";
				prerr_float (Rectangle.height sprite.dest_rect);
				prerr_newline ();
				prerr_newline ();
				draw_texture_pro spritesheet
					src_rect sprite.dest_rect
					(Vector2.create 0. 0.) 0.
					(Color.create 0 0 0 255)
		| Color color ->
				draw_rectangle_rec sprite.dest_rect color

let game (game: Game.t) =
	List.iter (fun (_, (s: Sprite.t)) ->
		if s.visible then
			let scene_spritesheet =
				(Array.get game.scenes game.current_scene).spritesheet
					|> Array.get game.scene_spritesheets
			in
			sprite s game.spritesheet scene_spritesheet
	) (Hashtbl.fold (fun name sprite_index full_render_order ->
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
	) game.sprite_names []);
	let dialog_y = ref 0. in
	game.dialogs <- List.filter (dialog dialog_y game.font game.font_ratio) game.dialogs

