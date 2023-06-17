open Raylib

type direction_t =
	| Right
	| Left

type dialog_t = {
	sprite_name: string;
	contents: string;
	mutable left: int;
	speed: int; (* every tick, render letters of dialog based on speed. left is number of letters left to read. at end, check if left = 0. if so, reset dialog. *)
}

module Sprite = struct
	type action_t =
		| ShortAttack of float (*angle*)
		| LongAttack of direction_t
		| Jump
		| StopMoving (*only send this if stats.moving = Some*)
		| Move of direction_t
		| Speak of (string * int option) (*contents, allow_input*)
		| EnterScene of Vector2.t (* Enter is probably a Key *)
		| Die
		| ExitScene
		| NoAction

	type stats_constant_t = {
		gravity_accel: float;
		move_speed: float;
		jump_speed: float;
		long_attack_speed: float;
		long_attack_time: int;
		short_attack_range: float;
		short_attack_animation_time: int;
		dialog_speed: int
	}

	type stats_change_on_level_t = {
		(*level: int;
		exp_to_advance_level: int; (* increases as bosses get harder *)*)
		long_attack: int;
		short_attack: int;
		defense: int;
		health: int;
		energy: int;
	}

	type stats_t = {
		constant: stats_constant_t;
		mutable change_on_level: stats_change_on_level_t;
		mutable velocity: Vector2.t; (* every tick it is modified based on angle, accel, air_resistance. then position is moved by velocity *)
		mutable jump_quota: int; (* decrease by 1 during jump. can't jump if is less than or equal to 0. when collide with ground reset to 2 *)
		mutable energy: int; (* attacks decrease this. short attack decrease it in a small way. long attacks more. replenished by a certain amount every tick*)
		mutable health: int;
		(*mutable exp: int; (* every time it's increased it is checked against stats.exp_to_advance_level to decide whether to increase level *)*)
		mutable moving: direction_t option; (*if Move Right/Left is sent, check whether player already moving in that direction. if so, don't do anything. otherwise, move *)
	}

	type type_t =
		| Player
		| Bot of (unit -> action_t list)
		| Other
		
	type t = {
		name: string;
		team: int;
		sprite_type: type_t;
		src_rects: Rectangle.t Array.t; (* on spritesheet *)
		mutable src_rect: int; (* index of spritsheet_locations *)
		mutable dest_rect: Rectangle.t;
		mutable long_attack: (direction_t * int) option; (* long attack, don't allow inputs while this is happening. during it the player is invincible, and they pass through any player (not other) on either side, dealing damage. this means that if the player collides with any bot while long attacking, that bot takes damage for the duration of the long attack. int counts down until it gets to 0, at which point long attack stops. during the entirety of the long attack, player moves at long_attack_speed in 'direction' direction *)
		mutable short_attack: float option; (*angle. next tick, short_attack is reset to None*)
		mutable dialog: dialog_t option;
		mutable visible: bool;
		mutable dead: bool;
		mutable z_index: int;
		mutable use_scene_spritesheet: bool;
		stats: stats_t
	}

	let send_action sprite action =
		match action with
		| ShortAttack angle ->
				sprite.short_attack <- Some angle
		| LongAttack direction ->
				sprite.long_attack <- Some
					(direction, sprite.stats.constant.long_attack_time)
		| Jump ->
				if sprite.stats.jump_quota > 0 then
					Vector2.set_y sprite.stats.velocity
						sprite.stats.constant.jump_speed;
					sprite.stats.jump_quota <-
						sprite.stats.jump_quota - 1
		| Move direction ->
				let speed = sprite.stats.constant.move_speed *.
					(match direction with
						| Left -> -1.
						| Right -> 1.)
				in Vector2.set_x sprite.stats.velocity speed
		| StopMoving -> Vector2.set_x sprite.stats.velocity 0.
		| Speak (contents, speed_option) ->
				let dialog = {
					sprite_name = sprite.name;
					contents;
					left = String.length contents;
					speed = match speed_option with
						| Some speed -> speed
						| None -> sprite.stats.constant.dialog_speed
				} in sprite.dialog <- Some dialog
		| EnterScene start_pos ->
				let open Vector2 in
				Rectangle.set_x sprite.dest_rect (x start_pos);
				Rectangle.set_y sprite.dest_rect (y start_pos);
				sprite.visible <- true
		| ExitScene ->
				sprite.visible <- false
		| Die ->
				sprite.visible <- false;
				sprite.dead <- true
		| NoAction -> ()

	(*angle IS IN RADIANS, unit circle*)
	let find_closest_enemy_in_direction_and_range
		(sprite: t)
		(angle: float)
		(range: float)
		(sprites: t Array.t)
		: t option
	=
		let center =
			let open Rectangle in
			let f f1 f2 = (f1 sprite.dest_rect) +. ((f2 sprite.dest_rect) /. 2.) in
			Vector2.create (f x width) (f y height)
		in
		let vector_end =
			let f f1 t1 = (f1 center) +. (range *. (t1 angle)) in
			let open Vector2 in create (f x cos) (f y sin)
		in
		let i = ref 0 in
		Array.get (Array.fold_left
			(fun filtered sprite ->
				let line_between_points vf rf =
					(((vf center) < (rf sprite.dest_rect)) &&
					((vf vector_end) > (rf sprite.dest_rect))) ||
					(((vf center) > (rf sprite.dest_rect)) &&
					((vf vector_end) < (rf sprite.dest_rect)))
				in
				if (line_between_points Vector2.x Rectangle.x) &&
					(line_between_points Vector2.y Rectangle.y)
				then
					Array.unsafe_set filtered !i (Some sprite);
					i := !i + 1;
				filtered
			) (Array.make ((Array.length sprites) + 1) None) sprites) 0

	let correct_movement_for_collision rect other_rect (mvmt: Vector2.t) =
		let open Rectangle in
		let in_between_points vf rf1 rf2 =
			let a = ((rf1 rect) +. (vf mvmt)) -. (rf1 other_rect) in
			let b = ((rf1 other_rect) +. (rf2 other_rect)) -.
				((rf1 rect) +. (rf2 rect) +. (vf mvmt))
			in (a, b)
		in
		let (left_over, right_over) = in_between_points Vector2.x x width in
		let (top_over, bottom_over) = in_between_points Vector2.y y height in
		if (left_over > 0. && right_over > 0. &&
			top_over > 0. && bottom_over > 0.) then
			(Vector2.create
				(if (left_over > right_over) then
					(Vector2.x mvmt) -. left_over
				else
					(Vector2.x mvmt) +. right_over)
				(if (top_over > bottom_over) then
					(Vector2.y mvmt) -. top_over
				else
					(Vector2.y mvmt) +. bottom_over),
			true)
		else
			(mvmt, false)

	(* returns: new_movement, collided_with_ground(or any Other sprite in a downwards direction) *)
	let correct_movement_for_collisions sprite movement sprites : Vector2.t * bool =
		Array.fold_right (fun other_sprite (movement, collide_with_ground) ->
			let (new_movement, _) = correct_movement_for_collision
				sprite.dest_rect other_sprite.dest_rect movement in
			let collide_with_ground = collide_with_ground ||
				(((Vector2.y new_movement) < (Vector2.y movement)) &&
				other_sprite.sprite_type = Other) in
			(new_movement, collide_with_ground)
		) sprites (movement, false)

	let correct_movement_for_long_attack_collisions
		(sprite: t)
		(movement: Vector2.t)
		(sprites: t Array.t)
		: Vector2.t * t list
	=
		Array.fold_right (fun other_sprite (movement, enemies) ->
			let (new_movement, corrected) = correct_movement_for_collision
				sprite.dest_rect other_sprite.dest_rect movement in
			if corrected then
				match other_sprite.sprite_type with
				| Bot _ | Player when sprite.team <> other_sprite.team ->
						(movement, other_sprite :: enemies)
				| _ -> (new_movement, enemies)
			else
				(movement, enemies)
		) sprites (movement, [])

	let render sprite spritesheet =
		if sprite.visible then
			draw_texture_pro
				spritesheet
				(Array.get
					sprite.src_rects
					sprite.src_rect)
				sprite.dest_rect
				(Vector2.create 0. 0.) 0.
				(Color.create 0 0 0 255)
end

type scene_contents_t =
	| Planned of (int, string * Sprite.action_t) Hashtbl.t (* tick, (sprite, action). every tick this hashtbl is checked *)
	| Fight

type scene_t = {
	spritesheet: int; (* index of scene_spritesheets *)
	contents: scene_contents_t;
	mutable next_scene: int option (* if None, end game *)
}

(*type animation_t =
	| Attack of (string * int)*)

type game_t = {
	mutable tick: int;
	mutable scene_tick: int;
	mutable switch_scene: bool;
	mutable should_exit: bool;
	sprites: Sprite.t Array.t;
	sprite_names: (string, int) Hashtbl.t;
	spritesheet: Texture2D.t;
	scene_spritesheets: Texture2D.t Array.t;
	scenes: scene_t Array.t;
	mutable current_scene: int;
	(*mutable animations: animation_t Array.t;*)
	mutable dialogs: dialog_t list;
	font: Font.t;
	font_ratio: float (*width/height*)
}

let bot_ai () : Sprite.action_t list = []

module Config = struct
	open Yojson.Basic.Util

	type sprite_type_config_t =
		| Player
		| Bot of int (*builtin ai programs*)
		| Other

	let sprite_type_config_of_json json =
		match json |> member "type" |> to_string with
		| "Player" -> Player
		| "Bot" -> Bot (json |> member "Bot" |> to_int)
		| _ -> Other

	let sprite_type_of_config config : Sprite.type_t =
		match config with
		| Player -> Player
		| Bot _ -> Bot bot_ai
		| Other -> Other

	type vector2_config_t = {
		x: int;
		y: int
	}

	let vector2_config_of_json json : vector2_config_t = {
		x = json |> member "x" |> to_int;
		y = json |> member "y" |> to_int
	}

	let vector2_of_config config = Vector2.create
		(float_of_int config.x) (float_of_int config.y)

	type rectangle_config_t = {
		x: int;
		y: int;
		width: int;
		height: int;
	}

	let rectangle_config_of_json json : rectangle_config_t = {
		x = json |> member "x" |> to_int;
		y = json |> member "y" |> to_int;
		width = json |> member "width" |> to_int;
		height = json |> member "height" |> to_int
	}

	let rectangle_of_config config = Rectangle.create
		(float_of_int config.x) (float_of_int config.y)
		(float_of_int config.width) (float_of_int config.height)

	let stats_constant_t_of_json json : Sprite.stats_constant_t = {
		gravity_accel = json |> member "gravity_accel" |> to_float;
		move_speed = json |> member "move_speed" |> to_float;
		jump_speed = json |> member "jump_speed" |> to_float;
		long_attack_speed = json |> member "long_attack_speed" |> to_float;
		long_attack_time = json |> member "long_attack_time" |> to_int;
		short_attack_range = json |> member "short_attack_range" |> to_float;
		short_attack_animation_time = json |> member "short_attack_animation_time"
			|> to_int;
		dialog_speed = json |> member "dialog_speed" |> to_int
	}

	let stats_change_on_level_t_of_json json : Sprite.stats_change_on_level_t = {
		long_attack = json |> member "long_attack" |> to_int;
		short_attack = json |> member "short_attack" |> to_int;
		defense = json |> member "defense" |> to_int;
		health = json |> member "health" |> to_int;
		energy = json |> member "energy" |> to_int
	}

	type sprite_config_t = {
		name: string;
		team: int;
		sprite_type: sprite_type_config_t;
		src_rects: rectangle_config_t list;
		src_rect: int;
		dest_rect: rectangle_config_t;
		visible: bool;
		z_index: int;
		use_scene_spritesheet: bool;
		stats_constants: Sprite.stats_constant_t;
		stats_change_on_level: Sprite.stats_change_on_level_t
	}

	let sprite_config_of_json json : sprite_config_t = {
		name = json |> member "name" |> to_string;
		team = json |> member "team" |> to_int;
		sprite_type = json |> member "sprite_type" |> sprite_type_config_of_json;
		src_rects = json |> member "src_rects" |> to_list
			|> List.map rectangle_config_of_json;
		src_rect = json |> member "src_rect" |> to_int;
		dest_rect = json |> member "dest_rect" |> rectangle_config_of_json;
		visible = json |> member "visible" |> to_bool;
		z_index = json |> member "z_index" |> to_int;
		use_scene_spritesheet = json |> member "use_scene_spritesheet" |> to_bool;
		stats_constants = json |> member "stats_constants"
			|> stats_constant_t_of_json;
		stats_change_on_level = json |> member "stats_change_on_level"
			|> stats_change_on_level_t_of_json
	}

	let sprite_of_config config : Sprite.t = {
		name = config.name;
		team = config.team;
		sprite_type = sprite_type_of_config config.sprite_type;
		src_rects = Array.of_list config.src_rects
			|> Array.map rectangle_of_config;
		src_rect = config.src_rect;
		dest_rect = rectangle_of_config config.dest_rect;
		long_attack = None;
		short_attack = None;
		dialog = None;
		visible = config.visible;
		dead = false;
		z_index = config.z_index;
		use_scene_spritesheet = config.use_scene_spritesheet;
		stats = {
			constant = config.stats_constants;
			change_on_level = config.stats_change_on_level;
			velocity = Vector2.create 0. 0.;
			jump_quota = 2;
			energy = config.stats_change_on_level.energy;
			health = config.stats_change_on_level.health;
			moving = None
		}
	}

	let direction_of_bool config = if config then Right else Left

	type sprite_action_config_t =
		| ShortAttack of float
		| LongAttack of bool (*true=right, false=left*)
		| Jump
		| StopMoving
		| Move of bool
		| Speak of (string * int option) (*contents, allow_input*)
		| EnterScene of vector2_config_t
		| Die
		| ExitScene
		| NoAction

	let sprite_action_config_of_json json : sprite_action_config_t =
		match json |> member "type" |> to_string with
		| "ShortAttack" -> ShortAttack (json |> member "ShortAttack" |> to_float)
		| "LongAttack" -> LongAttack (json |> member "LongAttack" |> to_bool)
		| "Jump" -> Jump
		| "StopMoving" -> StopMoving
		| "Move" -> Move (json |> member "Move" |> to_bool)
		| "Speak" ->
				let speak = json |> member "Speak" in
				Speak
					(speak |> member "contents" |> to_string,
					speak |> member "allow_input" |> to_int_option)
		| "EnterScene" -> EnterScene (json |> member "EnterScene" |> vector2_config_of_json)
		| "Die" -> Die
		| "ExitScene" -> ExitScene
		| _ -> NoAction

	let scene_action_of_config config : Sprite.action_t = 
		match config with
		| ShortAttack angle -> ShortAttack angle
		| LongAttack direction -> LongAttack (direction_of_bool direction)
		| Jump -> Jump
		| StopMoving -> StopMoving
		| Move direction -> Move (direction_of_bool direction)
		| Speak speak -> Speak speak
		| EnterScene vec -> EnterScene (vector2_of_config vec)
		| Die -> Die
		| ExitScene -> ExitScene
		| NoAction -> NoAction

	type scene_contents_config_t =
		| Planned of (int * string * sprite_action_config_t) list
		| Fight

	let scene_contents_config_json json : scene_contents_config_t =
		match json |> member "type" |> to_string with
		| "Planned" -> Planned
			(json |> member "Planned" |> to_list |> List.map (fun elem ->
				(elem |> member "tick" |> to_int,
				elem |> member "sprite" |> to_string,
				elem |> member "action" |> sprite_action_config_of_json)))
		| _ -> Fight

	let scene_contents_of_config config : scene_contents_t =
		match config with
		| Planned li ->
				let hashtbl = Hashtbl.create (List.length li) in
				List.iter (fun (tick, sprite, action) ->
					Hashtbl.add hashtbl tick
						(sprite,scene_action_of_config action)
				) li;
				Planned hashtbl
		| Fight -> Fight


	type scene_config_t = {
		spritesheet: int;
		contents: scene_contents_config_t;
		next_scene: int option
	}

	let scene_config_of_json json : scene_config_t = {
		spritesheet = json |> member "spritesheet" |> to_int;
		contents = json |> member "contents"
			|> scene_contents_config_json;
		next_scene = json |> member "next_scene" |> to_int_option
	}

	let scene_of_config config : scene_t = {
		spritesheet = config.spritesheet;
		contents = scene_contents_of_config config.contents;
		next_scene = config.next_scene
	}

	type t = {
		sprites: sprite_config_t list;
		spritesheet: string;
		scene_spritesheets: string list;
		scenes: scene_config_t list; (* first scene is index 0 *)
		font: string;
		font_ratio: float
	}

	(*let title = json |> member "title" |> to_string in*)
	let of_json json = {
		sprites = json |> member "sprites" |> to_list
			|> List.map sprite_config_of_json;
		spritesheet = json |> member "spritesheet" |> to_string;
		scene_spritesheets = json |> member "scene_spritesheets"
			|> to_list |> List.map to_string;
		scenes = json |> member "scenes" |> to_list
			|> List.map scene_config_of_json;
		font = json |> member "font" |> to_string;
		font_ratio = json |> member "font_ratio" |> to_float
	}

	let to_game config : game_t =
		let sprite_names = Hashtbl.create (List.length config.sprites) in
		List.iteri (fun i sprite ->
			Hashtbl.add sprite_names sprite.name i
		) config.sprites;
		{
			tick = 0;
			scene_tick = 0;
			switch_scene = false;
			should_exit = false;
			sprites = Array.map sprite_of_config (Array.of_list config.sprites);
			sprite_names;
			spritesheet = load_texture config.spritesheet;
			scene_spritesheets = Array.map load_texture
				(Array.of_list config.scene_spritesheets);
			scenes = Array.map scene_of_config (Array.of_list config.scenes);
			current_scene = 0;
			dialogs = [];
			font = load_font config.font;
			font_ratio = config.font_ratio
		}
end

module Render = struct
	let text_box bg_color text_color text x y width font_size font_ratio =
		let line_max_length = int_of_float ((float_of_int width) /.
			((float_of_int font_size) *. font_ratio)) in
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
		let padding = int_of_float ((float_of_int width) *. 0.05) in (*TODO*)
		let width = (2 * padding) + width in
		let height = padding + ((font_size + padding) * (List.length lines)) in
		draw_rectangle x y width height bg_color;
		List.iteri (fun i line ->
			draw_text line
				(x + padding)
				(y + ((font_size + padding) * i) + padding)
				font_size text_color
		) lines;
		Rectangle.create
			(float_of_int x)
			(float_of_int y)
			(float_of_int width)
			(float_of_int height)

	(* returns if not done *)
	let dialog font_ratio (dialog: dialog_t) : bool =
		dialog.left <- dialog.left - 1;
		if dialog.left > 0 then
			ignore
				(text_box Color.white Color.black
					(String.sub dialog.contents
						0 ((String.length dialog.contents) - dialog.left))
					25 25 600 24 font_ratio);
		dialog.left > 0

	let game game =
		List.iter (fun (_, (sprite: Sprite.t)) ->
			if sprite.visible then
				let spritesheet =
					if sprite.use_scene_spritesheet then
						(Array.get game.scene_spritesheets
							(Array.get game.scenes game.current_scene).spritesheet)
					else
						game.spritesheet
				in
				Sprite.render sprite spritesheet
			else
				())
		(Hashtbl.fold (fun name sprite_index full_render_order ->
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
				| [] -> []
			in
			sort_render_order full_render_order
		) game.sprite_names []);
		game.dialogs <- List.filter (dialog game.font_ratio) game.dialogs
end

let get_user_input () : Sprite.action_t list =
	let actions = ref [] in
	let map_key_input input action =
		if is_key_down input then
			actions := action :: !actions;
	in
	let open Key in
	let open Sprite in
	map_key_input Space Jump;
	map_key_input A (Move Left);
	map_key_input D (Move Right);
	[]

(*
- animations queue (type, countdown to end): (NO)
	- short_attack
- dialog queue:
	- dialog_t
- if $long_attack:
	- $time -= 1
	- if $time <= 0:
		- long_attack = none
		- break 'long-attack-if
	- correct $long_attack_speed based on collision
		- use custom function (not normal correction function) so that
			- if collide with other object with z-index greater, stop
			- if collide with opponent (on opposite team), no matter the z-index, continue but deal attack to that opponent
				- if opponent's health <= 0:
					- opponent.die()
	- break 'all (sprite can't do anything while long attacking)
- accept input from player, if player. if bot, accept input from Bot
- if $short_attack:
	- deal attack to nearest opponent based on range and direction
	- if opponent's health <= 0
		- opponent.die()
	- add short_attack animation to animations queue (NO)
	- $short_attack = None
- if $dialog:
	- add $dialog to dialog queue
	- $dialog = None
- change $velocity based on $gravity_accel
- change $velocity based on $air_resistance (NO)
- correct movement (from $velocity) based on collision
	- if collide with ground: reset $jump_quota
	- move by movement
 *)
let tick_sprite (game: game_t) (sprite: Sprite.t) =
	match sprite.long_attack with
	| Some (direction, time_left) ->
			if (time_left - 1) <= 0 then
				sprite.long_attack <- None
			else
				sprite.long_attack <-
					Some (direction, time_left - 1);
				let (movement, collided_enemies) =
					Sprite.correct_movement_for_long_attack_collisions
					sprite
					(Vector2.create
						(sprite.stats.constant.long_attack_speed *.
							(match direction with
								| Right -> 1.
								| Left -> -1.)) 0.)
					game.sprites
				in
				let open Rectangle in
				set_x sprite.dest_rect ((x sprite.dest_rect) +. (Vector2.x movement));
				set_y sprite.dest_rect ((y sprite.dest_rect) +. (Vector2.y movement));
				List.iter (fun (enemy: Sprite.t) ->
					enemy.stats.health <- enemy.stats.health -
						sprite.stats.change_on_level.long_attack;
					if enemy.stats.health <= 0 then
						Sprite.send_action enemy Die;
				) collided_enemies
	| None ->
			let actions = match sprite.sprite_type with
				| Player -> get_user_input ()
				| Bot get_actions -> get_actions ()
				| Other -> []
			in
			List.iter (Sprite.send_action sprite) actions;
			match sprite.short_attack with
			| Some angle ->
					let enemy =
						Sprite.find_closest_enemy_in_direction_and_range
						sprite angle sprite.stats.constant.short_attack_range game.sprites
					in
					ignore (match enemy with
							| Some enemy ->
								enemy.stats.health <- enemy.stats.health -
									sprite.stats.change_on_level.short_attack;
								if enemy.stats.health <= 0 then
									ignore (Sprite.send_action enemy Die)
							| None -> ());
					sprite.short_attack <- None
			| None -> ();
			match sprite.dialog with
			| Some dialog ->
					game.dialogs <- dialog :: game.dialogs;
					sprite.dialog <- None
			| None -> ();
			sprite.stats.velocity <- Vector2.add sprite.stats.velocity
				(Vector2.create 0. sprite.stats.constant.gravity_accel);
			let (movement, collided_with_ground) =
				Sprite.correct_movement_for_collisions
					sprite
					sprite.stats.velocity
					game.sprites
			in
			if collided_with_ground then
				sprite.stats.jump_quota <- 2;
			sprite.stats.velocity <- movement;
			let open Rectangle in
			set_x sprite.dest_rect ((x sprite.dest_rect) +. (Vector2.x movement));
			set_y sprite.dest_rect ((x sprite.dest_rect) +. (Vector2.y movement))

let tick_planned_scene game actions =
	match Hashtbl.find_opt actions game.scene_tick with
	| Some (sprite_name, action) ->
			let sprite_index = Hashtbl.find game.sprite_names sprite_name in
			let sprite = Array.get game.sprites sprite_index in
			Sprite.send_action sprite action
	| _ -> ()

let tick_game game =
	let current_scene = Array.get game.scenes game.current_scene in
	match current_scene.contents with
	| Planned actions -> tick_planned_scene game actions
	| Fight -> Hashtbl.iter (fun _ sprite_index ->
			tick_sprite game (Array.get game.sprites sprite_index)) game.sprite_names;
	game.tick <- game.tick + 1;
	if game.switch_scene then
		match current_scene.next_scene with
		| Some next_scene -> game.current_scene <- next_scene
		| None -> game.should_exit <- true;

	game.scene_tick <- game.scene_tick + 1

let save_game_data (_: game_t) = ()

let setup () =
	init_window 1280 720 "English Final";
	set_target_fps 60;
	let filename = Array.get Sys.argv 1 in
	let json = Yojson.Basic.from_file filename in
	json |> Config.of_json |> Config.to_game

let rec loop game =
	match window_should_close () || game.should_exit with
	| true ->
			save_game_data game;
			close_window ()
	| false ->
			begin_drawing ();
			tick_game game;
			Render.game game;
			end_drawing ();
			loop game

let () = setup () |> loop
