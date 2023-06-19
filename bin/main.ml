open Raylib

type direction_t =
	| Right
	| Left

type dialog_t = {
	sprite_name: string;
	contents: string;
	mutable left: int;
	time_until_next_char: int; (* every tick, render letters of dialog based on speed. left is number of letters left to read. at end, check if left = 0. if so, reset dialog. *)
	mutable left_until_next_char: int; (*render when left = 0, reset to time*)
	allow_input: bool
}

module Sprite = struct
	type action_t =
		| ShortAttack of float (*angle*)
		| LongAttack of direction_t
		| Jump
		| StopMoving (*only send this if stats.moving = Some*)
		| Move of direction_t
		| Speak of (string * bool option) (*contents, allow_input*)
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
		dialog_time_until_next_char: int;
		long_attack_energy: int;
		short_attack_energy: int;
		animation_frame_time: int (*ticks*)
	}

	type stats_change_on_level_t = {
		(*level: int;
		exp_to_advance_level: int; (* increases as bosses get harder *)*)
		long_attack: int;
		short_attack: int;
		health: int;
		energy: int;
		energy_add_countdown: int
	}

	type stats_t = {
		constant: stats_constant_t;
		change_on_level: stats_change_on_level_t;
		mutable velocity: Vector2.t; (* every tick it is modified based on angle, accel, air_resistance. then position is moved by velocity *)
		mutable jump_quota: int; (* decrease by 1 during jump. can't jump if is less than or equal to 0. when collide with ground reset to 2 *)
		mutable energy: int; (* attacks decrease this. short attack decrease it in a small way. long attacks more. replenished by a certain amount every tick*)
		mutable health: int;
		(*mutable exp: int; (* every time it's increased it is checked against stats.exp_to_advance_level to decide whether to increase level *)*)
	}

	type type_t =
		| Player
		| Bot of (unit -> action_t list)
		| Other

	type src_rect_t =
		| Spritesheet of bool * Rectangle.t (*use_scene_spritesheet, src_rect*)
		| Color of Color.t

	type animation_event_t =
		| ShortAttackLeft
		| ShortAttackRight
		| ShortAttackUp
		| LongAttackLeft
		| LongAttackRight
		| MovingLeft
		| MovingRight
		| MovingUp
		| StandingFacingLeft
		| StandingFacingRight

	type t = {
		name: string;
		team: int;
		sprite_type: type_t;
		src_rects: src_rect_t Array.t;
		mutable src_rect: int; (* index of src_rects *)
		animations: (animation_event_t, int Array.t) Hashtbl.t;
		mutable current_animation_event: animation_event_t * int; (*current animation event, current animation index*)
		mutable animation_frame_left: int;
		dest_rect: Rectangle.t;
		mutable long_attack: (direction_t * int) option; (* long attack, don't allow inputs while this is happening. during it the player is invincible, and they pass through any player (not other) on either side, dealing damage. this means that if the player collides with any bot while long attacking, that bot takes damage for the duration of the long attack. int counts down until it gets to 0, at which point long attack stops. during the entirety of the long attack, player moves at long_attack_speed in 'direction' direction *)
		mutable short_attack: float option; (*angle. next tick, short_attack is reset to None*)
		mutable energy_add_countdown: int; (*ticks*)
		mutable dialog: dialog_t option;
		mutable visible: bool;
		mutable dead: bool;
		z_index: int;
		stats: stats_t
	}

	let send_action sprite action =
		let (old_animation, _) = sprite.current_animation_event in
		match action with
		| ShortAttack angle ->
				if Option.is_none sprite.short_attack then
					if sprite.stats.energy <
						sprite.stats.constant.short_attack_energy
					then
						sprite.short_attack <- Some angle;
					sprite.stats.energy <- sprite.stats.energy -
						sprite.stats.constant.short_attack_energy;
					sprite.current_animation_event <-
						let f a b = (a *. Float.pi) /. b in
						if angle > (f 2. 3.) && angle < (f 3. 2.) then
							(ShortAttackLeft, 0)
						else if angle > (f 3. 2.) && angle < (f 1. 3.) then
							(ShortAttackRight, 0)
						else
							(ShortAttackUp, 0)
		| LongAttack direction ->
				if Option.is_none sprite.long_attack then
					if sprite.stats.energy <
						sprite.stats.constant.long_attack_energy
					then
						sprite.long_attack <- Some
							(direction, sprite.stats.constant.long_attack_time);
					sprite.stats.energy <- sprite.stats.energy -
						sprite.stats.constant.long_attack_energy;
					sprite.current_animation_event <-
						((match direction with
							| Left -> LongAttackLeft
							| Right -> LongAttackRight), 0)
		| Jump ->
				if sprite.stats.jump_quota > 0 then
					Vector2.set_y sprite.stats.velocity
						sprite.stats.constant.jump_speed;
					sprite.stats.jump_quota <-
						sprite.stats.jump_quota - 1;
					if (sprite.stats.velocity |> Vector2.x |> abs_float) < 20. then
						sprite.current_animation_event <- (MovingUp, 0)
		| Move direction ->
				let speed = sprite.stats.constant.move_speed *.
					(match direction with
						| Left -> -1.
						| Right -> 1.)
				in
				Vector2.set_x sprite.stats.velocity speed;
				sprite.current_animation_event <-
					((match direction with
						| Left -> MovingLeft
						| Right -> MovingRight), 0)
		| StopMoving ->
				sprite.current_animation_event <-
					((if (Vector2.x sprite.stats.velocity) < 0. then
						StandingFacingLeft
					else
						StandingFacingRight), 0);
				Vector2.set_x sprite.stats.velocity 0.;
		| Speak (contents, allow_input_option) ->
				let dialog = {
					sprite_name = sprite.name;
					contents;
					left = String.length contents;
					time_until_next_char =
						sprite.stats.constant.dialog_time_until_next_char;
					left_until_next_char =
						sprite.stats.constant.dialog_time_until_next_char;
					allow_input =
						(match allow_input_option with
							| Some allow_input -> allow_input
							| None -> true)
				} in sprite.dialog <- Some dialog
		| EnterScene start_pos ->
				Rectangle.set_x sprite.dest_rect (Vector2.x start_pos);
				Rectangle.set_y sprite.dest_rect (Vector2.y start_pos);
				sprite.visible <- true;
				if sprite.dead then
					sprite.dead <- false
		| ExitScene ->
				sprite.visible <- false
		| Die ->
				sprite.visible <- false;
				sprite.dead <- true;
				sprite.stats.health <-
					sprite.stats.change_on_level.health;
				sprite.stats.energy <-
					sprite.stats.change_on_level.energy;
				Vector2.set_x sprite.stats.velocity 0.;
				Vector2.set_y sprite.stats.velocity 0.;
		| NoAction -> ();
		let (new_animation, _) = sprite.current_animation_event in
		if old_animation <> new_animation then
			sprite.animation_frame_left <- 0

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
end

module Scene = struct
	type planned_t =
		| Action of Sprite.action_t
		| SwitchScene

	type contents_t =
		| Planned of (int, string * planned_t) Hashtbl.t (* tick, (sprite, action). every tick this hashtbl is checked *)
		| Fight

	type t = {
		spritesheet: int; (* index of scene_spritesheets *)
		contents: contents_t;
		next_scene: int option (* if None, end game *)
	}
end

type game_t = {
	title: string;
	width: int;
	height: int;
	fps: int;
	time_per_frame: float;
	mutable tick: int;
	mutable scene_tick: int;
	mutable switch_scene: bool;
	mutable should_exit: bool;
	mutable sprites: Sprite.t Array.t;
	sprite_names: (string, int) Hashtbl.t;
	spritesheet: Texture2D.t;
	scene_spritesheets: Texture2D.t Array.t;
	scenes: Scene.t Array.t;
	mutable current_scene: int;
	(*mutable animations: animation_t Array.t;*)
	mutable dialogs: dialog_t list;
	font: Font.t;
	font_ratio: float (*width/height*)
}

let bot_ai (_: game_t) () : Sprite.action_t list = []

module Config = struct
	open Yojson.Basic.Util

	type sprite_type_config_t =
		| Player
		| Bot of int (*builtin ai programs*)
		| Other

	type vector2_config_t = { x: int; y: int }

	type rectangle_config_t = { x: int; y: int; w: int; h: int }

	type src_rect_config_t =
		| Spritesheet of {
			use_scene_spritesheet: bool;
			src_rect: rectangle_config_t
		}
		| Color of { r: int; g: int; b: int; a: int }

	type animation_events_config_t = {
		event: Sprite.animation_event_t;
		indices: int list
	}

	type sprite_config_t = {
		name: string;
		team: int;
		sprite_type: sprite_type_config_t;
		src_rects: src_rect_config_t list;
		src_rect: int;
		animations: animation_events_config_t list;
		dest_rect: rectangle_config_t;
		visible: bool;
		z_index: int;
		stats_constants: Sprite.stats_constant_t;
		stats_change_on_level: Sprite.stats_change_on_level_t
	}

	type sprite_action_config_t =
		| ShortAttack of float
		| LongAttack of bool (*true=right, false=left*)
		| Jump
		| StopMoving
		| Move of bool
		| Speak of (string * bool option) (*contents, allow_input*)
		| EnterScene of vector2_config_t
		| Die
		| ExitScene
		| NoAction

	type scene_planned_config_t =
		| Action of sprite_action_config_t
		| SwitchScene

	type scene_contents_planned_config_t = {
		tick: int;
		sprite: string;
		action: scene_planned_config_t
	}

	type scene_contents_config_t =
		| Planned of scene_contents_planned_config_t list
		| Fight

	type scene_config_t = {
		spritesheet: int;
		contents: scene_contents_config_t;
		next_scene: int option
	}

	type t = {
		title: string;
		width: int;
		height: int;
		fps: int;
		sprites: sprite_config_t list;
		spritesheet: string;
		scene_spritesheets: string list;
		scenes: scene_config_t list; (* first scene is index 0 *)
		font: string;
		font_ratio: float
	}

	let direction_of_bool config = if config then Right else Left

	let sprite_type_config_of_json json =
		match json |> member "type" |> to_string with
		| "Player" -> Player
		| "Bot" -> Bot (json |> member "Bot" |> to_int)
		| _ -> Other

	let vector2_config_of_json json : vector2_config_t = {
		x = json |> member "x" |> to_int;
		y = json |> member "y" |> to_int
	}

	let rectangle_config_of_json json : rectangle_config_t = {
		x = json |> member "x" |> to_int;
		y = json |> member "y" |> to_int;
		w = json |> member "w" |> to_int;
		h = json |> member "h" |> to_int
	}

	let stats_constant_t_of_json json : Sprite.stats_constant_t = {
		gravity_accel = json |> member "gravity_accel" |> to_float;
		move_speed = json |> member "move_speed" |> to_float;
		jump_speed = json |> member "jump_speed" |> to_float;
		long_attack_speed = json |> member "long_attack_speed" |> to_float;
		long_attack_time = json |> member "long_attack_time" |> to_int;
		short_attack_range = json |> member "short_attack_range" |> to_float;
		dialog_time_until_next_char = json |> member "dialog_time_until_next_char" |> to_int;
		long_attack_energy = json |> member "long_attack_energy" |> to_int;
		short_attack_energy = json |> member "short_attack_energy" |> to_int;
		animation_frame_time = json |> member "animation_frame_time" |> to_int
	}

	let stats_change_on_level_t_of_json json : Sprite.stats_change_on_level_t = {
		long_attack = json |> member "long_attack" |> to_int;
		short_attack = json |> member "short_attack" |> to_int;
		health = json |> member "health" |> to_int;
		energy = json |> member "energy" |> to_int;
		energy_add_countdown = json |> member "energy_add_countdown" |> to_int
	}

	let src_rect_config_of_json json : src_rect_config_t =
		match json |> member "type" |> to_string with
		| "Spritesheet" ->
			let spritesheet = json |> member "Spritesheet" in
			Spritesheet {
				use_scene_spritesheet = spritesheet |> member "use_scene_spritesheet" |> to_bool;
				src_rect = spritesheet |> member "src_rect" |> rectangle_config_of_json
			}
		| _ ->
			let color = json |> member "Color" in
			let f s = color |> member s |> to_int in
			Color { r = f "r"; g = f "g"; b = f "b"; a = f "a" }

	let animation_event_config_of_json json
		: Sprite.animation_event_t
	= match json |> to_string with
		| "ShortAttackLeft" -> ShortAttackLeft
		| "ShortAttackRight" -> ShortAttackRight
		| "ShortAttackUp" -> ShortAttackUp
		| "LongAttackLeft" -> LongAttackLeft
		| "LongAttackRight" -> LongAttackRight
		| "MovingLeft" -> MovingLeft
		| "MovingRight" -> MovingRight
		| "MovingUp" -> MovingUp
		| "StandingFacingRight" -> StandingFacingRight
		| _ -> StandingFacingLeft

	let animation_events_config_of_json json
		: animation_events_config_t
	= {
		event = json |> member "event"
			|> animation_event_config_of_json;
		indices = json |> member "indices"
			|> to_list |> filter_int
	}

	let sprite_config_of_json json : sprite_config_t = {
		name = json |> member "name" |> to_string;
		team = json |> member "team" |> to_int;
		sprite_type = json |> member "sprite_type" |> sprite_type_config_of_json;
		src_rects = json |> member "src_rects" |> to_list
			|> List.map src_rect_config_of_json;
		src_rect = json |> member "src_rect" |> to_int;
		animations = json |> member "animations" |> to_list
			|> List.map animation_events_config_of_json;
		dest_rect = json |> member "dest_rect" |> rectangle_config_of_json;
		visible = json |> member "visible" |> to_bool;
		z_index = json |> member "z_index" |> to_int;
		stats_constants = json |> member "stats_constants"
			|> stats_constant_t_of_json;
		stats_change_on_level = json |> member "stats_change_on_level"
			|> stats_change_on_level_t_of_json
	}

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
					speak |> member "allow_input" |> to_bool_option)
		| "EnterScene" -> EnterScene (json |> member "EnterScene" |> vector2_config_of_json)
		| "Die" -> Die
		| "ExitScene" -> ExitScene
		| _ -> NoAction

	let scene_planned_config_of_json json : scene_planned_config_t =
		match json |> member "type" |> to_string with
		| "Action" -> Action
			(json |> member "Action" |> sprite_action_config_of_json)
		| _ -> SwitchScene

	let scene_contents_planned_config_of_json json
		: scene_contents_planned_config_t
	= {
		tick = json |> member "tick" |> to_int;
		sprite = json |> member "sprite" |> to_string;
		action = json |> member "action" |> scene_planned_config_of_json
	}

	let scene_contents_config_of_json json : scene_contents_config_t =
		match json |> member "type" |> to_string with
		| "Planned" -> Planned
			(json |> member "Planned" |> to_list
				|> List.map scene_contents_planned_config_of_json)
		| _ -> Fight

	let scene_config_of_json json : scene_config_t = {
		spritesheet = json |> member "spritesheet" |> to_int;
		contents = json |> member "contents"
			|> scene_contents_config_of_json;
		next_scene = json |> member "next_scene" |> to_int_option
	}

	let of_json json = {
		title = json |> member "title" |> to_string;
		width = json |> member "width" |> to_int;
		height = json |> member "height" |> to_int;
		fps = json |> member "fps" |> to_int;
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

	let sprite_type_of_config config game : Sprite.type_t =
		match config with
		| Player -> Player
		| Bot _ -> Bot (bot_ai game)
		| Other -> Other

	let vector2_of_config (config: vector2_config_t) = Vector2.create
		(float_of_int config.x) (float_of_int config.y)

	let rectangle_of_config config = Rectangle.create
		(float_of_int config.x) (float_of_int config.y)
		(float_of_int config.w) (float_of_int config.h)

	let src_rect_of_config config : Sprite.src_rect_t =
		match config with
		| Spritesheet { use_scene_spritesheet; src_rect } ->
			Spritesheet
				(use_scene_spritesheet,
				src_rect |> rectangle_of_config)
		| Color color -> Color (Color.create color.r color.g color.b color.a)

	let animation_events_of_config (config: animation_events_config_t list)
		: (Sprite.animation_event_t, int Array.t) Hashtbl.t
	=
		let hashtbl = Hashtbl.create (List.length config) in
		List.iter (fun { event; indices } ->
			Hashtbl.add hashtbl event (Array.of_list indices)
		) config;
		hashtbl

	let sprite_of_config game config : Sprite.t = {
		name = config.name;
		team = config.team;
		sprite_type = sprite_type_of_config config.sprite_type game;
		src_rects = Array.of_list config.src_rects
			|> Array.map src_rect_of_config;
		src_rect = config.src_rect;
		animations = config.animations |> animation_events_of_config;
		current_animation_event = (StandingFacingRight, 0);
		animation_frame_left = config.stats_constants.animation_frame_time;
		dest_rect = rectangle_of_config config.dest_rect;
		long_attack = None;
		short_attack = None;
		energy_add_countdown = config.stats_change_on_level.energy_add_countdown;
		dialog = None;
		visible = config.visible;
		dead = false;
		z_index = config.z_index;
		stats = {
			constant = config.stats_constants;
			change_on_level = config.stats_change_on_level;
			velocity = Vector2.create 0. 0.;
			jump_quota = 2;
			energy = config.stats_change_on_level.energy;
			health = config.stats_change_on_level.health;
		}
	}

	let sprite_action_of_config config : Sprite.action_t = 
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

	let scene_contents_planned_of_config config : Scene.planned_t =
		match config with
		| Action action -> Action (sprite_action_of_config action)
		| SwitchScene -> SwitchScene

	let scene_contents_of_config config : Scene.contents_t =
		match config with
		| Planned li ->
				let hashtbl = Hashtbl.create (List.length li) in
				List.iter (fun { tick; sprite; action } ->
					Hashtbl.add hashtbl tick
						(sprite, scene_contents_planned_of_config action)
				) li;
				Planned hashtbl
		| Fight -> Fight

	let scene_of_config (config: scene_config_t) : Scene.t = {
		spritesheet = config.spritesheet;
		contents = scene_contents_of_config config.contents;
		next_scene = config.next_scene
	}

	let to_game config : game_t =
		let sprite_names = Hashtbl.create (List.length config.sprites) in
		List.iteri (fun i sprite ->
			Hashtbl.add sprite_names sprite.name i
		) config.sprites;
		let game = {
			title = config.title;
			width = config.width;
			height = config.height;
			fps = config.fps;
			time_per_frame = 1. /. (float_of_int config.fps);
			tick = 0;
			scene_tick = 0;
			switch_scene = false;
			should_exit = false;
			sprites = Array.of_list [];
			sprite_names;
			spritesheet = load_texture config.spritesheet;
			scene_spritesheets = config.scene_spritesheets
				|> Array.of_list
				|> Array.map load_texture;
			scenes = config.scenes
				|> Array.of_list
				|> Array.map scene_of_config;
			current_scene = 0;
			dialogs = [];
			font = load_font config.font;
			font_ratio = config.font_ratio
		} in
		game.sprites <- config.sprites
			|> Array.of_list 
			|> Array.map (sprite_of_config game);
		game
end

module Render = struct
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
	let dialog y font font_ratio (dialog: dialog_t) : bool =
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
					draw_texture_pro spritesheet
						src_rect sprite.dest_rect
						(Vector2.create 0. 0.) 0.
						(Color.create 0 0 0 255)
			| Color color ->
					draw_rectangle_rec sprite.dest_rect color

	let game game =
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
end

let get_user_input (sprite: Sprite.t) (game: game_t) : Sprite.action_t list =
	let switch_scene = Array.fold_right
		(fun (other_sprite: Sprite.t) switch_scene -> switch_scene && (
			(sprite.sprite_type <> Sprite.Other) &&
			other_sprite.dead && (sprite.team <> other_sprite.team)
		)) game.sprites true
	in
	if switch_scene then
		game.switch_scene <- true;
	let actions = ref [] in
	let map_key_input input action =
		if is_key_down input then
			actions := action :: !actions;
	in
	let open Key in
	let open Sprite in
	map_key_input Space Jump;
	map_key_input Left (Move Left);
	map_key_input Right (Move Right);
	map_key_input A (ShortAttack 0.);
	map_key_input D (ShortAttack Float.pi);
	map_key_input Q (LongAttack Left);
	map_key_input E (LongAttack Right);
	!actions

module Tick = struct
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
	let sprite (game: game_t) (sprite: Sprite.t) =
		sprite.animation_frame_left <- sprite.animation_frame_left - 1;
		if sprite.animation_frame_left <= 0 then
			let (animation, index) = sprite.current_animation_event in
			let index = (index + 1) mod
				(animation |> Hashtbl.find sprite.animations |> Array.length) in
			sprite.src_rect <- Array.get (Hashtbl.find sprite.animations animation) index;
			sprite.current_animation_event <- (animation, index);
			sprite.animation_frame_left <- sprite.stats.constant.animation_frame_time;
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
					) collided_enemies;
		| None ->
				let actions = match sprite.sprite_type with
					| Player ->
							let allow_input = List.fold_right (fun dialog allow_input ->
								allow_input || dialog.allow_input) game.dialogs false
							in
							if allow_input then
								get_user_input sprite game
							else
								[]
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
						sprite.short_attack <- None;
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
				set_y sprite.dest_rect ((x sprite.dest_rect) +. (Vector2.y movement));
				sprite.energy_add_countdown <- sprite.energy_add_countdown - 1;
				if sprite.energy_add_countdown <= 0 then
					sprite.stats.energy <- sprite.stats.energy - 1;
					sprite.energy_add_countdown <-
						sprite.stats.change_on_level.energy_add_countdown

	let planned_scene game contents =
		match Hashtbl.find_opt contents game.scene_tick with
		| Some (sprite_name, (planned: Scene.planned_t)) ->
				(match planned with
					| SwitchScene -> game.switch_scene <- true
					| Action action ->
							let f f a b = f b a in
							sprite_name
								|> Hashtbl.find game.sprite_names
								|> Array.get game.sprites
								|> f Sprite.send_action action)
		| None -> ()

	let game game =
		let current_scene = Array.get game.scenes game.current_scene in
		match current_scene.contents with
		| Planned contents -> planned_scene game contents
		| Fight -> Array.iter
			(fun (s: Sprite.t) ->
				if not s.dead then sprite game s
			) game.sprites;
		game.tick <- game.tick + 1;
		game.scene_tick <- game.scene_tick + 1;
		if game.switch_scene then
			match current_scene.next_scene with
			| Some next_scene ->
					game.scene_tick <- 0;
					game.current_scene <- next_scene
			| None -> game.should_exit <- true;
end

let save_game_data (_: game_t) = ()

let setup () =
	let game = Array.get Sys.argv 1
		|> Yojson.Basic.from_file
		|> Config.of_json
		|> Config.to_game
	in
	prerr_endline "DEBUG1";
	init_window game.width game.height game.title;
	set_target_fps game.fps;
	game

let rec loop game =
	match window_should_close () || game.should_exit with
	| true ->
			save_game_data game;
			close_window ()
	| false ->
			let start_time = get_time () in
			begin_drawing ();
			Tick.game game;
			Render.game game;
			end_drawing ();
			swap_screen_buffer ();
			let time_taken = (get_time ()) -. start_time in
			let time_left = game.time_per_frame -. time_taken in
			prerr_string "time_left: ";
			prerr_float time_left;
			prerr_newline ();
			if time_left > 0. then
				wait_time time_left;
			loop game

let () = setup () |> loop
