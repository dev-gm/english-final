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

type dialog_t = {
	sprite_name: string;
	contents: string;
	mutable left: int;
	time_until_next_char: int; (* every tick, render letters of dialog based on speed. left is number of letters left to read. at end, check if left = 0. if so, reset dialog. *)
	mutable left_until_next_char: int; (*render when left = 0, reset to time*)
	allow_input: bool
}

type action_t =
	| ShortAttack of int (*angle, in degrees*)
	| LongAttack of direction_t
	| Jump
	| StopMoving (*only send this if stats.moving = Some*)
	| Move of direction_t
	| Speak of (string * bool option) (*contents, allow_input*)
	| EnterScene of Vector.t (* Enter is probably a Key *)
	| Die
	| ExitScene
	| NoAction

type stats_constant_t = {
	gravity_accel: int;
	move_speed: int;
	jump_speed: int;
	long_attack_speed: int;
	long_attack_time: int;
	short_attack_range: int;
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
	mutable velocity: Vector.t; (* every tick it is modified based on angle, accel, air_resistance. then position is moved by velocity *)
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
	| Spritesheet of Sdlrect.t
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
	mutable dest_rect: Sdlrect.t;
	mutable long_attack: (direction_t * int) option; (* long attack, don't allow inputs while this is happening. during it the player is invincible, and they pass through any player (not other) on either side, dealing damage. this means that if the player collides with any bot while long attacking, that bot takes damage for the duration of the long attack. int counts down until it gets to 0, at which point long attack stops. during the entirety of the long attack, player moves at long_attack_speed in 'direction' direction *)
	mutable short_attack: int option; (*angle. next tick, short_attack is reset to None*)
	mutable energy_add_countdown: int; (*ticks*)
	mutable dialog: dialog_t option;
	mutable visible: bool;
	mutable dead: bool;
	sprites_can_collide: bool; (*if sprites can collide with it. Other (floors, etc) will usually say yes. if Player or Bot don't want collision, then they can say no*)
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
					if angle > 120 && angle < 270 then
						(ShortAttackLeft, 0)
					else if angle > 270 && angle < 60 then
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
				sprite.stats.velocity.y <- sprite.stats.constant.jump_speed;
				sprite.stats.jump_quota <-
					sprite.stats.jump_quota - 1;
				if (sprite.stats.velocity.x |> abs) < 20 then
					sprite.current_animation_event <- (MovingUp, 0)
	| Move direction ->
			let speed = sprite.stats.constant.move_speed *
				(match direction with
					| Left -> -1
					| Right -> 1)
			in
			sprite.stats.velocity.x <- speed;
			sprite.current_animation_event <-
				((match direction with
					| Left -> MovingLeft
					| Right -> MovingRight), 0)
	| StopMoving ->
			sprite.current_animation_event <-
				((if (sprite.stats.velocity.x) < 0 then
					StandingFacingLeft
				else
					StandingFacingRight), 0);
			sprite.stats.velocity.x <- 0;
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
			sprite.dest_rect <-
				{ sprite.dest_rect with x = start_pos.x; y = start_pos.y };
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
			sprite.stats.velocity.x <- 0;
			sprite.stats.velocity.y <- 0
	| NoAction -> ();
	let (new_animation, _) = sprite.current_animation_event in
	if old_animation <> new_animation then
		sprite.animation_frame_left <- 0

(*angle IS IN RADIANS, unit circle*)
let find_closest_enemy_in_direction_and_range
	(sprite: t)
	(angle: int)
	(range: int)
	(sprites: t Array.t)
	: t option
=
	let center: Vector.t = {
		x = (sprite.dest_rect.x + sprite.dest_rect.w) / 2;
		y = (sprite.dest_rect.y + sprite.dest_rect.h) / 2
	} in
	let vector_end: Vector.t = {
		x = center.x + ((float_of_int range) *.
			(cos (radian_of_degree angle)) |> int_of_float);
		y = center.y + ((float_of_int range) *.
			(sin (radian_of_degree angle)) |> int_of_float)
	} in
	let i = ref 0 in
	Array.get (Array.fold_left
		(fun filtered sprite ->
			let intersection =
				Sdlrect.intersect_rect_and_line
					~rect:sprite.dest_rect
					~p1:(Vector.to_tuple center)
					~p2:(Vector.to_tuple vector_end)
			in
			if Option.is_some intersection then
				Array.unsafe_set filtered !i (Some sprite);
				i := !i + 1;
			filtered
		) (Array.make ((Array.length sprites) + 1) None) sprites) 0

let correct_movement_for_collision
	(a: Sdlrect.t) (b: Sdlrect.t) (mvmt: Vector.t)
	: Vector.t * bool
=
	let bounds: Sdlrect.t =
		let f s v m = [s; s + v; s + m; s + m + v] in
		let (x_values, y_values) = (f a.x a.w mvmt.x, f a.y a.h mvmt.y) in
		{
			x = min_int_list x_values;
			y = min_int_list y_values;
			w = max_int_list x_values;
			h = max_int_list y_values
		}
	in
	let in_path = 
		if Sdlrect.has_intersection ~a:bounds ~b:b then
			let four_corners (r: Sdlrect.t) =
				List.map (fun a ->
					List.map (fun b ->
						(float_of_int a, float_of_int b)
					) [r.y; r.y + r.h]
				) [r.x; r.x + r.w]
					|> List.flatten
			in
			List.exists2 (fun (x1, y1) (x2, y2) ->
				List.exists (fun x3 ->
					let x3 = float_of_int x3 in
					let y_intersect =
						(((x3 -. x1) *. (y2 -. y1)) /. (x2 -. x1)) +. y1
							|> int_of_float
					in y_intersect > b.y && y_intersect < (b.y + b.h)
				) [b.x; b.x + b.w] ||
				List.exists (fun y3 ->
					let y3 = float_of_int y3 in
					let x_intersect =
						(((x2 -. x1) *. (y3 -. y1)) /. (y2 -. y1)) +. x1
							|> int_of_float
					in x_intersect > b.x && x_intersect < (b.x + b.w)
				) [b.y; b.y + b.h]
			) (four_corners a) (four_corners (Vector.rect_add a mvmt))
		else false
	in
	if in_path then
		((if (abs mvmt.x) > (abs mvmt.y) then
			let x =
				if mvmt.x > 0 then
					b.x - (a.x + a.w)
				else if mvmt.x < 0 then
					(b.x + b.w) - a.x
				else
					a.x
			in
			let y =
				let slope =
					(float_of_int mvmt.y) /. (float_of_int mvmt.x) in
				if slope > 0. then
					(slope *. (x - a.x |> float_of_int) |> int_of_float) + a.h
				else if slope < 0. then
					slope *. (x - a.x |> float_of_int) |> int_of_float
				else
					a.y
			in { x; y }
		else
			let y =
				if mvmt.y > 0 then
					b.y - (a.y + a.h)
				else if mvmt.y < 0 then
					(b.y + b.h) - a.y
				else
					a.y
			in
			let x =
				if mvmt.x = 0 then
					a.x
				else
					let slope =
						(float_of_int mvmt.y) /. (float_of_int mvmt.x) in
					if slope > 0. then
						((y - a.h |> float_of_int) /. slope |> int_of_float) + a.x
					else if slope < 0. then
						((float_of_int y) /. slope |> int_of_float) + a.x
					else
						a.x;
			in { x; y }), true)
	else
		(mvmt, false)


(* returns: new_movement, collided_with_ground(or any Other sprite in a downwards direction) *)
let correct_movement_for_collisions sprite movement sprites : Vector.t * bool =
	Array.fold_left (fun (movement, collide_with_ground) other_sprite ->
		if other_sprite.sprites_can_collide then
			let (new_movement, _) = correct_movement_for_collision
				sprite.dest_rect other_sprite.dest_rect movement in
			let collide_with_ground = collide_with_ground ||
				((new_movement.y < movement.y) &&
				(other_sprite.sprite_type = Other)) in
			(new_movement, collide_with_ground)
		else
			(movement, collide_with_ground)
	) (movement, false) sprites

let correct_movement_for_long_attack_collisions
	(sprite: t)
	(movement: Vector.t)
	(sprites: t Array.t)
	: Vector.t * t list
=
	Array.fold_left (fun (movement, enemies) other_sprite ->
		let (new_movement, corrected) = correct_movement_for_collision
			sprite.dest_rect other_sprite.dest_rect movement in
		if corrected then
			match other_sprite.sprite_type with
			| Bot _ | Player when sprite.team <> other_sprite.team ->
					(movement, other_sprite :: enemies)
			| _ -> (new_movement, enemies)
		else
			(movement, enemies)
	) (movement, []) sprites

type planned_t =
	| Action of Sprite.action_t
	| SwitchScene

type contents_t =
	| Planned of (int, string * planned_t) Hashtbl.t (* tick, (sprite, action). every tick this hashtbl is checked *)
	| Fight

type t = {
	contents: contents_t;
	next_scene: int option (* if None, end game *)
}

type t = {
	title: string;
	width: int;
	height: int;
	fps: int;
	ms_per_frame: int;
	window: Sdlwindow.t;
	renderer: Sdlrender.t;
	mutable tick: int;
	mutable scene_tick: int;
	mutable switch_scene: bool;
	mutable should_exit: bool;
	mutable sprites: Sprite.t Array.t;
	sprite_names: (string, int) Hashtbl.t;
	spritesheet: Sdltexture.t;
	scenes: Scene.t Array.t;
	mutable current_scene: int;
	(*mutable animations: animation_t Array.t;*)
	mutable dialogs: Sprite.dialog_t list;
	fonts: (int, Sdlttf.font) Hashtbl.t; (*font_size, font*)
	font_ratio: float
}

let bot_ai (_: t) () : Sprite.action_t list = []

let handle_user_inputs (sprite: Sprite.t) (game: t) : Sprite.action_t list =
	let switch_scene = Array.fold_right
		(fun (other_sprite: Sprite.t) switch_scene -> switch_scene && (
			(sprite.sprite_type <> Sprite.Other) &&
			other_sprite.dead && (sprite.team <> other_sprite.team)
		)) game.sprites true
	in
	if switch_scene then
		game.switch_scene <- true;
	let open Sdl in
	let handle_user_input = function
		| Event.KeyDown { keycode = Keycode.Left; _ } ->
				Sprite.Move Left
		| Event.KeyDown { keycode = Keycode.Right; _ } ->
				Sprite.Move Right
		| Event.KeyDown { keycode = Keycode.Up; _ } ->
				Sprite.Jump
		| Event.Quit _ ->
				game.should_exit <- true;
				Sprite.NoAction
		| _ -> Sprite.NoAction
	in
	let rec event_loop () =
		match Event.poll_event () with
		| None -> []
		| Some event -> (handle_user_input event) :: event_loop ()
	in
	event_loop ()

let save_data (_: t) = ()


type sprite_type_config_t =
	| Player
	| Bot of int (*builtin ai programs*)
	| Other

type src_rect_config_t =
	| Spritesheet of Sdlrect.t
	| Color of Color.t

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
	dest_rect: Sdlrect.t;
	visible: bool;
	sprites_can_collide: bool;
	z_index: int;
	stats_constants: Sprite.stats_constant_t;
	stats_change_on_level: Sprite.stats_change_on_level_t
}

type sprite_action_config_t =
	| ShortAttack of int
	| LongAttack of bool (*true=right, false=left*)
	| Jump
	| StopMoving
	| Move of bool
	| Speak of (string * bool option) (*contents, allow_input*)
	| EnterScene of Vector.t
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
	scenes: scene_config_t list; (* first scene is index 0 *)
	font: string;
	font_sizes: int list;
	font_ratio: float
}

let direction_of_bool config = if config then Right else Left

let sprite_type_config_of_json json =
	match json |> member "type" |> to_string with
	| "Player" -> Player
	| "Bot" -> Bot (json |> member "Bot" |> to_int)
	| _ -> Other

let vector_of_json json : Vector.t = {
	x = json |> member "x" |> to_int;
	y = json |> member "y" |> to_int
}

let rectangle_of_json json : Sdlrect.t = Sdlrect.make4
	~x:(json |> member "x" |> to_int)
	~y:(json |> member "y" |> to_int)
	~w:(json |> member "w" |> to_int)
	~h:(json |> member "h" |> to_int)

let stats_constant_t_of_json json : Sprite.stats_constant_t = {
	gravity_accel = json |> member "gravity_accel" |> to_int;
	move_speed = json |> member "move_speed" |> to_int;
	jump_speed = json |> member "jump_speed" |> to_int;
	long_attack_speed = json |> member "long_attack_speed" |> to_int;
	long_attack_time = json |> member "long_attack_time" |> to_int;
	short_attack_range = json |> member "short_attack_range" |> to_int;
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
	| "Spritesheet" -> Spritesheet (json |> member "Spritesheet" |> rectangle_of_json)
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

let animation_events_config_of_json json : animation_events_config_t = {
	event = json |> member "event" |> animation_event_config_of_json;
	indices = json |> member "indices" |> to_list |> List.map to_int
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
	dest_rect = json |> member "dest_rect" |> rectangle_of_json;
	visible = json |> member "visible" |> to_bool;
	sprites_can_collide = json |> member "sprites_can_collide" |> to_bool;
	z_index = json |> member "z_index" |> to_int;
	stats_constants = json |> member "stats_constants"
		|> stats_constant_t_of_json;
	stats_change_on_level = json |> member "stats_change_on_level"
		|> stats_change_on_level_t_of_json
}

let sprite_action_config_of_json json : sprite_action_config_t =
	match json |> member "type" |> to_string with
	| "ShortAttack" -> ShortAttack (json |> member "ShortAttack" |> to_int)
	| "LongAttack" -> LongAttack (json |> member "LongAttack" |> to_bool)
	| "Jump" -> Jump
	| "StopMoving" -> StopMoving
	| "Move" -> Move (json |> member "Move" |> to_bool)
	| "Speak" ->
			let speak = json |> member "Speak" in
			Speak
				(speak |> member "contents" |> to_string,
				speak |> member "allow_input" |> to_bool_option)
	| "EnterScene" -> EnterScene (json |> member "EnterScene" |> vector_of_json)
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
	contents = json |> member "contents"
		|> scene_contents_config_of_json;
	next_scene = json |> member "next_scene" |> to_int_option
}

let of_json json = {
	title = json |> member "title" |> to_string;
	width = json |> member "width" |> to_int;
	height = json |> member "height" |> to_int;
	fps = json |> member "fps" |> to_int;
	spritesheet = json |> member "spritesheet" |> to_string;
	sprites = json |> member "sprites" |> to_list
		|> List.map sprite_config_of_json;
	scenes = json |> member "scenes" |> to_list
		|> List.map scene_config_of_json;
	font = json |> member "font" |> to_string;
	font_sizes = json |> member "font_sizes" |> to_list
		|> List.map to_int;
	font_ratio = json |> member "font_ratio" |> to_float
}

let sprite_type_of_config config game : Sprite.type_t =
	match config with
	| Player -> Player
	| Bot _ -> Bot (Game.bot_ai game)
	| Other -> Other

let src_rect_of_config config : Sprite.src_rect_t =
	match config with
	| Spritesheet src_rect -> Spritesheet src_rect
	| Color color -> Color color

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
	dest_rect = config.dest_rect;
	long_attack = None;
	short_attack = None;
	energy_add_countdown = config.stats_change_on_level.energy_add_countdown;
	dialog = None;
	visible = config.visible;
	dead = false;
	sprites_can_collide = config.sprites_can_collide;
	z_index = config.z_index;
	stats = {
		constant = config.stats_constants;
		change_on_level = config.stats_change_on_level;
		velocity = { x = 0; y = 0 };
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
	| EnterScene vec -> EnterScene vec
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
	contents = scene_contents_of_config config.contents;
	next_scene = config.next_scene
}

let to_game config : Game.t =
	Sdl.init [`VIDEO; `TIMER];
	let sprite_names = Hashtbl.create (List.length config.sprites) in
	List.iteri (fun i sprite ->
		Hashtbl.add sprite_names sprite.name i
	) config.sprites;
	let window, renderer = Sdlrender.create_window_and_renderer
		~width:config.width ~height:config.height ~flags:[] in
	Sdlwindow.set_title ~window ~title:config.title;
	Sdlimage.init [`PNG];
	Sdlttf.init ();
	let spritesheet =
		let file = Sdlrwops.from_file
			~filename:config.spritesheet ~mode:"rb" in
		let surface = Sdlimage.load_png_rw file in
		let texture = Sdltexture.create_from_surface renderer surface in
		Sdlrwops.free file;
		Sdlsurface.free surface;
		texture
	in
	let game: Game.t = {
		title = config.title;
		width = config.width;
		height = config.height;
		fps = config.fps;
		ms_per_frame = 1000. /. (float_of_int config.fps) |> int_of_float;
		window;
		renderer;
		spritesheet;
		tick = 0;
		scene_tick = 0;
		switch_scene = false;
		should_exit = false;
		sprites = Array.of_list [];
		sprite_names;
		scenes = config.scenes
			|> Array.of_list
			|> Array.map scene_of_config;
		current_scene = 0;
		fonts = config.font_sizes |>
			List.fold_left (fun hashtbl font_size ->
				Hashtbl.add hashtbl font_size
					(Sdlttf.open_font ~file:config.font ~ptsize:font_size);
				hashtbl
			) (Hashtbl.create (List.length config.font_sizes));
		font_ratio = config.font_ratio;
		dialogs = [];
	} in
	game.sprites <- config.sprites
		|> Array.of_list 
		|> Array.map (sprite_of_config game);
	game

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
let sprite (game: Game.t) (sprite: Sprite.t) =
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
					Sprite.correct_movement_for_long_attack_collisions sprite {
						x = sprite.stats.constant.long_attack_speed *
							(match direction with | Right -> 1 | Left -> -1);
						y = 0
					} game.sprites
				in
				sprite.dest_rect <- Vector.rect_add sprite.dest_rect movement;
				List.iter (fun (enemy: Sprite.t) ->
					enemy.stats.health <- enemy.stats.health -
						sprite.stats.change_on_level.long_attack;
					if enemy.stats.health <= 0 then
						Sprite.send_action enemy Die;
				) collided_enemies;
	| None ->
			let actions = match sprite.sprite_type with
				| Player ->
						let allow_input = List.fold_right
							(fun (dialog: Sprite.dialog_t) allow_input ->
								allow_input || dialog.allow_input
							) game.dialogs false
						in
						if allow_input then
							Game.handle_user_inputs sprite game
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
			sprite.stats.velocity <- Vector.add sprite.stats.velocity
				(Vector.create 0 sprite.stats.constant.gravity_accel);
			let (movement, collided_with_ground) =
				Sprite.correct_movement_for_collisions
					sprite
					sprite.stats.velocity
					game.sprites
			in
			if collided_with_ground then
				sprite.stats.jump_quota <- 2;
			sprite.stats.velocity <- movement;
			let x = sprite.dest_rect.x + movement.x |> ref in
			let y = sprite.dest_rect.y + movement.y |> ref in
			if !x < 0 then x := 0
			else if !x > game.width then x := game.width
			else if !y < 0 then y := 0
			else if !y > game.height then y:= game.height;
			sprite.dest_rect <- { sprite.dest_rect with x = !x; y = !y };
			sprite.energy_add_countdown <- sprite.energy_add_countdown - 1;
			if sprite.energy_add_countdown <= 0 then
				sprite.stats.energy <- sprite.stats.energy - 1;
				sprite.energy_add_countdown <-
					sprite.stats.change_on_level.energy_add_countdown

let planned_scene (game: Game.t) contents =
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

let game (game: Game.t) =
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

let setup () =
	Array.get Sys.argv 1
		|> Yojson.Basic.from_file
		|> Config.of_json
		|> Config.to_game

let rec loop (game: Game.t) =
	match game.should_exit with
	| true ->
			Sdlimage.quit ();
			Sdlttf.quit ();
			Sdlwindow.destroy game.window;
			Sdl.quit ();
	| false ->
			let start = Sdltimer.get_ticks () in
			Tick.game game;
			Render.game game;
			let time_left = game.ms_per_frame -
				((Sdltimer.get_ticks ()) - start) in
			if time_left > 0 then
				Sdltimer.delay ~ms:time_left;
			loop game

let () = setup () |> loop
