open Utils
open Raylib
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
	| Bot _ -> Bot (Game.bot_ai game)
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

let to_game config : Game.t =
	let sprite_names = Hashtbl.create (List.length config.sprites) in
	List.iteri (fun i sprite ->
		Hashtbl.add sprite_names sprite.name i
	) config.sprites;
	let game: Game.t = {
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

