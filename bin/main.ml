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
		| Speak of (string * int option) (*contents, allow_input, speed*)
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
		level: int;
		exp_to_advance_level: int; (* increases as bosses get harder *)
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
		mutable exp: int; (* every time it's increased it is checked against stats.exp_to_advance_level to decide whether to increase level *)
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
		src_rects: Rectangle.t list; (* on spritesheet *)
		mutable src_rect: int; (* index of spritsheet_locations *)
		mutable dest_rect: Rectangle.t;
		mutable long_attack: (direction_t * int) option; (* long attack, don't allow inputs while this is happening. during it the player is invincible, and they pass through any player (not other) on either side, dealing damage. this means that if the player collides with any bot while long attacking, that bot takes damage for the duration of the long attack. int counts down until it gets to 0, at which point long attack stops. during the entirety of the long attack, player moves at long_attack_speed in 'direction' direction *)
		mutable short_attack: float option; (*angle. next tick, short_attack is reset to None*)
		mutable dialog: dialog_t option;
		mutable visible: bool;
		mutable dead: bool;
		mutable player_collide: bool;
		mutable bot_collide: bool;
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
		(sprites: t list)
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
		List.nth_opt (List.filter (fun other_sprite ->
			let line_between_points vf rf =
				(((vf center) < (rf other_sprite.dest_rect)) &&
				((vf vector_end) > (rf other_sprite.dest_rect))) ||
				(((vf center) > (rf other_sprite.dest_rect)) &&
				((vf vector_end) < (rf other_sprite.dest_rect)))
			in
			(line_between_points Vector2.x Rectangle.x) &&
			(line_between_points Vector2.y Rectangle.y)
		) sprites) 0

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
		List.fold_right (fun other_sprite (movement, collide_with_ground) ->
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
		(sprites: t list)
		: Vector2.t * t list
	=
		List.fold_right (fun other_sprite (movement, enemies) ->
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
				(List.nth
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
	sprites: Sprite.t list;
	spritesheet: int; (* index of scene_spritesheets *)
	contents: scene_contents_t;
	next_scene: int option (* if None, end game *)
}

(*type animation_t =
	| Attack of (string * int)*)

type game_t = {
	mutable tick: int;
	mutable scene_tick: int;
	mutable switch_scene: bool;
	mutable should_exit: bool;
	sprites: Sprite.t list;
	sprite_names: (string, int) Hashtbl.t;
	spritesheet: Texture2D.t;
	scene_spritesheets: Texture2D.t list;
	scenes: scene_t list;
	mutable current_scene: int;
	(*mutable animations: animation_t list;*)
	mutable dialogs: dialog_t list;
	font: Font.t;
	font_ratio: float (*width/height*)
}

module Config = struct
	open Yojson
end

module Render = struct
	(*TODO: make dialog text wrap and make box taller if it text goes over bounds*)
	let text_box bg_color text_color text x y width font_size =
		let ix = (int_of_float x) in
		let iy = (int_of_float y) in
		let iw = (int_of_float width) in
		draw_rectangle ix iy iw font_size bg_color;
		draw_text text ix iy font_size text_color;
		Rectangle.create x y width (float_of_int font_size)

	(* returns if not done *)
	let dialog (dialog: dialog_t) : bool =
		dialog.left <- dialog.left - 1;
		if dialog.left > 0 then
			ignore
				(text_box Color.white Color.black
					(String.sub dialog.contents
						0 ((String.length dialog.contents) - dialog.left))
					25. 25. 600. 50);
		dialog.left > 0

	let game game =
		List.iter (fun (_, (sprite: Sprite.t)) ->
			if sprite.visible then
				let spritesheet =
					if sprite.use_scene_spritesheet then
						(List.nth game.scene_spritesheets
							(List.nth game.scenes game.current_scene).spritesheet)
					else
						game.spritesheet
				in
				Sprite.render sprite spritesheet
			else
				())
		(Hashtbl.fold (fun name sprite_index full_render_order ->
			let sprite = List.nth game.sprites sprite_index in
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
		game.dialogs <- List.filter dialog game.dialogs
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
			let sprite = List.nth game.sprites sprite_index in
			Sprite.send_action sprite action
	| _ -> ()

let tick_game game =
	let current_scene = List.nth game.scenes game.current_scene in
	match current_scene.contents with
	| Planned actions -> tick_planned_scene game actions
	| Fight -> Hashtbl.iter (fun _ sprite_index ->
			tick_sprite game (List.nth game.sprites sprite_index)) game.sprite_names;
	game.tick <- game.tick + 1;
	if game.switch_scene then
		match current_scene.next_scene with
		| Some next_scene -> game.current_scene <- next_scene
		| None -> game.should_exit <- true;

	game.scene_tick <- game.scene_tick + 1

let save_game_data (_: game_t) = ()

let setup_game () = {
	tick = 0;
	scene_tick = 0;
	switch_scene = false;
	should_exit = false;
	sprite_names = Hashtbl.create 50;
	sprites = [];
	spritesheet = load_texture_from_image
		(load_image "./resources/spritesheet.png");
	scene_spritesheets = [load_texture_from_image
		(load_image "./resources/scene1-spritsheet.png")];
	scenes = [{
		sprites = [];
		spritesheet = 0;
		contents = Planned (Hashtbl.create 0);
		next_scene = None
	}];
	current_scene = 0;
	dialogs = [];
	font = load_font "./resources/fonts/RobotoMono-Medium.ttf";
	font_ratio = 1.;
}

let setup () =
	init_window 1280 720 "English Final";
	set_target_fps 60;
	setup_game ()

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
