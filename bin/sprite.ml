open Utils
open Raylib

type dialog_t = {
	sprite_name: string;
	contents: string;
	mutable left: int;
	time_until_next_char: int; (* every tick, render letters of dialog based on speed. left is number of letters left to read. at end, check if left = 0. if so, reset dialog. *)
	mutable left_until_next_char: int; (*render when left = 0, reset to time*)
	allow_input: bool
}

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

