open Utils

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
	| EnterScene of vector_t (* Enter is probably a Key *)
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
	mutable velocity: vector_t; (* every tick it is modified based on angle, accel, air_resistance. then position is moved by velocity *)
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
	| Spritesheet of bool * Sdl.Rect.t (*use_scene_spritesheet, src_rect*)
	| Color of color_t

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
	mutable dest_rect: Sdl.Rect.t;
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
	let center: vector_t = {
		x = (sprite.dest_rect.x + sprite.dest_rect.w) / 2;
		y = (sprite.dest_rect.y + sprite.dest_rect.h) / 2
	} in
	let vector_end: vector_t = {
		x = center.x + ((float_of_int range) *.
			(cos (radian_of_degree angle)) |> int_of_float);
		y = center.y + ((float_of_int range) *.
			(sin (radian_of_degree angle)) |> int_of_float)
	} in
	let i = ref 0 in
	Array.get (Array.fold_left
		(fun filtered sprite ->
			let intersection =
				Sdl.Rect.intersect_rect_and_line
					~rect:sprite.dest_rect
					~p1:(tuple_of_vector center)
					~p2:(tuple_of_vector vector_end)
			in
			if Option.is_some intersection then
				Array.unsafe_set filtered !i (Some sprite);
				i := !i + 1;
			filtered
		) (Array.make ((Array.length sprites) + 1) None) sprites) 0

let correct_movement_for_collision (a: Sdl.Rect.t) (b: Sdl.Rect.t) (mvmt: vector_t) =
	let bounds: Sdl.Rect.t =
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
		if Sdl.Rect.has_intersection ~a:bounds ~b:b then
			let four_corners (r: Sdl.Rect.t) =
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
			) (four_corners a) (four_corners (rect_add_vector a mvmt))
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
let correct_movement_for_collisions sprite movement sprites : vector_t * bool =
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
	(movement: vector_t)
	(sprites: t Array.t)
	: vector_t * t list
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

