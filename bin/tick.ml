open Utils
open Raylib

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
						let allow_input = List.fold_right
							(fun (dialog: Sprite.dialog_t) allow_input ->
								allow_input || dialog.allow_input
							) game.dialogs false
						in
						if allow_input then
							Game.handle_user_input sprite game
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
			if (x sprite.dest_rect) < 0. then
				set_x sprite.dest_rect 0.
			else if (x sprite.dest_rect) > (float_of_int game.width) then
				set_x sprite.dest_rect (float_of_int game.width)
			else if (y sprite.dest_rect) < 0. then
				set_y sprite.dest_rect 0.
			else if (y sprite.dest_rect) > (float_of_int game.height) then
				set_y sprite.dest_rect (float_of_int game.width);
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

