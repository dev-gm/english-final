open Utils

type t = {
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
	mutable dialogs: Sprite.dialog_t list;
	font: Font.t;
	font_ratio: float (*width/height*)
}

let bot_ai (_: t) () : Sprite.action_t list = []

let handle_user_input (sprite: Sprite.t) (game: t) : Sprite.action_t list =
	let switch_scene = Array.fold_right
		(fun (other_sprite: Sprite.t) switch_scene -> switch_scene && (
			(sprite.sprite_type <> Sprite.Other) &&
			other_sprite.dead && (sprite.team <> other_sprite.team)
		)) game.sprites true
	in
	if switch_scene then
		game.switch_scene <- true;
	let open Key in
	let rec get_key_presses keys_pressed =
		let key = get_key_pressed () in
		if key <> Null then
			get_key_presses (key :: keys_pressed)
		else
			keys_pressed
	in
	let keys = get_key_presses [] in
	let actions = ref [] in
	let map_key_input key action =
		if List.exists (fun k -> k <> key) keys then
			actions := action :: !actions
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

let save_data (_: t) = ()


