open Utils

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


