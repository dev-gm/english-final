open Raylib

let setup () =
	init_window 1280 720 "";
	let game = Array.get Sys.argv 1
		|> Yojson.Basic.from_file
		|> Config.of_json
		|> Config.to_game
	in
	set_window_size game.width game.height;
	set_window_title game.title;
	set_target_fps game.fps;
	let _ = game.time_per_frame in
	game

let rec loop (game: Game.t) =
	match window_should_close () || game.should_exit with
	| true ->
			Game.save_data game;
			close_window ()
	| false ->
			(*let start_time = get_time () in*)
			begin_drawing ();
			Tick.game game;
			Render.game game;
			end_drawing ();
			swap_screen_buffer ();
			(*let time_taken = (get_time ()) -. start_time in
			let time_left = game.time_per_frame -. time_taken in
			if time_left > 0. then
				wait_time time_left;*)
			loop game

let () = setup () |> loop
