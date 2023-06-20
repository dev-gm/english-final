let setup () =
	Array.get Sys.argv 1
		|> Yojson.Basic.from_file
		|> Config.of_json
		|> Config.to_game

let rec loop (game: Game.t) =
	match game.should_exit with
	| true ->
			Sdlimage.quit ();
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
