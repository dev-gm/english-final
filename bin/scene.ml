type planned_t =
	| Action of Sprite.action_t
	| SwitchScene

type contents_t =
	| Planned of (int, string * planned_t) Hashtbl.t (* tick, (sprite, action). every tick this hashtbl is checked *)
	| Fight

type t = {
	spritesheet: int; (* index of scene_spritesheets *)
	contents: contents_t;
	next_scene: int option (* if None, end game *)
}

