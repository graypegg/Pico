module LibPico.Math where

libPicoMath :: ([String],[String],String)
libPicoMath = ([
			"%add",
			"[/0]~addloop",
			"@<",
			"+1",
			"@>",
			"-1",
			"[END]~addloop",
			"@<",
			"/add",
			"%sub",
			"[/0]~subloop",
			"@<",
			"-1",
			"@>",
			"-1",
			"[END]~subloop",
			"@<",
			"/sub",
			"%addForward",
			"[/0]~addForwardloop",
			"-1",
			"@>",
			"+1",
			"@<",
			"[END]~addForwardloop",
			"@>",
			"/addForward",
			"%subForward",
			"[/0]~subForwardloop",
			"-1",
			"@>",
			"-1",
			"@<",
			"[END]~subForwardloop",
			"@>",
			"/subForward",
			"%mul",
			"@<",
			"[/0]~mulloop",
			"-1",
			"@>",
			"[/0]~mulloop2",
			"-1",
			"@>",
			"+1",
			"@>",
			"+1",
			"@<2",
			"[END]~mulloop2",
			"@>2",
			"[/0]~mulloop3",
			"-1",
			"@<2",
			"+1",
			"@>2",
			"[END]~mulloop3",
			"@<3",
			"[END]~mulloop",
			"@>2",
			"/mul"
			],
			["add","sub","addForward","subForward","mul"],
			"libpico.math")