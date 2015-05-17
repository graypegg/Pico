module LibPico.CellManagement where

libPicoCellManagement :: ([String],[String],String)
libPicoCellManagement = ([
						"%copy",
						"[/0]~copyloop1",
						"-1",
						"@>",
						"+1",
						"@>",
						"+1",
						"@<",
						"@<",
						"[END]~copyloop1",
						"@>",
						"@>",
						"[/0]~copyloop2",
						"-1",
						"@<",
						"@<",
						"+1",
						"@>",
						"@>",
						"[END]~copyloop2",
						"@<",
						"@<",
						"/copy",
						"%clear",
						"$copy",
						"$subFoward",
						"/clear"
						],
						["copy","clear"],
						"libpico.cellman")