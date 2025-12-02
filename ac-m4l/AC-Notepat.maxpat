{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 8,
			"minor" : 6,
			"revision" : 5,
			"architecture" : "x64",
			"modernui" : 1
		},
		"classnamespace" : "box",
		"rect" : [ 100.0, 100.0, 900.0, 700.0 ],
		"bglocked" : 0,
		"openinpresentation" : 1,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "",
		"assistshowspatchername" : 0,
		"boxes" : [ 			{
				"box" : 				{
					"id" : "obj-jweb",
					"maxclass" : "jweb",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 30.0, 120.0, 800.0, 500.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 10.0, 50.0, 780.0, 440.0 ],
					"rendermode" : 0,
					"url" : "https://aesthetic.computer/notepat"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-vol",
					"maxclass" : "live.dial",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 30.0, 30.0, 50.0, 48.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 10.0, 5.0, 50.0, 48.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_initial" : [ 80 ],
							"parameter_initial_enable" : 1,
							"parameter_longname" : "Volume",
							"parameter_mmax" : 100.0,
							"parameter_shortname" : "Vol",
							"parameter_type" : 0,
							"parameter_unitstyle" : 5
						}

					},
					"varname" : "Volume"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-inst",
					"maxclass" : "live.menu",
					"numinlets" : 1,
					"numoutlets" : 3,
					"outlettype" : [ "", "", "float" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 100.0, 45.0, 100.0, 15.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 70.0, 20.0, 100.0, 15.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_enum" : [ "notepat", "chord", "keys", "drum" ],
							"parameter_longname" : "Instrument",
							"parameter_mmax" : 3,
							"parameter_shortname" : "Inst",
							"parameter_type" : 2
						}

					},
					"varname" : "Instrument"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-local",
					"maxclass" : "live.toggle",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"parameter_enable" : 1,
					"patching_rect" : [ 220.0, 45.0, 15.0, 15.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 180.0, 20.0, 15.0, 15.0 ],
					"saved_attribute_attributes" : 					{
						"valueof" : 						{
							"parameter_enum" : [ "off", "on" ],
							"parameter_longname" : "Local Dev",
							"parameter_mmax" : 1,
							"parameter_shortname" : "Local",
							"parameter_type" : 2
						}

					},
					"varname" : "LocalDev"
				}

			}
, 			{
				"box" : 				{
					"fontsize" : 10.0,
					"id" : "obj-label-local",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 240.0, 45.0, 50.0, 18.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 200.0, 20.0, 40.0, 18.0 ],
					"text" : "Local"
				}

			}
, 			{
				"box" : 				{
					"fontface" : 1,
					"fontsize" : 14.0,
					"id" : "obj-title",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 350.0, 45.0, 150.0, 22.0 ],
					"presentation" : 1,
					"presentation_rect" : [ 350.0, 15.0, 150.0, 22.0 ],
					"text" : "AC Notepat"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-msg-url",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 100.0, 80.0, 250.0, 22.0 ],
					"text" : "url https://aesthetic.computer/notepat"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-sprintf",
					"maxclass" : "newobj",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 100.0, 650.0, 300.0, 22.0 ],
					"text" : "sprintf url https://aesthetic.computer/%s"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-jweb", 0 ],
					"source" : [ "obj-msg-url", 0 ]
				}

			}
 ],
		"parameters" : 		{
			"obj-inst" : [ "Instrument", "Inst", 0 ],
			"obj-local" : [ "Local Dev", "Local", 0 ],
			"obj-vol" : [ "Volume", "Vol", 0 ],
			"parameterbanks" : 			{
				"0" : 				{
					"index" : 0,
					"name" : "",
					"parameters" : [ "-", "-", "-", "-", "-", "-", "-", "-" ]
				}

			},
			"inherited_shortname" : 1
		},
		"dependency_cache" : [  ],
		"autosave" : 0
	}

}
