myHomeDir = os.getenv("HOME")

-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers

beautiful.init(myHomeDir .. "/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "myterm"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
   {
   awful.layout.suit.tile,
   awful.layout.suit.floating,
   awful.layout.suit.fair,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   tag2 = "shell"
   tag3 = "web"
   tag6 = "procman"
   if screen.count() == 2 then
      tag2 = "nautilus"
      tag3 = "misc"
      tag6 = "vbox"
   end
   if s == 1 then
      tags[s] = awful.tag({ "[1] emacs", "[2] " .. tag2, "[3] " .. tag3, "[4] matlab", "[5] ipn", "[6] " .. tag6, 7, 8, "[9] spotify" },
                          s, 
                          {layouts[1], layouts[2], layouts[1], layouts[2], layouts[2], layouts[2], layouts[1], layouts[1], layouts[1]})
   elseif s == 2 then
      if screen.count () == 2 then
         tags[s] = awful.tag({ "[1] web", "[2] shell", "[3] procman", "[4] evince", "[5] kodi", 6, 7, 8, 9 },
                             s, 
                             {layouts[1], layouts[2], layouts[2], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1]})
      else
         tags[s] = awful.tag({ "[1] emacs", "[2] " .. tag2, "[3] " .. tag3, "[4] matlab", "[5] ipn", "[6] " .. tag6, 7, 8, "[9] spotify" },
                             s, 
                             {layouts[1], layouts[2], layouts[1], layouts[2], layouts[2], layouts[2], layouts[1], layouts[1], layouts[1]})
      end
   elseif s == 3 then
      tags[s] = awful.tag({ "[1] web", "[2] shell", "[3] procman", "[4] evince", "[5] kodi", 6, 7, 8, 9 },
                          s, 
                          {layouts[1], layouts[2], layouts[2], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1]})
   else
      tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9  }, s, layouts[1])
   end
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Firefox", "firefox" },
                                    { "Nautilus", "file-manager" },
                                    { "Terminal", terminal },
				 }
		       })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" }, "%a %B %d, %l:%M %P" )

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ }, 4, awful.tag.viewnext),
   awful.button({ }, 5, awful.tag.viewprev)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
			   if not c:isvisible() then
			      awful.tag.viewonly(c:tags()[1])
			   end
			   client.focus = c
			   c:raise()
			end),
   awful.button({ }, 3, function ()
			   if instance then
			      instance:hide()
			      instance = nil
			   else
			      instance = awful.menu.clients({ width=250 })
			   end
			end),
   awful.button({ }, 4, function ()
			   awful.client.focus.byidx(1)
			   if client.focus then client.focus:raise() end
			end),
   awful.button({ }, 5, function ()
			   awful.client.focus.byidx(-1)
			   if client.focus then client.focus:raise() end
			end))

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
			     awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
			     awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
			     awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
			     awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(function(c)
					    return awful.widget.tasklist.label.currenttags(c, s)
					 end, mytasklist.buttons)

   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s })
   -- Add widgets to the wibox - order matters
   mywibox[s].widgets = {
      {
	 mylauncher,
	 mytaglist[s],
	 mypromptbox[s],
	 layout = awful.widget.layout.horizontal.leftright
      },
      mylayoutbox[s],
      mytextclock,
      s == 1 and mysystray or nil,
      mytasklist[s],
      layout = awful.widget.layout.horizontal.rightleft
   }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
		awful.button({ }, 2, awful.tag.viewtoggle),
		awful.button({ }, 3, function () mymainmenu:toggle() end),
		awful.button({ }, 4, awful.tag.viewnext),
		awful.button({ }, 5, awful.tag.viewprev)
	  ))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   awful.key({ modkey,           }, "j",
	     function ()
		awful.client.focus.byidx(-1)
		if client.focus then client.focus:raise() end
	     end),
   awful.key({ modkey,           }, "k",
	     function ()
		awful.client.focus.byidx( 1)
		if client.focus then client.focus:raise() end
	     end),
   awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(-1)    end),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( 1)    end),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative(-1) end),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
	     function ()
		awful.client.focus.history.previous()
		if client.focus then
		   client.focus:raise()
		end
	     end),

   -- Standard program
   -- My favorite programs
   awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
   awful.key({ modkey, "Shift"   }, "f", function () awful.util.spawn("firefox") end),
   awful.key({ modkey, "Shift"   }, "g", function () awful.util.spawn("firefox") end),
   awful.key({ modkey, "Shift"   }, "e", function () awful.util.spawn("emacsclient -nc") end),
   awful.key({ modkey, "Shift"   }, "m", function () awful.util.spawn("file-manager") end),
   awful.key({ modkey, "Shift"   }, "a", function () awful.util.spawn("matlab -desktop") end),
   awful.key({ }, "XF86Calculator", function () awful.util.spawn("gnome-calculator") end),
   awful.key({ modkey, "Shift"   }, "v", function () awful.util.spawn("nvidia-settings") end),
   awful.key({ modkey, "Shift"   }, "b", function () awful.util.spawn("blur-lock") end),
   awful.key({ modkey,           }, "d", function () awful.util.spawn("zeal") end),
   awful.key({ modkey, "Control" }, "r", awesome.restart),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   -- Prompt
   awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

   -- Multimedia keys
   awful.key({ }, "XF86AudioRaiseVolume",    
	     function () awful.util.spawn("vol-ctrl -i 2") end),
   awful.key({ }, "XF86AudioLowerVolume",   
	     function () awful.util.spawn("vol-ctrl -d 2") end),
   awful.key({ }, "XF86AudioMute",   
	     function () awful.util.spawn("vol-ctrl -t") end),
   awful.key({ }, "F11",   
	     function () awful.util.spawn("gnome-screenshot -i") end),
   awful.key({ }, "F12",   
	     function () awful.util.spawn("pavucontrol") end),

   awful.key({ modkey }, "x",
	     function ()
		awful.prompt.run({ prompt = "Run Lua code: " },
				 mypromptbox[mouse.screen].widget,
				 awful.util.eval, nil,
				 awful.util.getdir("cache") .. "/history_eval")
	     end),

   -- all minimized clients are restored 
   awful.key({ modkey, "Shift"   }, "n", 
	     function()
		local tag = awful.tag.selected()
                for i=1, #tag:clients() do
		   tag:clients()[i].minimized=false
		   tag:clients()[i]:redraw()
		end
	     end),

    -- renaming trick
    awful.key({ modkey}, "c",
              function ()
                  awful.prompt.run({ prompt = "nickname this tag: " },
                  mypromptbox[mouse.screen].widget,
                  function(text)
                    if awful.tag.getidx(awful.tag.selected(mouse.screen)) > 10 then
                      return
                    end
                    if text == "" then
                      awful.tag.selected(mouse.screen).name = 
                        awful.tag.getidx(awful.tag.selected(mouse.screen))
                    else
                      awful.tag.selected(mouse.screen).name = 
                        "[" .. awful.tag.getidx(awful.tag.selected(mouse.screen))
                            .. "] " .. text
                    end
                  end, nil,
                  awful.util.getdir("cache") .. "/history_nickname")
              end)

    -- Uncomment for old versions of awesome (like the one in Ubuntu 10.04)
    -- -- renaming trick
    -- awful.key({ modkey}, "c",
    --           function ()
    --               awful.prompt.run({ prompt = "nickname this tag: " },
    --               mypromptbox[mouse.screen].widget,
    --               function(text)
    --                  awful.tag.selected(mouse.screen).name = 
    --                  string.sub(awful.tag.selected(mouse.screen).name, 1, 1)
    --                  .. " " .. text
    --               end, nil,
    --               awful.util.getdir("cache") .. "/history_nickname")
    --            end)
)

clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey            }, "F1",     function (c) c:kill()                         end),
   awful.key({                   },  "#225",  function (c) c:kill()                         end),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
   awful.key({ modkey,           }, "m",
	     function (c)
		c.maximized_horizontal = not c.maximized_horizontal
		c.maximized_vertical   = not c.maximized_vertical
	     end),
    awful.key({ modkey }, "F2", function (c)
       if   c.titlebar then awful.titlebar.remove(c)
       else awful.titlebar.add(c, { modkey = modkey }) end
    end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
   globalkeys = awful.util.table.join(globalkeys,
      awful.key({ modkey }, "#" .. i + 9,
	 function ()
	    local screen = mouse.screen
	    if tags[screen][i] then
	       awful.tag.viewonly(tags[screen][i])
	    end
	 end),
      awful.key({ modkey, "Control" }, "#" .. i + 9,
	 function ()
	    local screen = mouse.screen
	    if tags[screen][i] then
	       awful.tag.viewtoggle(tags[screen][i])
	    end
	 end),
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
         function ()
	    if client.focus and tags[client.focus.screen][i] then
	       awful.client.movetotag(tags[client.focus.screen][i])
	    end
	 end),
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
	 function ()
	    if client.focus and tags[client.focus.screen][i] then
	       awful.client.toggletag(tags[client.focus.screen][i])
	    end
	 end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ }, 9, function (c) c:kill() end),
   awful.button({ modkey }, 3, awful.mouse.client.resize))
   -- awful.button({ }, 9, function (c) 
   --                         c.maximized_horizontal = not c.maximized_horizontal
   --                         c.maximized_vertical   = not c.maximized_vertical
   --                         client.focus = c; c:raise()
   --                      end))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
		    border_color = beautiful.border_normal,
		    focus = true,
		    keys = clientkeys,
		    buttons = clientbuttons } },
   { rule = { class = "MPlayer" },
     properties = { floating = true } },
   { rule = { class = "pinentry" },
     properties = { floating = true } },
   { rule = { class = "gimp" },
     properties = { floating = true } },
   { rule = { name = "Snes9x" },
     properties = { floating = true } },
   { rule = { name = "Ediff" },
     properties = { floating = true } },
   { rule = { name = "Zeal" },
     properties = { floating = true } },
   -- Set Firefox to always map on tags number 2 of screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", 
   function (c, startup)
      -- Add a titlebar
      -- awful.titlebar.add(c, { modkey = modkey })

      -- Enable sloppy focus
      c:add_signal("mouse::enter", 
		   function(c)
		      if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
		      and awful.client.focus.filter(c) then
		      client.focus = c
		   end
		end)

      if not startup then
	 -- Set the windows at the slave,
	 -- i.e. put it at the end of others instead of setting it master.
	 awful.client.setslave(c)

	 -- Put windows in a smart way, only if they does not set an initial position.
	 if not c.size_hints.user_position and not c.size_hints.program_position then
	    awful.placement.no_overlap(c)
	    awful.placement.no_offscreen(c)
	 end
      end
   end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
