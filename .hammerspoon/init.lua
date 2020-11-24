local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0

local browser = "Firefox"

local maximized = hs.layout.maximized
local left = hs.layout.left50
local right = hs.layout.right50
local bottom50 = {x=0, y=0.5, w=1, h=0.5}
local bottom20 = {x=0, y=0.8, w=1, h=0.2}
local top80 = {x=0, y=0, w=1, h=0.8}

local function focusSome(apps, size)
   for i = 1,size do
      local app = hs.application.get(apps[i])
      if (app)
      then
	 app:activate()
      end
   end
end

local function chat(screen)
   local lv = {x=0, y=0, w=0.5, h=0.5}
   local rv = {x=0.5, y=0, w=0.5, h=0.5}
   local h1 = {x=0, y=0.5, w=1, h=0.5}

   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Signal", nil, screen, lv, nil, nil},
	 {"Messages", nil, screen, rv, nil, nil},
	 {"Slack", nil, screen, h1, nil, nil},
	 {"Discord", nil, screen, maximized, nil, nil},
   })

   focusSome({"Discord", "Messages", "Signal", "Slack"}, 4)
end

local function cal(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Calendar", nil, screen, maximized, nil, nil},
   })
end

local function hack(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, maximized, nil, nil},
	 {browser, nil, screen, maximized, nil, nil},
	 {"Preview", nil, screen, maximized, nil, nil},
   })
   focusSome({browser, "Emacs"}, 2)
end

local function read(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, left, nil, nil},
	 {browser, nil, screen, right, nil, nil},
	 {"Preview", nil, screen, right, nil, nil},
   })
   focusSome({browser, "Emacs"}, 2)
end

local function build(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, bottom20, nil, nil},
	 {browser, nil, screen, top80, nil, nil},
	 {"Preview", nil, screen, top80, nil, nil},
   })
   focusSome({browser, "Emacs"},  2)
end

local function default()
   local ss = hs.screen.allScreens()
   local lp = 'Color LCD'
   local ex = 'LG HDR 4K'

   if (ss[2] ~= nil)
   then
      chat(ex)
      cal(lp)
      hack(ex)
   else
      chat(lp)
      cal(lp)
      hack(lp)
   end
end

-- ----------------------------------------------------------------------
-- bindings

spoon.MiroWindowsManager:bindHotkeys({
      up = {hyper, "up"},
      right = {hyper, "right"},
      down = {hyper, "down"},
      left = {hyper, "left"},
      fullscreen = {hyper, "f"}
})

hs.hotkey.bind(hyper, "d", default)
hs.hotkey.bind(hyper, "c", chat)
hs.hotkey.bind(hyper, "h", hack)
hs.hotkey.bind(hyper, "r", read)
hs.hotkey.bind(hyper, "b", build)
hs.hotkey.bind(hyper, "0", hs.reload)
