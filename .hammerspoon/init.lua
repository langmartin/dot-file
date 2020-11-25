local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0

local browser = "Firefox"

local laptop = 'Color LCD'
local external = 'LG HDR 4K'

local maximized = hs.layout.maximized
local left = hs.layout.left50
local right = hs.layout.right50
local left40 = {x=0, y=0, w=0.4, h=1}
local right60 = {x=0.4, y=0, w=0.6, h=1}
local top50 = {x=0, y=0, w=1, h=0.5}
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

local function throw()
   local win = hs.window.focusedWindow()
   local next = win:screen():toWest()
   if (next) then
      win:moveOneScreenWest(true, true)
   else
      win:moveOneScreenEast(true, true)
   end
end

local function focusTop()
   local win = hs.window.frontmostWindow()
   win:focus()
end

local two = {
   path = '/Users/lang/contrib/ddcctl/ddcctl',
   b = 0,
   v = 0
}

function twoSet(attr, number)
   hs.task.new(
      two.path,
      function(code, out, err)
	 -- print('out: ' .. out)
	 -- print('err: ' .. err)
      end,
      {'-d', '1', '-' .. attr, tostring(number)}
   ):start()
end

local function twoBrighter()
   two.b = two.b + 10
   two.b = math.min(100, two.b)
   twoSet('b', two.b)
end

local function twoDimmer()
   two.b = two.b - 10
   two.b = math.max(0, two.b)
   twoSet('b', two.b)
end

local function twoLouder()
   two.v = two.v + 1
   two.v = math.min(100, two.v)
   twoSet('v', two.v)
end

local function twoQuieter()
   two.v = two.v - 1
   two.v = math.max(0, two.v)
   twoSet('v', two.v)
end

local function chat(screen)
   local lv = {x=0, y=0, w=0.5, h=0.5}
   local rv = {x=0.5, y=0, w=0.5, h=0.5}
   local h1 = {x=0, y=0.5, w=1, h=0.5}

   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Signal", nil, screen, lv, nil, nil},
	 {"Messages", nil, screen, rv, nil, nil},
	 {"Slack", nil, screen, bottom50, nil, nil},
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
	 {"Emacs", nil, screen, left40, nil, nil},
	 {browser, nil, screen, right60, nil, nil},
	 {"Preview", nil, screen, right60, nil, nil},
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

local function slackterm(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top50, nil, nil},
	 {"Slack", nil, screen, bottom50, nil, nil},
   })
end

local function default()
   if (hs.screen.allScreens()[2] ~= nil)
   then
      chat(external)
      cal(laptop)
      hack(external)
   else
      chat(laptop)
      cal(laptop)
      hack(laptop)
   end
end

local function slack()
   if (hs.screen.allScreens()[2] ~= nil) then
      cal(external)
      hack(external)
      chat(laptop)
   else
      chat(laptop)
   end
end

local function alt()
   if (hs.screen.allScreens()[2] ~= nil) then
      cal(external)
      build(external)
      slackterm(laptop)
   else
      build(laptop)
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
hs.hotkey.bind(hyper, "s", slack)
hs.hotkey.bind(hyper, "a", alt)
hs.hotkey.bind(hyper, "c", chat)
hs.hotkey.bind(hyper, "h", hack)
hs.hotkey.bind(hyper, "r", read)
hs.hotkey.bind(hyper, "b", build)
hs.hotkey.bind(hyper, "tab", throw)
hs.hotkey.bind(hyper, "0", hs.reload)

hs.hotkey.bind(hyper, "1", twoDimmer)
hs.hotkey.bind(hyper, "2", twoBrighter)
hs.hotkey.bind(hyper, "-", twoQuieter)
hs.hotkey.bind(hyper, "=", twoLouder)
