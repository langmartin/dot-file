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

local function twoScreens()
   return hs.screen.find(external) ~= nil
end

local function sideScreen()
   return twoScreens() and laptop or external
end

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
   v = 0,
   v_def = false
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

local function twoDefaultVolume()
   if two.v_def then
      twoSet('v', 0)
      two.v_def = false
   else
      twoSet('v', 10)
      two.v_def = true
   end
end

local function chatOnImpl(screen, slack)
   local lv = {x=0, y=0, w=0.5, h=0.5}
   local rv = {x=0.5, y=0, w=0.5, h=0.5}

   hs.layout.apply({
	 {"Signal", nil, screen, lv, nil, nil},
	 {"Messages", nil, screen, rv, nil, nil},
	 {"Slack", nil, screen, slack, nil, nil},
	 {"Discord", nil, screen, maximized, nil, nil},
   })

   focusSome({"Discord", "Messages", "Signal", "Slack"}, 4)
end

local function chatOn(screen)
   chatOnImpl(screen, maximized)
end

local function chatTileOn(screen)
   chatOnImpl(screen, bottom50)
end

function calOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Calendar", nil, screen, maximized, nil, nil},
   })
end

function zoom()
   local screen = sideScreen()
   local zoom = hs.application.find("zoom")
   local win zoom:findWindow("zoom")
   if (win and win:title() == "Zoom") then
      win:close()
   end

   hs.layout.apply({
	 {"zoom.us", "Zoom Meeting", screen, maximized, nil, nil},
   })
end

local function hackOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, maximized, nil, nil},
	 {browser, nil, screen, maximized, nil, nil},
	 {"Preview", nil, screen, maximized, nil, nil},
   })
   focusSome({browser, "Emacs"}, 2)
end

local function readOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, left40, nil, nil},
	 {browser, nil, screen, right60, nil, nil},
	 {"Preview", nil, screen, right60, nil, nil},
   })
   focusSome({browser, "Emacs"}, 2)
end

local function buildOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, bottom20, nil, nil},
	 {browser, nil, screen, top80, nil, nil},
	 {"Preview", nil, screen, top80, nil, nil},
   })
   focusSome({browser, "Emacs"},  2)
end

local function slacktermOn(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top50, nil, nil},
	 {"Slack", nil, screen, bottom50, nil, nil},
   })
end

local function default()
   if (twoScreens()) then
      calOn(laptop)
      slacktermOn(external)
      chatOn(external)
      hackOn(external)
   else
      chatOn(laptop)
      hackOn(laptop)
   end
end

local function chat()
   if (twoScreens()) then
      calOn(external)
      hackOn(external)
      chatTileOn(laptop)
   else
      chatTileOn(laptop)
   end
end

local function build()
   if (hs.screen.find(external)) then
      calOn(external)
      chatOn(external)
      slacktermOn(laptop)
      buildOn(external)
   else
      buildOn(laptop)
   end
end

-- ----------------------------------------------------------------------
-- hooks

local function startWatchers()
   hs.screen.watcher.new(default):start()
   -- hs.application.watcher.new(function (appName, event, app)
   -- 	 if (appName == "zoom.us" and event == "lauched") then
   -- 	    zoom()
   -- 	 end
   -- end):start()
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

startWatchers()

hs.hotkey.bind(hyper, "d", default)
hs.hotkey.bind(hyper, "c", chat)
hs.hotkey.bind(hyper, "h", hackOn)
hs.hotkey.bind(hyper, "r", readOn)
hs.hotkey.bind(hyper, "b", build)
hs.hotkey.bind(hyper, "z", zoom)
hs.hotkey.bind(hyper, "tab", throw)
hs.hotkey.bind(hyper, ".", hs.reload)

hs.hotkey.bind(hyper, "1", twoDimmer)
hs.hotkey.bind(hyper, "2", twoBrighter)
hs.hotkey.bind(hyper, "-", twoQuieter)
hs.hotkey.bind(hyper, "=", twoLouder)
hs.hotkey.bind(hyper, "0", twoDefaultVolume)
