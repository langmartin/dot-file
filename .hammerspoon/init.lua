local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0

local browser = "Safari"
local browser2 = "Firefox"

local laptop_name = 'built'
local external_name = 'lg'

local function laptop()
   return hs.screen.find(laptop_name)
end

local function external()
   return hs.screen.find(external_name)
end

local maximized = hs.layout.maximized
local left = hs.layout.left50
local right = hs.layout.right50
local left40 = {x=0, y=0, w=0.4, h=1}
local right60 = {x=0.4, y=0, w=0.6, h=1}
local top50 = {x=0, y=0, w=1, h=0.5}
local top30 = {x=0, y=0, w=1, h=0.3}
local bottom70 = {x=0, y=0.3, w=1, h=0.7}
local bottom50 = {x=0, y=0.5, w=1, h=0.5}
local bottom20 = {x=0, y=0.8, w=1, h=0.2}
local top80 = {x=0, y=0, w=1, h=0.8}

local topLeft = {x=0, y=0, w=0.5, h=0.5}
local topRight = {x=0.5, y=0, w=0.5, h=0.5}
local bottomLeft = {x=0, y=0.5, w=0.5, h=0.5}
local bottomRight = {x=0.5, y=0.5, w=0.5, h=0.5}

local function twoScreens()
   return external() ~= nil
end

local function sideScreen()
   return twoScreens() and laptop() or external()
end

local function focusSome(apps)
   size = #(apps)
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

local function brightBy(current)
   by = 10
   if (current <= 3)
   then
      by = 1
   elseif (current <= 10)
   then
         by = 2
   elseif (current <= 20)
   then
         by = 4
   end
   return by
end

local function twoBrighter()
   two.b = two.b + brightBy(two.b)
   two.b = math.min(100, two.b)
   twoSet('b', two.b)
end

local function twoDimmer()
   two.b = two.b - brightBy(two.b)
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
   if two.v > 0 then
      twoSet('v', 0)
      two.v = 0
   else
      twoSet('v', 10)
      two.v = 10
   end
end

local function chatOnImpl(screen, slack)
   hs.layout.apply({
	 {"Signal", nil, screen, topLeft, nil, nil},
	 {"Messages", nil, screen, topRight, nil, nil},
	 {"Slack", nil, screen, slack, nil, nil},
	 {"Keybase", nil, screen, topRight, nil, nil},
	 {"Discord", nil, screen, maximized, nil, nil},
   })

   focusSome({"Keybase", "Discord", "Messages", "Signal", "Slack"})
end

local function chatOn(screen)
   chatOnImpl(screen, maximized)
end

local function chatTileOn(screen)
   chatOnImpl(screen, bottomLeft)
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

local function maxSide()
   local win = hs.window.focusedWindow()
   win:moveToScreen(sideScreen())
   win:maximize()
end

local function hackOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, maximized, nil, nil},
	 {browser, nil, screen, maximized, nil, nil},
	 {browser2, nil, screen, maximized, nil, nil},	 
	 {"Preview", nil, screen, maximized, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function readOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, left40, nil, nil},
	 {browser, nil, screen, right60, nil, nil},
	 {browser2, nil, screen, right60, nil, nil},
	 {"Preview", nil, screen, right60, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function buildOn(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, bottom20, nil, nil},
	 {browser, nil, screen, top80, nil, nil},
	 {browser2, nil, screen, top80, nil, nil},
	 {"Preview", nil, screen, top80, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function termOn(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top50, nil, nil},
	 {"iTerm2", nil, screen, top50, nil, nil},
   })
end

local function slacktermOn(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top30, nil, nil},
	 {"Slack", nil, screen, bottom70, nil, nil},
   })
end

local function default()
   -- hs.alert("default: " .. hs.screen.find(external):name())
   if (twoScreens()) then
      calOn(laptop())
      termOn(external())
      chatOn(external())
      hackOn(external())
   else
      chatOn(laptop())
      hackOn(laptop())
   end
end

local function chat()
   if (twoScreens()) then
      calOn(external())
      hackOn(external())
      chatTileOn(laptop())
   else
      chatTileOn(laptop())
   end
end

local function build()
   if (hs.screen.find(external())) then
      calOn(external())
      chatOn(external())
      slacktermOn(laptop())
      buildOn(external())
   else
      buildOn(laptop())
   end
end

-- ----------------------------------------------------------------------
-- hooks

hs.screen.watcher.new(function ()
      default()
end):start()

-- hs.application.watcher.new(function (appName, event, app)
--       if (appName == "zoom.us" and event == "lauched") then
-- 	 zoom()
--       end
-- end):start()

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
hs.hotkey.bind(hyper, "h", hackOn)
hs.hotkey.bind(hyper, "r", readOn)
hs.hotkey.bind(hyper, "b", build)
hs.hotkey.bind(hyper, "z", maxSide)
hs.hotkey.bind(hyper, "tab", throw)

hs.hotkey.bind(hyper, "1", twoDimmer)
hs.hotkey.bind(hyper, "2", twoBrighter)
hs.hotkey.bind(hyper, "-", twoQuieter)
hs.hotkey.bind(hyper, "=", twoLouder)
hs.hotkey.bind(hyper, "0", twoDefaultVolume)
