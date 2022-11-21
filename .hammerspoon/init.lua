local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0

local browser = "Firefox"
local browser2 = "Safari"

local laptop_name = 'built'
local external_name = 'lg'

local function laptop()
   return hs.screen.find(laptop_name)
end

local function external()
   return hs.screen.find(external_name)
end

local function twoScreens()
   return external() ~= nil
end

local function sideScreen()
   return twoScreens() and laptop() or external()
end

function verticalp()
   if (not(twoScreens())) then
      return false
   end
   local s = external()
   local f = s:frame()
   return f.h > f.w
end

local maximized = hs.layout.maximized
local left = hs.layout.left50
local right = hs.layout.right50
local left40 = {x=0, y=0, w=0.4, h=1}
local right60 = {x=0.4, y=0, w=0.6, h=1}
local top80 = {x=0, y=0, w=1, h=0.8}
local top50 = {x=0, y=0, w=1, h=0.5}
local top40 = {x=0, y=0, w=1, h=0.4}
local top30 = {x=0, y=0, w=1, h=0.3}
local top10 = {x=0, y=0, w=1, h=0.1}
local mid80 = {x=0, y=0.1, w=1, h=0.8}
local bottom70 = {x=0, y=0.3, w=1, h=0.7}
local bottom60 = {x=0, y=0.4, w=1, h=0.6}
local bottom50 = {x=0, y=0.5, w=1, h=0.5}
local bottom20 = {x=0, y=0.8, w=1, h=0.2}
local bottom10 = {x=0, y=0.9, w=1, h=0.1}
local topLeft = {x=0, y=0, w=0.5, h=0.5}
local topRight = {x=0.5, y=0, w=0.5, h=0.5}
local bottomLeft = {x=0, y=0.5, w=0.5, h=0.5}
local bottomRight = {x=0.5, y=0.5, w=0.5, h=0.5}
local bottomRightDiscord = {x=0.5, y=0.4, w=0.5, h=0.6}

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
	 {"Slack", nil, screen, maximized, nil, nil},
	 {"Keybase", nil, screen, topRight, nil, nil},
	 {"Discord", nil, screen, bottomRightDiscord, nil, nil},
   })

   focusSome({"Keybase", "Slack", "Discord", "Messages", "Signal"})
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

local function hackOnV(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, mid80, nil, nil},
	 {browser, nil, screen, mid80, nil, nil},
	 {browser2, nil, screen, mid80, nil, nil},
	 {"Preview", nil, screen, mid80, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function readOnH(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, left40, nil, nil},
	 {browser, nil, screen, right60, nil, nil},
	 {browser2, nil, screen, right60, nil, nil},
	 {"Preview", nil, screen, right60, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function readOnV(screen)
   screen = screen or hs.screen.mainScreen()
   hs.layout.apply({
	 {"Emacs", nil, screen, top40, nil, nil},
	 {browser, nil, screen, bottom60, nil, nil},
	 {browser2, nil, screen, bottom60, nil, nil},
	 {"Preview", nil, screen, bottom60, nil, nil},
   })
   focusSome({browser, "Emacs"})
end

local function readOn()
   if (verticalp()) then
      readOnV()
   else
      readOnH()
   end
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

local function termOnV(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top10, nil, nil},
         {"Activity Monitor", nil, screen, bottom20, nil, nil},
   })
   focusSome({"Activity Monitor", "Terminal"})
end

local function slacktermOn(screen)
   hs.layout.apply({
	 {"Terminal", nil, screen, top30, nil, nil},
	 {"Slack", nil, screen, bottom70, nil, nil},
   })
end

local function chatH()
   if (twoScreens()) then
      calOn(external())
      hackOn(external())
      chatTileOn(external())
   else
      chatTileOn(laptop())
   end
end

local function chatV(screen)
   hs.layout.apply({
	 {"Keybase", nil, screen, {x=0, y=0, w=1, h=0.2}, nil, nil},
	 {"Signal", nil, screen, {x=0, y=0, w=1, h=0.2}, nil, nil},
	 {"Messages", nil, screen, {x=0, y=0.2, w=1, h=0.1}, nil, nil},
	 {"Slack", nil, screen, {x=0, y=0.3, w=1, h=0.5}, nil, nil},
	 {"Discord", nil, screen, {x=0, y=0.7, w=1, h=0.3}, nil, nil},
   })

   focusSome({"Keybase", "Discord", "Slack", "Messages", "Signal"})
end

local function chat()
   if (verticalp()) then
      chatV()
   else
      chatH()
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

local function defaultH()
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

local function defaultV()
   if (twoScreens()) then
      calOn(laptop())
      chatV(external())
      termOnV(external())
      hackOnV(external())
   else
      chatOn(laptop())
      hackOn(laptop())
   end
end

local function default()
   if (verticalp()) then
      defaultV()
   else
      defaultH()
   end
end

local function scr(vertical, horizontal, laptop)
   if (verticalp()) then
      vertical()
   elseif (twoScreens()) then
      horizontal()
   else
      laptop()
   end
end

local function restartMiddleClick()
   kill = hs.task.new("/usr/bin/pkill", nil, {"MiddleClick"})
   kill:start()
   hs.timer.doAfter(1, function ()
                       open = hs.task.new("/usr/bin/open", nil, {"/Applications/MiddleClick.app"})
                       open:start()
   end)
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

-- hs.hotkey.bind(hyper, "1", twoDimmer)
-- hs.hotkey.bind(hyper, "2", twoBrighter)
hs.hotkey.bind(hyper, "-", twoQuieter)
hs.hotkey.bind(hyper, "=", twoLouder)
hs.hotkey.bind(hyper, "0", twoDefaultVolume)
hs.hotkey.bind(hyper, ".", hs.reload)
hs.hotkey.bind(hyper, "m", restartMiddleClick)
