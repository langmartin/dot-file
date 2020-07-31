var sl = {
    "full": "move screenOriginX;screenOriginY screenSizeX;screenSizeY",
    "sm": "0",
    "lg": "1",
    "right13": "move screenOriginX+(screenSizeX/3)*2;screenOriginY screenSizeX/3;screenSizeY"
};

var fullSm = slate.operationFromString([sl.full, sl.sm].join(" "));
var fullLg = slate.operationFromString([sl.full, sl.lg].join(" "));
var right13 = slate.operationFromString(sl.right13);

function zoom2 (win) {
    var title = win.title();

    var wop = {
        "Zoom Meeting": fullSm,
        "Zoom": fullLg,
    }[win.title()];

    var aop = {
        "Emacs": right13,
        "Safari": right13,
    }[win.app.name()];

    var op = wop || aop;
    if (op) win.doOperation(op);
}

slate.bind("z:alt,cmd,ctrl", zoom2);
