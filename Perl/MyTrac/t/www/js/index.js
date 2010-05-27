if (typeof(window.log) == "undefined") {
    window.log = {
        toggle: function() {},
        move: function() {},
        resize: function() {},
        clear: function() {},
        debug: function() {},
        info: function() {},
        warn: function() {},
        error: function() {},
        profile: function() {}
    };
}

$(document).ready(function() {
    log.debug("Ready to go!");
});

