(function() {


// avoid evaluate this script twice in same window.
if (document.getElementById('markit-dialog')) {
    return;
}


////////////////////////   FUNCTIONS    ////////////////////////////////
function load_script(url, onload) {
    var s = document.createElement('script');
    s.setAttribute('src', url);
    s.setAttribute('charset', 'UTF-8');

    s.ontimeout = s.onerror = function() {
        s.ontimeout = s.onerror = null;
        this.parentNode.removeChild(this);
        alert("Can't load " + url);
    }

    var done = false;
    // IE 6 requires onreadystatechange
    s.onreadystatechange = s.onload = function() {
        if (! done && (!this.readyState ||
                    this.readyState === "loaded" || this.readyState === "complete")) {
            done = true;
            s.onreadystatechange = s.onload = null;
            this.parentNode.removeChild(this);
            onload();
        }
    }

    document.getElementsByTagName('body')[0].appendChild(s);
}


function show_version_info(dialog) {
    dialog.find("#markit-info").text("[Load by " + (load_by_script_tag ? "script tag]" : "XMLHttpRequest]"));
    dialog.find("#markit-version").text(LOADER_VERSION);
    if (LOADER_VERSION != LATEST_LOADER_VERSION) {
        dialog.find("#markit-version2").text("(NEW: v" + LATEST_LOADER_VERSION + ")");
    }
}


function add_mark(table, coord, text) {
    table.append('<tr><td><a href="javascript:window.scrollTo(' +
        coord + ')">(' + coord + ')</a></td>' +
        '<td><input type="text" size="20" value="' + text + '"/></td>' +
        '<td><a href="#">delete</a></td></tr>');
}


function on_mark(dialog) {
    var marks_table = dialog.find("#markit-marks"),
        left = $(window).scrollLeft(),
        top = $(window).scrollTop(),
        coord = left + ',' + top;

    add_mark(marks_table, coord, "a mark");
}


function on_save(dialog) {
    var args = [];
    var offset = dialog.offset();
    args.push("left", offset.left);
    args.push("top", offset.top);
    args.push("key", MARKIT_KEY);
    args.push("url", location.href);
    args.push("title", $("title").text());

    dialog.find("#markit-marks tr").each(function(i, tr) {
        var tds = $(tr).children();
        args.push("mark", $(tds[0]).text() + $(tds[1]).children().val());
    });

    var data = encode_request(args);
    $.post(MARKIT_ROOT + "mark/add", data, function(data, textStatus, xhr) {
        alert("Success!");
    });
}


function encode_request(args) {
    var data = [];

    for (var i = 0; i < args.length; i += 2) {
        data[data.length] = encodeURIComponent(args[i]) + "=" + encodeURIComponent(args[i+1]);
    }

    return data.join("&").replace(/%20/g, "+");
}


function initialize() {
    var dialog_html = '##DIALOG_HTML##';
    dialog_html = dialog_html.replace(/##MARKIT_ROOT##/, MARKIT_ROOT);

    var dialog = $(dialog_html).appendTo('body').draggable();

    show_version_info(dialog);

    dialog.find("#markit-btn_mark").click(function(e) {
        on_mark(dialog);
    });

    dialog.find("#markit-btn_save").click(function(e) {
        on_save(dialog);
    });

    $("#markit-marks a[href='#']").live("click", function() {
        var tr = this.parentNode.parentNode;
        tr.parentNode.removeChild(tr);
        return false;
    });

    $("#markit-marks").sortable({items: 'tr'});

    $.get(MARKIT_ROOT + "mark/view", {key: MARKIT_KEY, url: location.href},
            function(data, textStatus, xhr) {
                if (! data || data.length < 3)
                    return;

                // add marks
                var marks_table = dialog.find("#markit-marks");

                var marks = data[2].split("\n");
                for (var i = 0; i < marks.length; ++i) {
                    var matches = marks[i].match(/^\s*\((\d+\s*,\s*\d+)\)(.*)$/);
                    if (matches && matches.length == 3) {
                        add_mark(marks_table, matches[1], matches[2]);
                    }
                }

                // restore position
                var left = data[0], top = data[1];
                var window_height = $(window).height();
                var window_width = $(window).width();
                var dialog_height = dialog.height();
                var dialog_width = dialog.width();

                //alert("dialog: " + left +  " " + top + " " + dialog_width + " " + dialog_height + "\n" +
                //      "window: " + window_width + " " + window_height);

                if (left + dialog_width > window_width)
                    left = window_width - dialog_width;
                if (left < 0)
                    left = 0;

                if (top + dialog_height > window_height)
                    top = window_height - dialog_height;
                if (top < 0)
                    top = 0;

                //alert("dialog new: " + left + " " + top);

                dialog.offset({left: left, top: top});

            }, "json");
}


function main() {
    if (! jQuery) {
        return;
    }

    var $ = jQuery;

    var dialog = $('#markit-dialog');

    if (dialog.length == 0) {
        initialize();

        // must be after markit-dialog is created!
        if (markit_script) {
            $(markit_script).remove();
            markit_script = null;
        }

    } else {
        dialog.toggle();
    }
}


////////////////////////     CODE       ////////////////////////////////

var MARKIT_ROOT, MARKIT_KEY, JQUERY_URL, JQUERY_UI_URL, CKEDITOR_URL,
    LATEST_LOADER_VERSION, LOADER_VERSION,
    markit_script, load_by_script_tag;

LATEST_LOADER_VERSION = ##LOADER_VERSION##;
markit_script = document.getElementById('markit-script');

if (markit_script) {    // by <script src=...>
    load_by_script_tag = true;

    MARKIT_ROOT = markit_script.getAttribute('r');
    MARKIT_KEY = markit_script.getAttribute('k');
    LOADER_VERSION = markit_script.getAttribute('v');
} else {                // by XMLHttpRequest
    load_by_script_tag = false;

    MARKIT_ROOT = '#r#';
    MARKIT_KEY = '#k#';
    LOADER_VERSION = '#v#';
}

JQUERY_URL = MARKIT_ROOT + "js/jquery-1.4.2.min.js";
JQUERY_UI_URL = MARKIT_ROOT + "js/jquery-ui-1.8.2.custom.min.js";
CKEDITOR_URL = MARKIT_ROOT + "ckeditor/ckeditor.js";


if (typeof(jQuery) == 'undefined') {
    load_script(JQUERY_URL,
        function() {
            jQuery.noConflict();
            load_script(JQUERY_UI_URL, main);
        });
} else if (! (jQuery.ui && jQuery.ui.version)) {
    load_script(JQUERY_UI_URL, main);
} else {
    main();
}


})()

