(function() {


var g_window = window.top,
    g_document = g_window.document;

// avoid evaluating this script twice in same window.
if (g_document.getElementById('markit-dialog')) {
    return;
}


////////////////////////   FUNCTIONS    ////////////////////////////////
var from_json = JSON ? JSON.parse : EVAL;

function load_script(url, onload) {
    var s = g_document.createElement('script');
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

    g_document.getElementsByTagName('body')[0].appendChild(s);
}


function encode_request(args) {
    var data = [];

    for (var i = 0; i < args.length; i += 2) {
        data[data.length] = encodeURIComponent(args[i]) + "=" + encodeURIComponent(args[i+1]);
    }

    return data.join("&").replace(/%20/g, "+");
}


function XDomainRequest_request(method, url, data, success) {
    if (data && typeof(data) !== "string") {
        data = jQuery.param(data);
    }

    if (method === "GET" && data && data.length > 0) {
        url = url + "?" + data;
        data = "";
    }

    var xdr = new XDomainRequest();

    xdr.onerror = function() {
        xdr.onerror = xdr.ontimeout = xdr.onload = null;
        alert("error happened!");
    };

    xdr.ontimeout = function() {
        xdr.onerror = xdr.ontimeout = xdr.onload = null;
        alert("request timeout!");
    };

    xdr.timeout = 10000;
    xdr.onload = function() {
        xdr.onerror = xdr.ontimeout = xdr.onload = null;
        success(xdr.responseText);
    };

    xdr.open(method, url);
    xdr.send(data);
}


function initialize($) {
    ////////////////////////   FUNCTIONS  //////////////////////////////
    function ajax_post(url, data, success) {
        if (g_window.XDomainRequest) {
            try {
                XDomainRequest_request("POST", url, data, success);
                return;
            } catch (e) {
                alert(e);
            }
        }

        $.post(url, data, success);
    }


    function ajax_getJSON(url, data, success) {
        if (g_window.XDomainRequest) {
            try {
                XDomainRequest_request("GET", url, data, function(data) {
                        success(from_json(data));
                    });
                return;
            } catch (e) {
                alert(e);
            }
        }

        $.getJSON(url, data, success);
    }


    function show_version_info() {
        dialog.find("#markit-info").text("[Load by " + (load_by_script_tag ? "script tag]" : "XMLHttpRequest]"));
        dialog.find("#markit-version").text(LOADER_VERSION);
        if (LOADER_VERSION != LATEST_LOADER_VERSION) {
            dialog.find("#markit-version2").text("(NEW: v" + LATEST_LOADER_VERSION + ")");
        }
    }


    function add_mark(table, coord, text) {
        table.append('<tr><td><a href="javascript:top.scrollTo(' +
            coord + ')">(' + coord + ')</a></td>' +
            '<td><input type="text" size="20" value="' + text + '"/></td>' +
            '<td><a href="#">delete</a></td></tr>');
    }


    function on_mark() {
        var marks_table = dialog.find("#markit-marks"),
            left = $(g_window).scrollLeft(),
            top = $(g_window).scrollTop(),
            coord = left + ',' + top;

        add_mark(marks_table, coord, "a mark");
    }


    function on_save() {
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
        ajax_post(MARKIT_ROOT + "mark/add", data, function(data) {
            alert("Success!");
        });
    }


    function restore_marks(data) {
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
        var window_height = $(g_window).height();
        var window_width = $(g_window).width();
        var dialog_height = dialog.height();
        var dialog_width = dialog.width();

        //alert("dialog: " + left +  " " + top + " " + dialog_width + " " + dialog_height + "\n" +
        //      "top: " + window_width + " " + window_height);

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
    }


    ////////////////////////   CODE   //////////////////////////////////

    var dialog_html = '##DIALOG_HTML##';
    dialog_html = dialog_html.replace(/##MARKIT_ROOT##/, MARKIT_ROOT);

    var dialog = $(dialog_html).appendTo('body').draggable();

    show_version_info();

    dialog.find("#markit-btn_mark").click(on_mark);

    dialog.find("#markit-btn_save").click(on_save);

    $("#markit-marks a[href='#']").live("click", function() {
        var tr = this.parentNode.parentNode;
        tr.parentNode.removeChild(tr);
        return false;
    });

    $("#markit-marks").sortable({items: 'tr'});

    ajax_getJSON(MARKIT_ROOT + "mark/view", {key: MARKIT_KEY, url: location.href},
            restore_marks);
}


function main() {
    if (! jQuery) {
        return;
    }

    var $ = jQuery;
    var dialog = $('#markit-dialog');

    if (dialog.length == 0) {
        initialize($);

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
markit_script = g_document.getElementById('markit-script');

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


if (typeof(jQuery) === 'undefined') {
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

