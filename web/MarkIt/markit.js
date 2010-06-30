(function() {


// avoid evaluate this script twice in same window.
if (document.getElementById('markit-dialog')) {
    return;
}

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

function main() {
    if (! jQuery) {
        return;
    }

    var $ = jQuery;

    var dialog = $('#markit-dialog');

    if (dialog.length == 0) {
        var dialog_html = '##DIALOG_HTML##';
        dialog_html = dialog_html.replace(/##MARKIT_ROOT##/, MARKIT_ROOT);
        dialog = $(dialog_html).appendTo('body').draggable();

        dialog.find("#markit-info").text("[Load by " + (load_by_script_tag ? "script tag]" : "XMLHttpRequest]"));
        dialog.find("#markit-version").text(LOADER_VERSION);
        if (LOADER_VERSION != LATEST_LOADER_VERSION) {
            dialog.find("#markit-version2").text("(NEW: v" + LATEST_LOADER_VERSION + ")");
        }

        dialog.find("#markit-btn_mark").click(function(e) {
            var marks_table = dialog.find("#markit-marks"),
                left = $(window).scrollLeft(),
                top = $(window).scrollTop(),
                coord = left + ',' + top;

            marks_table.append('<tr><td><a href="javascript:window.scrollTo(' +
                coord + ')">(' + coord + ')</a></td>' +
                '<td><input type="text" size="20" value="a mark"/></td>' +
                '<td><a href="#">delete</a></td></tr>');
        });

        dialog.find("#markit-btn_save").click(function(e) {
            var title, url;
            title = $("title").text();
            url = location.href;

            if ($.browser.msie) {
                window.external.addFavorite(url, title);
            } else if ($.browser.mozilla) {
                window.sidebar.addPanel(title, url, "");
            } else {
                alert("This browser isn't supported!");
            }
        });

        $("#markit-marks a[href=#]").live("click", function() {
            var tr = this.parentNode.parentNode;
            tr.parentNode.removeChild(tr);
            return false;
        });

        $("#markit-marks").sortable({items: 'tr'});

        // must be after markit-dialog is created!
        if (markit_script) {
            $(markit_script).remove();
            markit_script = null;
        }

    } else {
        dialog.toggle();
    }
}

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

