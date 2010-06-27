(function() {


// avoid evaluate this script twice in same window.
if (document.getElementById('markit-dialog')) {
    return;
}

var markit_script, MARKIT_ROOT, MARKIT_KEY, JQUERY_URL, JQUERY_UI_URL, CKEDITOR_URL,
    load_by_script_tag;

markit_script = document.getElementById('markit-script');

if (markit_script) {    // by <script src=...>
    load_by_script_tag = true;

    MARKIT_ROOT = markit_script.getAttribute('r');
    MARKIT_KEY = markit_script.getAttribute('k');
} else {                // by XMLHttpRequest
    load_by_script_tag = false;

    MARKIT_ROOT = '#r#';
    MARKIT_KEY = '#k#';
}

JQUERY_URL = MARKIT_ROOT + "js/jquery-1.4.2.js";
JQUERY_UI_URL = MARKIT_ROOT + "js/jquery-ui-1.8.2.custom.js";
CKEDITOR_URL = MARKIT_ROOT + "ckeditor/ckeditor_source.js";


function load_script(url, onload) {
    var s = document.createElement('script');
    s.setAttribute('src', url);
    s.setAttribute('charset', 'UTF-8');
    document.getElementsByTagName('body')[0].appendChild(s);
    s.ontimeout = s.onerror = function() {
        this.parentNode.removeChild(this);
        alert("Can't load " + url);
    }
    s.onload = function() {
        this.parentNode.removeChild(this);
        onload();
    }
}

function main() {
    if (! jQuery) {
        return;
    }

    var $ = jQuery;

    var dialog = $('#markit-dialog');

    if (dialog.length == 0) {
        var dialog_html = '##DIALOG_HTML##';
        dialog = $(dialog_html).appendTo('body').draggable();

        dialog.find("#markit-info").text("Load by " + (load_by_script_tag ? "script tag" : "XMLHttpRequest"));

        dialog.find("#btn_mark").click(function(e) {
            dialog.data("scrollTop", $(window).scrollTop());
            dialog.data("scrollLeft", $(window).scrollLeft());
        });

        dialog.find("#btn_scroll").click(function(e) {
            var top = dialog.data("scrollTop");
            var left = dialog.data("scrollLeft");

            if (typeof(top) == 'undefined' || typeof(left) == 'undefined') {
                alert("No mark has been set!");
            } else {
                window.scrollTo(left, top);
            }
        });

        dialog.find("#btn_favor").click(function(e) {
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

