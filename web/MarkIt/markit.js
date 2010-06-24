(function() {


function load_script(url, onload) {
    var s = document.createElement('script');
    s.setAttribute('src', url);
    document.getElementsByTagName('body')[0].appendChild(s);
    s.onload = onload;
}

function load_jquery_ui(onload) {
    load_script('http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.2/jquery-ui.min.js',
        onload);
}

function main() {
    var $ = jQuery;

    var dialog = $('#markit-dialog');

    if (dialog.length == 0) {
        var dialog_html = ##DIALOG_HTML##;
        dialog = $(dialog_html).appendTo('body').draggable();
        dialog.find("#btn_mark").click(function(e) {
            dialog.data("scrollTop", $(window).scrollTop());
            dialog.data("scrollLeft", $(window).scrollLeft());
        });

        dialog.find("#btn_scroll").click(function(e) {
            var top = dialog.data("scrollTop");
            var left = dialog.data("scrollLeft");

            if (typeof(top) != "undefined" && typeof(left) != "undefined") {
                window.scrollTo(left, top);
            } else {
                alert("No position remembered!");
            }
        });
    } else {
        dialog.toggle();
    }
}

if (typeof(jQuery) == 'undefined') {
    load_script('http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js',
        function() { jQuery.noConflict(); load_jquery_ui(main); });
} else if (! (jQuery.ui && jQuery.ui.version)) {
    load_jquery_ui(main);
} else {
    main();
}


})()

