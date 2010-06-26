(function() {
    function elem(id) {
        return document.getElementById(id);
    }

    function tags(name) {
        return document.getElementsByTagName(name);
    }

    function attr(node, name, value) {
        if (value) {
            node.setAttribute(name, value)
            return node;
        } else {
            return node.getAttribute(name);
        }
    }

    function load_markit_js_by_script_tag() {
        script = document.createElement('script');
        attr(attr(attr(attr(attr(attr(attr(script,
                    'src', markit_url),
                    'j', jquery_url),
                    'u', jquery_ui_url),
                    'c', ckeditor_url),
                    'k', markit_key),
                    'charset', 'UTF-8'),
                    'id', id);
        script.ontimeout = script.onerror = function() {
            script.ontimeout = script.onerror = null;
            script.parentNode.removeChild(script);
            alert('Can\'t load ' + markit_url);
        }

        tags('body')[0].appendChild(script);
    }

    var dialog = elem('markit-dialog'),
        id = 'markit-script',
        msg = 'Loading MarkIt dialog, please try again later.',
        markit_key      = '##MARKIT_KEY',
        markit_root     = '##MARKIT_ROOT##',
        markit_url      = markit_root + '##MARKIT_URL##',
        jquery_url      = markit_root + '##JQUERY_URL##',
        jquery_ui_url   = markit_root + '##JQUERY_UI_URL##',
        ckeditor_url    = markit_root + '##CKEDITOR_URL##',
        file_protocol   = 'file:',
        script, scripts,
        xhr,
        i;

    if (dialog) {
        // markit.js must load jQuery and jQuery UI first, then create dialog
        jQuery(dialog).toggle();
        return;
    }


    if (elem(id)) {
        alert(msg);
        return;
    } else {
        scripts = tags('script');

        for (i = 0; i < scripts.length; ++i) {
            if (markit_url == attr(scripts[i], 'src')) {
                alert(msg);
                return;
            }
        }
    }

    if (! (location.href.indexOf(file_protocol) != 0 && markit_url.indexOf(file_protocol) == 0)) {
        try {
            // prefer XMLHttpRequest to script tag for privacy, as other
            // scripts in this document won't peek into our private data,
            // especially `markit_key'.
            //
            // XMLHttpRequest in IE7 can't request local files, so we
            // we use the ActiveXObject when it is available.
            if (window.XMLHttpRequest && (markit_url.indexOf(file_protocol) != 0 || !window.ActiveXObject)) {
                xhr = new XMLHttpRequest();     // Firefox, Opera, Safari
            } else {
                xhr = new ActiveXObject('Microsoft.XMLHTTP');   // IE 5.5+
            }

            xhr.onreadystatechange = function() {
                if (xhr.readyState == 4) {
                    xhr.onreadystatechange = function() {};

                    if (xhr.status == 200 || xhr.status == 0 /* file:// */) {
                        script = xhr.responseText;
                        //alert('got script: ' + script);

                        // YUI compressor refuses to optimize scripts containing eval(),
                        // so we do a trick in Makefile, where EVAL is replaced with eval.
                        EVAL(script.replace(
                                    /#j#/g, jquery_url).replace(
                                    /#u#/g, jquery_ui_url).replace(
                                    /#c#/g, ckeditor_url).replace(
                                    /#k#/g, markit_key));
                    } else {
                        load_markit_js_by_script_tag();
                    }
                }
            }

            xhr.open('GET', markit_url, true);
            xhr.send(null);
            return;
        } catch (e) {
        }
    }

    load_markit_js_by_script_tag();

})()

