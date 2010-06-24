(function() {
    var scripts = document.getElementsByTagName('script');
    var url='##MARKIT_URL##';

    for (var i = 0; i < scripts.length; ++i) {
        var src = scripts[i].getAttribute('src');
        if (src === url) {
            var dialog = document.getElementById('markit-dialog');
            if (dialog) {
                $(dialog).toggle();
            } else {
                alert('Loading MarkIt dialog, please try again later.');
            }
            return;
        }
    }

    var s = document.createElement('script');
    s.setAttribute('src', url);
    document.getElementsByTagName('body')[0].appendChild(s);
})()

