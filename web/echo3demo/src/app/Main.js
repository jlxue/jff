MyApp = Core.extend(Echo.Application, {

    $static: {

        /**
         * Global initialization method.  Creates/starts client/application in "rootArea" element of document.
         */
        init: function() {
            Core.Web.init();
            if (Echo.DebugConsole) {
                Echo.DebugConsole.install();
            }
            var app = new MyApp();
            var client = new Echo.FreeClient(app, document.getElementById("rootArea"));
            client.addResourcePath("Echo", "lib/echo/");
            client.addResourcePath("Extras", "lib/extras/");
            app.setStyleSheet(MyApp.StyleSheet);
            client.init();
        },

        /** Required JavaScript module URLs for "About" dialog. */
        MODULE_ABOUT: [
            "lib/extras/Application.TabPane.js",
            "lib/extras/Sync.TabPane.js",
            "app/About.js",
        ],

        /**
         * Set of available locale modules.
         */
        LOCALE_MODULES: {
            "en": true,
            "zh": true,
        },

        /**
         * Globally configured locale.
         * @type String
         */
        locale: null,

        /**
         * Retrieves resource map for current (globally configured) locale from resource bundle.
         */
        getMessages: function() {
            return MyApp.Messages.get(MyApp.locale);
        },

        /**
         * User preference information.
         */
        pref: {

            /**
             * Flag indicating whether animated transition effects are enabled.
             * @type Boolean
             */
            transitionsEnabled: true,

            /**
             * Default WindowPane style name.
             * @type String
             */
            windowStyleName: "Default",
        },
    },

    /**
     * Localized resource map.
     */
    _msg: null,

    /** @see Echo.Application#init */
    init: function() {
        this._msg = MyApp.getMessages();
        var label = new Echo.Label({
            text: this._msg["About.WindowTitle"],
        });
        this.rootComponent.add(label);
    },
});

