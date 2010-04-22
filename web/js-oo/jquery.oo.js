/*
 * Copyright (c) 2010 Mo Chen <withinsea@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * jQuery OO
 *
 * Depends:
 *	jquery.js 1.3+ (optional)
 */
if (window.jQuery ? !jQuery.claz : !window.oo) (function ($) {

    var extend = $.extend || function () {
        for (var i = 1; i < arguments.length; i++)
            for (var name in arguments[i])
                if (name != 'prototype')
                    arguments[0][name] = arguments[i][name];
        return arguments[0];
    };

    var DEFAULT_OPTS = {
        fakeConstructorProperty: false
    };

    extend($, {

        clazSetup: function (opts) {
            if (!opts) {
                return DEFAULT_OPTS;
            } else {
                extend(DEFAULT_OPTS, opts);
                return this;
            }
        },

        claz: function (constructor, overrides, opts) {
            return $.extendClaz(Object, constructor, overrides, opts);
        },

        extendClaz: function (base, constructor, overrides, opts) { // extendClaz(type, .. || extendClaz(prototype, ..

            opts = extend({}, ((constructor instanceof Function) ? opts : overrides) || {}, DEFAULT_OPTS);
            overrides = (constructor instanceof Function ? overrides : constructor) || {};
            constructor = (constructor instanceof Function) ? constructor :
                          (base instanceof Function) ? function () {
                              var ret = base.apply(this, arguments);
                              return (ret == undefined) ? this : ret;
                          } : function () {
                          };

            var standin = function () {
            };
            standin.prototype = (base instanceof Function) ? base.prototype : base;
            var claz = constructor;
            claz.prototype = extend(new standin(), overrides);

            if (opts.fakeConstructorProperty) {
                claz.prototype.constructor = claz;
            }

            return claz;
        },

        neo: function (claz) { // neo(type, [arg1, [arg2...]]) || neo(prototype, [overrides1, [overrides2...])
            var standin = function () {
            };
            standin.prototype = (claz instanceof Function) ? claz.prototype : claz;
            var obj = new standin();
            if (claz instanceof Function) {
                var ret = claz.apply(obj, Array.prototype.slice.apply(arguments, [1]));
                return (ret == undefined) ? obj : ret;
            } else {
                for (var i = 1; i < arguments.length; i++) {
                    extend(obj, arguments[i]);
                }
                return obj;
            }
        },

        proto: function (claz, prototype) {
            if (prototype === undefined) {
                return claz.prototype;
            } else {
                claz.prototype = prototype;
                return claz;
            }
        },

        propNames: function (obj) {
            var names = [];
            for (var name in obj) {
                names.push(name);
            }
            return names;
        },

        prop: function (obj, name, value) {
            if (typeof name == 'string') {
                if (value === undefined) {
                    return obj[name];
                } else {
                    obj[name] = value;
                    return obj;
                }
            } else {
                return extend(obj, name || {});
            }
        },

        deleteProp: function (obj) {    // deleteProp(obj, [propname1, [propname2...]]);
            for (var i = 1; i < arguments.length; i++) {
                delete obj[arguments[i]];
            }
            return obj;
        }

    });

})(window.jQuery || (window.oo = {}));

// Component (require jquery.js)
if (window.jQuery && !jQuery.ex) (function ($) {

    $.fn.extend({

        as: function (type) {
            return type(as);
        },

        type: function (type) {
            if (!type) {
                return this.data('type') || $;
            } else if (type == $) {
                this.removeData('type');
                return this.as($);
            } else {
                this.data('type', type);
                return this.as(type);
            }
        },

        instof: function (type) {
            return (this instanceof type);
        }

    });

    var _init = $.fn.init;

    $.extend({

        init: function () {
            var ret = _init.apply(this, arguments);
            var type = ret.type();
            return ret.instof(type) ? ret : ret.as(type);
        },

        ex: function (fn, staticFn) {

            staticFn = staticFn || {};
            fn = fn || {};

            var $base = this;
            var standin = function () {
            };
            standin.prototype = $base.prototype;
            var extype = function () {
            };
            extype.prototype = new standin();

            var $ex = $.extend(function () {
                return _init.apply(new extype(), arguments);
            }, $base, {
                prototype: extype.prototype,
                fn: extype.prototype
            });

            $ex.extend(staticFn);
            $ex.fn.extend(fn);

            return $ex;
        }

    });

})(window.jQuery);
