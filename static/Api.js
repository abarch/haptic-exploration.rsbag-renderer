/*
 * Global funcs
 */
var error = function(){
    var args = Array.prototype.slice.call(arguments).map(function(arg){
        if(typeof arg === 'object'){
            return JSON.stringify(arg);
        }else{
            return arg;
        }
    });
    var message = args.join(" ");
    $("main>.error").removeClass("hidden")
        .append("<span>"+message+"</span>");
    return message;
}

/*
 * API class
 */
// Prevent jQuery from composing multiple same-named parameters to
// a []-suffixed name automatically.
jQuery.ajaxSettings.traditional = true;

var Api = function(){
    var self = this;
    
    var params = self.getUrlParams();

    self.params = function(name){return params[name];}
    self.param = function(name){return self.params(name)[0];}

    var defineApiFunc = function(apipoint, funcargs, transform){
        apipoint = apipoint
            || null;
        funcargs = funcargs
            || [];
        transform = transform
            || function(){this.callback(this.data);}

        if(apipoint == null) throw "No apipoint given to define!";
        // Construct closure
        var func = function(args, callback){
            self.log("Calling",apipoint,"with",args);
            $.ajax({
                dataType: "json",
                url: "/api/"+apipoint,
                data: args,
                success: function(values){transform.call({
                    status: values.status,
                    data: values.data,
                    message: values.message,
                    callback: callback
                });},
                error: function(jqr, status, error){
                    window.error("Error on API request",apipoint,"with",args,":",status,error);
                }
            });
            return null;
        }
        // Register
        var parts = apipoint.split("/");
        var path = parts.slice(0,-1);
        var location = self;
        $.each(path, function(i, part){
            if(location[part] === undefined) location[part]={};
            location = location[part];
        });
        location[parts[parts.length-1]] = func;
        return func;
    }

    defineApiFunc("shutdown", []);
    defineApiFunc("transform", ["transform"]);
    defineApiFunc("transform/list", []);
    defineApiFunc("transform/load", ["path", "source", "name"]);
    defineApiFunc("transform/remove", ["transform"]);
    defineApiFunc("transform/update", ["transform", "source"]);
    defineApiFunc("visualizer", ["visualizer"]);
    defineApiFunc("visualizer/list", []);
    defineApiFunc("visualizer/load", ["path", "source", "name"]);
    defineApiFunc("visualizer/remove", ["visualizer"]);
    defineApiFunc("visualizer/update", ["visualizer", "source"]);
    defineApiFunc("source", ["source"]);
    defineApiFunc("source/list", []);
    defineApiFunc("source/add/file", ["path", "name"]);
    defineApiFunc("source/remove", ["source"]);
    defineApiFunc("source/channel", ["source", "channel"]);
    defineApiFunc("source/channel/list", ["source"]);
    defineApiFunc("source/channel/event", ["source", "channel", "transform",
                                           "skip", "amount", "from", "to"]);

    self.streamSource = function(apiargs, deposit){
        apiargs.skip = apiargs.skip || 0;
        apiargs.amount = apiargs.amount || 100;
        self.source.channel.event(apiargs, function(data){
            if(0 < data.length){
                // Unpack
                $.each(data,function(i,item){
                    data[i]=item.data;
                });
                deposit(data);
                // Relaunch
                apiargs.skip += apiargs.amount;
                self.streamSource(apiargs, deposit);
            }
        });
    }
}

Api.prototype.getUrlParams = function(url){
    url = url || document.location.href;
    var re = /(?:\?|&(?:amp;)?)([^=&#]+)(?:=?([^&#]*))/g;
    var decode = function(s){return decodeURIComponent(s.replace(/\+/g, " "));};
    var params = {};
    
    while(match = re.exec(url)){
        var key = decode(match[1]);
        var val = decode(match[2]);
        if(typeof params[key] == 'undefined') params[key] = [val];
        else                                  params[key].push(val)
    }
    return params;
}

Api.prototype.log = function(message){
    var args = $.extend([], arguments);
    args.unshift("[Api]");
    console.log.apply(console, args);
    return true;
}
