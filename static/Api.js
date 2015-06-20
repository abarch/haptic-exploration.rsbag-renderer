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
var Api = function(){
    var self = this;
    
    var params = getUrlParams();

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
            $.ajax({
                dataType: "json",
                url: apipoint,
                data: args,
                success: function(values){transform.call({
                    status: values.status,
                    data: values.data,
                    message: values.message,
                    callback: callback
                });}
            });
            return null;
        }
        // Register
        var parts = apipoint.split("/");
        var path = parts.slice(0,-1);
        var location = self;
        $.each(path, function(i, part){location = location[part];});
        location[parts[parts.length-1]] = func;
        return func;
    }

    defineApiFunc("shutdown", []);
    defineApiFunc("transform", ["transform"]);
    defineApiFunc("transform/list", []);
    defineApiFunc("transform/load", ["path", "source", "name"]);
    defineApiFunc("transform/remove", ["transform"]);
    defineApiFunc("transform/update", ["transform", "source"]);
    defineApiFunc("visualization", ["visualization"]);
    defineApiFunc("visualization/list", []);
    defineApiFunc("visualization/load", ["path", "source", "name"]);
    defineApiFunc("visualization/remove", ["visualization"]);
    defineApiFunc("visualization/update", ["visualization", "source"]);
    defineApiFunc("source", ["source"]);
    defineApiFunc("source/list", []);
    defineApiFunc("source/add/file", ["path", "name"]);
    defineApiFunc("source/remove", ["source"]);
    defineApiFunc("source/channel", ["source" "channel"]);
    defineApiFunc("source/channel/list", ["source"]);
    defineApiFunc("source/channel/event", ["source", "channel", "transform",
                                           "skip", "amount", "from", "to"]);

    self.streamSource = function(apiargs, deposit){
        
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
