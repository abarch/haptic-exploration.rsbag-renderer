/*
 * Visualizer superclass
 */
var Visualizer = function(html){
    var self = this;
    self.html = html;

    self.clear = function(){
        throw "Visualizer must implement the clear() method!";
    }
    
    self.show = function(data){
        throw "Visualizer must implement the show(data) method!";
    }
    
    return self;
}

/*
 * Main Renderer class
 */
var Renderer = function(player, resolution, frames){
    var self = this;
    var actualPlayer = player
        || null;
    var actualResolution = resolution
        || parseInt($(actualPlayer).data("resolution"))
        || (self.log("Warn: No resolution given or found, defaulting to",30) && 30);
    var actualFrames = frames
        || parseInt($(actualPlayer).data("frames"))
        || 0;

    if(actualPlayer == null) throw "No player given to renderer!";
    
    self.player = $(actualPlayer);
    self.resolution = actualResolution;
    self.frame = 0;
    self.frames = self.allocateArray(actualFrames);
    self.framesLoaded = 0;
    self.visualizers = {};

    self.updateDisplay = function(){
        var pad = function(thing, count){
            thing = ""+thing;
            count = (count || 2) - thing.length;
            return (count<0)? thing : new Array(count+1).join("0")+thing;
        }
        var displayUnit = $(".seeker", self.player).width()/self.frames.length;
        $(".seeker .loaded", self.player).width(self.framesLoaded*displayUnit+"px");
        $(".seeker .current", self.player).width(self.frame*displayUnit+"px");
        var seconds = self.frame / self.resolution;
        var msecs = self.frame % self.resolution / self.resolution * 1000;
        var secs = Math.floor(seconds) % 60;
        var mins = Math.floor(seconds / 60);
        var text = pad(mins)+":"+pad(secs)+":"+pad(msecs,3);
        $(".frameinfo", self.player).text(text);
        return text;
    }

    self.frameData = function(frame){
        var actframe = frame || self.frame;
        return (actframe<self.framesLoaded && 0<=actframe) ? self.frames[actframe] : null;
    }

    self.setFrame = function(newframe){
        var data = self.frameData(newframe);
        if(data && newframe != self.frame){
            self.frame = newframe;
            self.updateDisplay();

            $.each(self.visualizers, function(i, visualizer){
                visualizer.show(data);
            });
        }
        return self.frame;
    }

    self.setFrameData = function(data){
        self.frames = data;
        self.framesLoaded = data.length;
        self.setFrame(0);
        return self.frames;
    }

    self.addFrameData = function(data){
        for(var i=0; i<data.length; i++){
            self.frames[i+self.framesLoaded] = data[i];
        }
        if(self.framesLoaded == 0 && data.length>0){
            self.frame = 0;
        }
        self.framesLoaded += data.length;
        self.updateDisplay();
        return self.frames;
    }

    var playing = false;
    self.setPlaying = function(val){
        playing = val;
        if(val){
            self.log("Playing");
            $(".play i", self.player).removeClass("icon-play").addClass("icon-pause");
            if(self.frame == self.frames.length-1){
                self.setFrame(0);
            }
        }else{
            self.log("Pausing");
            $(".play i", self.player).addClass("icon-play").removeClass("icon-pause");
        }
        return playing;
    }

    self.isPlaying = function(){
        return playing;
    }

    self.seekerFrame = function(ev){
        var seeker = $(".seeker", self.player);
        var x = ev.pageX - seeker.offset().left;
        return Math.floor(x/seeker.width()*self.frames.length);
    }

    self.loadVisualizer = function(js, html, css){
        var funct = new Function(js);
        var name = funct();
        var style = $("<style type=\"text/css\" />").data("visualizer", name).html(css||"");
        var html = $($.parseHTML(html)).data("visualizer",name);
        
        $("head").append(style);
        $(".visualizations",self.player).append(html);
        self.visualizers[name] = new window[name]();
        return self.visualizers[name];
    }

    self.initUI = function(){
        $(".play", self.player).click(function(){
            self.setPlaying(!self.isPlaying());
        });
        
        var wasplaying = false;
        var isholding = false;
        $(".seeker", self.player).mousedown(function(ev){
            isholding = true;
            wasplaying = self.isPlaying();
            self.setPlaying(false);
            self.setFrame(self.seekerFrame(ev));
        });
        
        $(window).mousemove(function(ev){
            if(isholding){
                self.setFrame(self.seekerFrame(ev));
            }
        }).mouseup(function(ev){
            if(isholding){
                isholding = false;   
                self.setPlaying(wasplaying);
            }
        });
        return true;
    }

    var frameDuration = 0;
    var lastFrameTime = 0;
    self.step = function(){
        var now = performance.now();
        if(0 < lastFrameTime)
            frameDuration = now-lastFrameTime;
        lastFrameTime = now;

        if(self.isPlaying() && self.frame<self.framesLoaded-1){
            // Calculate next frame by approximating time distance
            // through previous time taken to render the frame.
            var frameDistance = (frameDuration/1000)*self.resolution;
            var newFrame = self.frame+Math.floor(frameDistance);
            if(newFrame < self.framesLoaded){
                self.setFrame(newFrame);
            }else{
                self.setFrame(self.framesLoaded-1);
                self.setPlaying(false);
            }
        }

        window.requestAnimationFrame(self.step);
        return frameDuration;
    };

    self.initUI();
    self.step();

    return self;
}

Renderer.prototype.log = function(message){
    var args = $.extend([], arguments);
    args.unshift("[Renderer]");
    console.log.apply(console, args);
    return true;
}

Renderer.prototype.allocateArray = function(length){
    var array = [];
    for(var i=0; i<length; i++){
        array[i]=null;
    }
    return array;
}

var renderer = null;

$(function(){
    renderer = new Renderer($(".player").first());

    renderer.loadVisualizer('return "GL3DVisualizer";',
                            '<canvas></canvas>',
                            '');

    // Load proper frame data
    renderer.log("Loading testdata...");
    jQuery.getJSON("testdata.json", function(data){
        renderer.addFrameData(data);
        renderer.log("Done loading.");
    });
})
