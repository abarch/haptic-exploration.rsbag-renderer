$(function(){
    var FOV = 25;
    var NEAR = 0.1;
    var FAR = 1000000;

    var frame = 0;
    var frames = [];
    var framesLoaded = 0;
    var renderDuration = 0;
    var resolution = parseInt($("#player").data("resolution"));
    var canvas = $("canvas");
    var player = $("#player");
    var objects = {};
    var keys = {};
    var scene = new THREE.Scene();
    var camera = new THREE.PerspectiveCamera(FOV, 1, NEAR, FAR);
    //THREE.OrthographicCamera(600/-2, 600/2, 400/2, 400/-2, NEAR, FAR);
    camera.up.set(0, 0, 1);
    var renderer = new THREE.WebGLRenderer({canvas: canvas[0], alpha: true, antialias: true});
    var controls = new THREE.OrbitControls(camera, canvas[0]);

    function log(message){
        var args = $.extend([], arguments);
        args.unshift("[Renderer]");
        console.log.apply(console, args);
        return null;
    }

    function toRadians(deg){
        return deg*(Math.PI/180);
    }

    function toDegrees(rad){
        return rad*(180/Math.PI);
    }

    function allocateArray(length){
        var array = [];
        for(var i=0; i<length; i++){
            array[i]=null;
        }
        return array;
    }

    // Set up according to data
    data = allocateArray(parseInt($("#player").data("frames")));

    // Resize handling
    $(window).resize(function(){
        width = canvas.parent().innerWidth();
        height = canvas.parent().innerHeight()-canvas.position().top;
        
        camera.aspect = width/height;
        camera.left = width/-2;
        camera.right = width/2;
        camera.top = height/2;
        camera.bottom = height/-2;
        camera.updateProjectionMatrix();
        renderer.setSize(width, height);
    });

    // Key tracking
    function key(key,set){
        switch(arguments.length){
        case 1:
            return keys[key]; break;
        case 2:
            if(set) keys[key]=key; else delete keys[key]; break;
        default:
            throw new Error("Illegal argument count");
        }
    }

    canvas.focus();
    canvas.keydown(function(ev){
        key(ev.keyCode||ev.which, true);
    }).keyup(function(ev){
        key(ev.keyCode||ev.which, false);
    }).click(function(ev){
        this.focus();
    });

    function makePoint(x, y, z, size, color){
        x = x || 0;
        y = y || 0;
        z = z || 0;
        size = size || 0.0075;
        color = color || 0xFF0000;

        var geometry = new THREE.BoxGeometry(size, size, size);
        var material = new THREE.MeshBasicMaterial({color: color});
        var mesh = new THREE.Mesh(geometry, material);
        mesh.position.set(x, y, z);
        return mesh;
    }

    function addToScene(object, name){
        objects[name] = object;
        scene.add(object);
        return object;
    }

    function clearScene(){
        $.each(objects, function(k, v) {
            scene.remove(v);
            delete objects[k];
        });
        return objects;
    }

    function populateScene(points){
        $.each(points, function(k, v){
            addToScene(makePoint(v["X"], v["Y"], v["Z"], v["size"], v["color"]), k);
        });
    }

    function updateScene(points){
        $.each(points, function(k, v){
            objects[k].position.set(v["X"], v["Y"], v["Z"]);
        });
    }

    // Scene setup
    function init(){
        var grid = new THREE.GridHelper(1, 0.1);
        grid.geometry.applyMatrix(new THREE.Matrix4().makeRotationX(toRadians(90)));
        // avoid intersecting with axis.
        grid.geometry.applyMatrix(new THREE.Matrix4().makeTranslation(0,0,-0.0001));
        scene.add(grid);
        var axisHelper = new THREE.AxisHelper(0.1);
        scene.add(axisHelper);
        
        camera.position.x = 1;
        camera.position.y = 1;
        camera.position.z = 1;
        controls.damping = 0.2;
        animate();
        return scene;
    }

    // Main loop
    function updateCamera(){
        controls.update();

        var look = new THREE.Vector3(0, 0, -1).applyQuaternion(camera.quaternion);
        if(key(87) || key(38)){ // W / <up>
            camera.position.add(look.clone().setLength(0.03));
        }
        if(key(83) || key(40)){ // S / <down>
            camera.position.sub(look.clone().setLength(0.03));
        }
        if(key(65) || key (37)){ // A / <left>
            controls.panLeft(0.01);
        }
        if(key(68) || key (39)){ // D / <right>
            controls.panLeft(-0.01);
        }
        if(key(32)){
            if(key(16))
                controls.panUp(-0.01);
            else
                controls.panUp(0.01);
        }
        return camera;
    }
    
    function animate(){
        updateCamera();

        if(isPlaying() && frame<framesLoaded-1){
            // Calculate next frame by approximating time distance
            // through previous time taken to render the frame.
            var frameDistance = (renderDuration/1000)*resolution;
            var newFrame = frame+Math.floor(frameDistance);
            if(newFrame < framesLoaded){
                setFrame(newFrame);
            }else{
                setFrame(framesLoaded-1);
                setPlaying(false);
            }
        }
        
        return scene;
    }

    var lastFrame = 0;
    function render(){
        var now = performance.now();
        if(0 < lastFrame)
            renderDuration = now-lastFrame;
        lastFrame = now;
        renderer.render(animate(), camera);
        requestAnimationFrame(render);
        return null;
    };

    // Init
    $(window).trigger("resize");
    init();
    render();

    function updateDisplay(){
        var pad = function(thing, count){
            thing = ""+thing;
            count = (count || 2) - thing.length;
            return (count<0)? thing : new Array(count+1).join("0")+thing;
        }
        var displayUnit = $("#seeker", player).width()/frames.length;
        $("#seeker #loaded", player).width(framesLoaded*displayUnit+"px");
        $("#seeker #current", player).width(frame*displayUnit+"px");
        var seconds = frame/resolution;
        var msecs = frame%resolution/resolution*1000;
        var secs = Math.floor(seconds) % 60;
        var mins = Math.floor(seconds / 60);
        $("#frameinfo", player).text(pad(mins)+":"+pad(secs)+":"+pad(msecs,3));
    }

    function setFrame(newframe){
        data = frames[newframe];
        if(data){
            frame = newframe;
            updateScene(data);
            updateDisplay();
        }
        return frame;
    }

    function setFrameData(data){
        frames = data;
        framesLoaded = data.length;
        setFrame(0);
        return frames;
    }

    function addFrameData(data){
        for(var i=0; i<data.length; i++){
            frames[i+framesLoaded] = data[i];
        }
        if(framesLoaded == 0 && data.length>0){
            frame = 0;
            populateScene(frames[frame])
        }
        framesLoaded += data.length;
        updateDisplay();
        return frames;
    }

    function setPlaying(val){
        if(val){
            $("#play i", player).removeClass("icon-play")
                .addClass("icon-pause");
            if(frame == frames.length-1){
                setFrame(0);
            }
        }else{
            $("#play i", player).addClass("icon-play")
                .removeClass("icon-pause");
        }
    }

    function isPlaying(){
        return $("#play i", player).hasClass("icon-pause");
    }

    $("#play", player).click(function(){
        setPlaying(!isPlaying());
    });

    var wasplaying = false;
    var isholding = false;
    $("#seeker", player).mousedown(function(ev){
        isholding = true;
        wasplaying = isPlaying();
        setPlaying(false);
        var x = ev.pageX - $(this).offset().left;
        setFrame(Math.floor(x/$(this).width()*frames.length));
    });
    $(window).mousemove(function(ev){
        if(isholding){
            var seeker = $("#seeker", player);
            var x = ev.pageX - seeker.offset().left;
            setFrame(Math.floor(x/seeker.width()*frames.length));
        }
    }).mouseup(function(ev){
        if(isholding){
            isholding = false;   
            setPlaying(wasplaying);
        }
    });

    // Load proper frame data
    log("Loading testdata...");
    jQuery.getJSON("testdata.json", function(data){
        addFrameData(data);
        log("Done loading.");
    });
})
