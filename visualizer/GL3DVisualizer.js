var GL3DVisualizer = function(html, FOV, NEAR, FAR){
    var self = this;
    Visualizer.call(self, html);
    
    self.FOV = FOV || 25;
    self.NEAR = NEAR || 0.1;
    self.FAR = 1000000;

    self.canvas = $("canvas", html);
    self.initialized = false;
    self.objects = {};
    self.keys = {};
    self.scene = new THREE.Scene();
    self.camera = new THREE.PerspectiveCamera(FOV, 1, NEAR, FAR);
    self.camera.up.set(0, 0, 1);
    self.glrenderer = new THREE.WebGLRenderer({canvas: self.canvas[0], alpha: true, antialias: true});
    self.controls = new THREE.OrbitControls(self.camera, self.canvas[0]);

    self.key = function(key, set){
        switch(arguments.length){
        case 1:
            return self.keys[key]; break;
        case 2:
            if(set) self.keys[key]=key; else delete self.keys[key]; break;
        default:
            throw new Error("Illegal argument count");
        }
    }
    self.addToScene = function(object, name){
        self.objects[name] = object;
        self.scene.add(object);
        return self.objects;
    }

    self.clearScene = function(){
        $.each(self.objects, function(k, v) {
            self.scene.remove(v);
            delete self.objects[k];
        });
        return self.objects;
    }

    self.populateScene = function(points){
        $.each(points, function(k, v){
            self.addToScene(self.makePoint(v["X"], v["Y"], v["Z"], v["size"], v["color"]), k);
        });
        return self.objects;
    }

    self.updateScene = function(points){
        $.each(points, function(k, v){
            self.objects[k].position.set(v["X"], v["Y"], v["Z"]);
        });
        return self.objects;
    }

    self.updateCamera = function(){
        self.controls.update();

        var look = new THREE.Vector3(0, 0, -1).applyQuaternion(self.camera.quaternion);
        if(self.key(87) || self.key(38)){ // W / <up>
            self.camera.position.add(look.clone().setLength(0.03));
        }
        if(self.key(83) || self.key(40)){ // S / <down>
            self.camera.position.sub(look.clone().setLength(0.03));
        }
        if(self.key(65) || self.key (37)){ // A / <left>
            self.controls.panLeft(0.01);
        }
        if(self.key(68) || self.key (39)){ // D / <right>
            self.controls.panLeft(-0.01);
        }
        if(self.key(32)){
            if(key(16))
                self.controls.panUp(-0.01);
            else
                self.controls.panUp(0.01);
        }
        return self.camera;
    }

    self.render = function(){
        self.updateCamera();
        self.glrenderer.render(self.scene, self.camera);
        requestAnimationFrame(self.render);
        return true;
    }

    self.initUI = function(){
        // Resize handling
        $(window).resize(function(){
            width = self.canvas.parent().innerWidth();
            height = self.canvas.parent().innerHeight()-self.canvas.position().top;
            
            self.camera.aspect = width/height;
            self.camera.left = width/-2;
            self.camera.right = width/2;
            self.camera.top = height/2;
            self.camera.bottom = height/-2;
            self.camera.updateProjectionMatrix();
            self.glrenderer.setSize(width, height);
        });

        self.canvas.focus();
        self.canvas.keydown(function(ev){
            self.key(ev.keyCode||ev.which, true);
        }).keyup(function(ev){
            self.key(ev.keyCode||ev.which, false);
        }).click(function(ev){
            this.focus();
        });
        
        $(window).trigger("resize");
    }

    self.initScene = function(){
        var grid = new THREE.GridHelper(1, 0.1);
        grid.geometry.applyMatrix(new THREE.Matrix4().makeRotationX(self.toRadians(90)));
        // avoid intersecting with axis.
        grid.geometry.applyMatrix(new THREE.Matrix4().makeTranslation(0,0,-0.0001));
        self.scene.add(grid);
        var axisHelper = new THREE.AxisHelper(0.1);
        self.scene.add(axisHelper);
        
        self.camera.position.x = 1;
        self.camera.position.y = 1;
        self.camera.position.z = 1;
        self.controls.damping = 0.2;
        return self.scene;
    }

    // public interface
    self.clear = function(){
        self.initialized = false;
        self.clearScene();
        return true;
    }
    
    self.show = function(data){
        if(self.initialized){
            self.updateScene(data);
        }else{
            self.populateScene(data);
            self.initialized = true;
        }
        return data;
    }

    self.initUI();
    self.initScene();
    self.render();

    return self;
}

GL3DVisualizer.prototype = Object.create(Visualizer.prototype);
GL3DVisualizer.prototype.constructor = GL3DVisualizer;

GL3DVisualizer.prototype.toRadians = function(deg){
    return deg*(Math.PI/180);
}

GL3DVisualizer.prototype.toDegrees = function(rad){
    return rad*(180/Math.PI);
}

GL3DVisualizer.prototype.makePoint = function(x, y, z, size, color){
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

GL3DVisualizer.prototype.name = "GL3DVisualizer";

return GL3DVisualizer;
