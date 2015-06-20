/*
 * Resource class
 */
var Loader = function(){
    var self = this;

    self.ErrorReporter = function(xhr, status, error, task, i){
        window.error("Error loading",task.failed.url,":",status,error);
        self.log("Error loading",task.failed.url,":",status,error);
    }

    self.callIfDone = function(task){
        var done = true;
        $.each(task.urls, function(i, url){
            if(task.data[i] === null){
                done = false;
            }
        });
        if(done){
            task.success.apply(null, task.data);
        }
    }

    self.schedule = function(task, url){
        self.log("Scheduling", url.url);
        var i = task.urls.push(url)-1;
        task.data[i] = null;
        return jQuery.ajax(url).success(function(data){
            if(!task.failed){
                task.data[i] = data;
                self.log("Completed loading",url.url);
                self.callIfDone(task);
            }
        }).fail(function(xhr, status, error){
            task.failed = url;
            task.failure(xhr, status, error, task, i);
        });
    }

    self.makeTask = function(success, failure){
        return {
            "success": success,
            "failure": failure || self.ErrorReporter,
            "urls": [],
            "data": [],
            "failed": false
        };
    }

    self.load = function(urls, success, failure){
        var task = self.makeTask(success, failure);

        $.each(urls, function(i, url){
            self.schedule(task, url);
        });
        return task;
    }

    self.loadSequentially = function(urls, success, failure){
        var task = self.makeTask(success, failure);
        $.each(urls, function(i, url){
            if(0==i)return;
            var prevTask = task;
            task = self.makeTask(function(data){
                self.schedule(prevTask, url);
            });
        });
        self.schedule(task,urls[0]);
        return task;
    }
}

Loader.prototype.log = function(message){
    var args = $.extend([], arguments);
    args.unshift("[Loader]");
    console.log.apply(console, args);
    return true;
}
