var PrintVisualizer = function(html){
    var self = this;
    Visualizer.call(self, html);

    var container = $("pre",html);

    // http://stackoverflow.com/a/7220510/743237
    self.syntaxHighlight = function(json) {
        if (typeof json != 'string') {
            json = JSON.stringify(json, undefined, 2);
        }
        json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
            var cls = 'number';
            if (/^"/.test(match)) {
                if (/:$/.test(match)) {
                    cls = 'key';
                } else {
                    cls = 'string';
                }
            } else if (/true|false/.test(match)) {
                cls = 'boolean';
            } else if (/null/.test(match)) {
                cls = 'null';
            }
            return '<span class="' + cls + '">' + match + '</span>';
        });
    }
    
    self.clear = function(){
        container.html("");
    }

    self.show = function(data){
        container.html(self.syntaxHighlight(data));
    }
}

PrintVisualizer.prototype = Object.create(Visualizer.prototype);
PrintVisualizer.prototype.constructor = PrintVisualizer;
PrintVisualizer.name = "PrintVisualizer";

return PrintVisualizer;
