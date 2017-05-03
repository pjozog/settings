// Call external editor
IPython.keyboard_manager.command_shortcuts.add_shortcut('e', {
    handler : function (event) {
        var input = IPython.notebook.get_selected_cell().get_text();
        var cmd = "f = open('.jupyter_editor.py', 'w');f.close()";
        if (input != "") {
            cmd = '%%writefile .jupyter_editor.py\n' + input;
        }
        IPython.notebook.kernel.execute(cmd);
        cmd = "import os;os.system('emacsclient -c .jupyter_editor.py')";
        IPython.notebook.kernel.execute(cmd);
        return false;
    }}
);

// Reload contents into current cell
IPython.keyboard_manager.command_shortcuts.add_shortcut('g', {
    handler : function (event) {
        function handle_output(msg) {
            var ret = msg.content.text;
            IPython.notebook.get_selected_cell().set_text(ret);
        }
        var callback = {'output': handle_output};
        var cmd = "f = open('.jupyter_editor.py', 'r');print(f.read())";
        IPython.notebook.kernel.execute(cmd, {iopub: callback}, {silent: false});
        return false;
    }}
);
